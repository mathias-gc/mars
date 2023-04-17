# mars
mars <- function(formula, data, control=mars.control()) {
  cc <- match.call() # save the call
  mf <- model.frame(formula, data)
  y <- model.response(mf)
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)
  x <- x[, -1]
  fwd <- fwd_stepwise(y, x, control)
  bwd <- bwd_stepwise(fwd, control)
  fit <- lm(formula = y ~ . - 1, data = data.frame(y = y, bwd$B))


  # View the components

  out <- list(call=cc,
              formula=formula,
              y=y,
              B=bwd[["B"]],
              Bfuncs=bwd[["Bfuncs"]],
              x_names=colnames(x))

  out <- append(out, fit)
  class(out) <- "mars"

  return(out)
}

# Constructor
new_mars.control <- function(control) {
  structure(control, class="mars.control")
}

# Validator
validate_mars.control <- function(control) {
  stopifnot(
    is.numeric(control$d),
    is.logical(control$trace),
    control$Mmax %% 2 == 0,
    control$Mmax >= 2
  )
  return(TRUE)
}

# Helper
mars.control <- function(Mmax = 2, d = 3, trace = FALSE) {
  control <- list(Mmax = Mmax, d = d, trace = trace)
  validate_mars.control(control)
  new_mars.control(control)
}

mc <- mars.control()

fwd_stepwise <- function(y, x, control){
  # Initialize:
  N <- length(y) # sample size
  n <- ncol(x) # number of predictors = number of X
  Mmax <- control$Mmax
  # B: a data frame with optimal basis function as columns
  B <- init_B(N,Mmax)
  # splits: a data frame records all the optimal (m, v, t):
  splits <- data.frame(m=rep(NA,Mmax*2-1),v=rep(NA,Mmax*2-1),t=rep(NA,Mmax*2-1))

  # m: parent basis func to split, v: splitting var, t: splitting point
  #Bfuncs: a list to store information on basis functions
  # initialize as an empty list of length Mmax+1
  Bfuncs <- vector(mode = "list", length = Mmax+1)
  Bfuncs[[1]] <- NULL

  # Looping for forward selection:
  # only need to loop over half since adding pairs
  for(i in 1:(Mmax/2)) {
    M <- 2*i-1
    lof_best <- Inf
    for(m in 1:M) {
      for(v in setdiff(1:n, Bfuncs[[m]][,2])) {
        tt <- split_points(x[,v],B[,m])
        for(t in tt) {
          Bnew <- data.frame(B[,(1:M)],
                             Btem1=B[,m]*h(1, x[,v], t),
                             Btem2=B[,m]*h(-1, x[,v], t))
          gdat <- data.frame(y=y,Bnew)
          lof <- LOF(y~.,gdat, control=control)
          if(lof < lof_best) {
            lof_best <- lof
            splits[M,] <- c(m,v,t)
          } # end if
        } # end loop over splits
      } # end loop over variables
    } # end loop over basis functions to split
    # save optimal (m, v, t) and update basis functions
    mstar <- splits[M,1]; vstar <- splits[M,2]; tstar <- splits[M,3]


    cat(paste("[Info] best (m,v,t,lof): (", mstar, vstar, tstar, lof_best, ")\n"))

    if(!is.numeric(mstar) || mstar < 1 || mstar > ncol(B)) {
      stop("Invalid value for mstar")
    }

    # Update B
    B[,M+1] <- B[,mstar]*h(-1, x[,vstar], tstar)
    B[,M+2] <- B[,mstar]*h(+1, x[,vstar], tstar)

    Bfuncs[[M+1]] <- rbind(Bfuncs[[mstar]], c(-1,vstar,tstar))
    colnames(Bfuncs[[M+1]]) <- c("s","v","t")
    Bfuncs[[M+2]] <- rbind(Bfuncs[[mstar]], c(1,vstar,tstar))
    colnames(Bfuncs[[M+2]]) <- c("s","v","t")


  } # end loop over M

  colnames(B) <- paste0("B",(0:(ncol(B)-1)))

  return(list(y=y,B=B,Bfuncs=Bfuncs))
}

bwd_stepwise <- function(fwd_output, control) {

  # Extract variables from fwd_output
  y <- fwd_output$y
  B <- fwd_output$B
  Bfuncs <- fwd_output$Bfuncs
  Mmax <- control$Mmax

  # Initialize variables
  Jstar <- 2:(Mmax + 1)
  Kstar <- Jstar
  lofstar <- LOF(y~.-1, data = B, control)

  # Outer loop over model size
  for (M in Mmax:2) {

    # Initialize variables for this loop
    L <- Kstar
    b <- Inf

    # Inner loop over model terms to remove
    for (m in L) {

      # Remove mth basis function from L
      K <- setdiff(L, m)

      # Fit model with basis functions in K
      Bk <- B[, c(1, K)]
      lof <- LOF(y~.-1, data = Bk, control)

      # Update Kstar if lof is better
      if (lof < b) {
        Kstar <- K
        b <- lof

        # Update Jstar if lof is best seen so far
        if (b < lofstar) {
          Jstar <- Kstar
          lofstar <- b
        }
      }
    }
  }

  # Add intercept index to Jstar
  Jstar <- c(1, Jstar)

  # Return selected basis functions and their corresponding functions
  return(list(y=y,B=B[, Jstar],Bfuncs=Bfuncs[Jstar]))
}

LOF <- function(form, data, control) {

  # Fit the linear regression model
  mod <- lm(form, data)
  # Calculate the residual sum of squares
  RSS <- sum((mod$res)^2)
  # Get the number of coefficients (excluding intercept)
  M <- length(coefficients(mod)) - 1
  # Get the number of observations
  n <- nrow(data)
  # Calculate the sum of the hat values
  Ctilde <- sum(diag(hatvalues(mod))) + control$d * M
  # Calculate the generalized cross-validation criterion
  GCV <- RSS / ((n - Ctilde)^2 / n)

  # Return the GCV criterion
  return(GCV)
}

#LOF <- function(form,data) {
#  ff <- lm(form,data)
#  return(sum(residuals(ff)^2))
#}

init_B <- function(N,Mmax) {
  # Input: N- # of rows; Mmax: # of basis funcs
  # output: a N by (Mmax+1) dataframe
  B <- data.frame( matrix(NA,nrow=N,ncol=(Mmax+1)) )
  B[,1] <- 1 # first column for intercept: B0
  names(B) <- c("B0",paste0("B",1:Mmax))
  return(B)
}

split_points <- function(xv,Bm) {
  # input: xv: a variable xv to split
  #        Bm: a parent basis func to split
  # output: feasible splitting points
  out <- sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

h <- function(s, x, t) {
  pmax(0, s * (x - t))
}
