predict.mars <- function(object,newdata) {
  if(missing(newdata) || is.null(newdata)) {
    B <- as.matrix(object$B)
  }
  else {
    tt <- terms(object$formula,data=newdata)
    tt <- delete.response(tt)
    mf <- model.frame(tt,newdata)
    mt <- attr(mf, "terms")
    X <- model.matrix(mt, mf)[,-1] # remove intercept
    B <- make_B(X,object$Bfuncs)
  }
  beta <- object$coefficients
  drop(B %*% beta)
}

make_B <- function(X, Bfuncs) {
  n <- nrow(X)
  p <- length(Bfuncs)
  B <- matrix(1, n, p)
  for (j in 1:p) {
    bfunc <- Bfuncs[[j]]
    if (length(bfunc) == 0) next
    s <- bfunc[, "s"]
    v <- bfunc[, "v"]
    t <- bfunc[, "t"]
    if (is.null(v)) {
      # basis function for intercept
      B[, j] <- s
    } else {
      # product of hinge functions
      pmax_term <- function(i) {
        h(s[i], X[, v[i]], t[i])
      }
      B[, j] <- Reduce(`*`, lapply(seq_along(s), pmax_term))
    }
  }
  return(B)
}


# hinge function
h <- function(s, x, t) {
  pmax(0, s * (x - t))
}
