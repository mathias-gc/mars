summary.mars <- function(object, ...) {
  cat("\nMARS Summary:\n\n")
  cat("Call:\n")
  print(object$call)
  cat("\n")
  
  # Extract variables from object
  y <- object$y
  fitted.values <- object$fitted.values
  residuals <- object$residuals
  x_names <- object$x_names
  Bfuncs <- object$Bfuncs

  # Print information on the basis functions
  cat("Basis functions:\n")
  for (i in 1:length(Bfuncs)) {
    cat("\n")
    cat(paste0("  ", i, ". ", nrow(Bfuncs[[i]]), " basis functions with ", ncol(Bfuncs[[i]]) - 1, " predictors.\n"))
    if (i == 1) {
      cat("     Intercept.\n")
    } else {
      for (j in 1:nrow(Bfuncs[[i]])) {
        s <- ifelse(Bfuncs[[i]][j, "s"] == 1, "+", "-")
        cat(sprintf("     %s %s %s\n", s, x_names[Bfuncs[[i]][j, "v"]], format(round(Bfuncs[[i]][j, "t"], 3), nsmall = 3)))
      }
    }
  }
  cat("\n")
  
  # Print information on the residuals
  cat("Residuals:\n")
  print(summary(residuals))
  cat("\n")
  
  # Print information on the fitted values
  cat("Fitted values:\n")
  print(summary(fitted.values))
  cat("\n")
  
  invisible(object)
}
