anova.mars <- function(object, ...) {
  y <- object$y
  n <- length(y)
  p <- length(object$x_names)
  rss <- sum(object$residuals^2)
  tss <- sum((y - mean(y))^2)
  R_squared <- 1 - (rss / tss)
  Adj_R_squared <- 1 - ((1 - R_squared) * (n - 1) / (n - p - 1))
  df <- object$df.residual
  cat("Analysis of Variance Table\n")
  cat("Model: MARS\n")
  cat("Terms:", length(object$B), "\n")
  cat("Residuals:", df, "\n")
  cat("Sum of Squares:", rss, "\n")
  cat("R-squared:", R_squared, "\n")
  cat("Adj R-squared:", Adj_R_squared, "\n")
  invisible(NULL)
}
