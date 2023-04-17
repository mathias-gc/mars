print.mars <- function(x, ...) {
  cat("\nMARS Summary:\n\n")

  # Print call
  cat("Call:\n")
  print(x$call)
  cat("\n")

  # Print basis functions
  cat("Basis functions:\n")
  for (i in seq_along(x$Bfuncs)) {
    cat(paste0("  ", i, ". ", nrow(x$Bfuncs[[i]]), " basis functions with ", ncol(x$Bfuncs[[i]]) - 1, " predictors.\n"))
    if (i == 1) {
      cat("     Intercept.\n")
      cat("\n")
    } else {
      for (j in 1:nrow(x$Bfuncs[[i]])) {
        s <- ifelse(x$Bfuncs[[i]][j, "s"] == 1, "+", "-")
        cat(paste0("     ", s, " ", x$x_names[x$Bfuncs[[i]][j, "v"]], " ", round(x$Bfuncs[[i]][j, "t"], 3), "\n"))
      }
      cat("\n")
    }
  }

}
