# You can call this function on a mars object to create a scatterplot
# of the observed vs. fitted values. You can customize the y-axis
# label and pass additional arguments to the plot function through the ... argument.
plot.mars <- function(x, ylab = "Response", ...) {
  
  # Extract fitted values and response variable
  fitted.values <- x$fitted.values
  y <- x$y
  
  # Create a scatterplot of observed vs. fitted values
  plot(y, fitted.values, xlab = ylab, ylab = "Fitted Values", ...)
  
  # Add a 45-degree line
  abline(a = 0, b = 1, col = "red")
  
  # Legend
  legend("bottomright", legend = c("Observed", "Fitted"),
         pch = c(1, 19), col = c("black", "blue"), bg = "white")
}
