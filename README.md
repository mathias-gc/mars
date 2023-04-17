# mars
Multivariate Regression Adaptive Splines (MARS) implementation in R

## Group:
Mathias Gausachs

## List of files

### Main file

`mars.R` The function mars performs multivariate adaptive regression splines (MARS) analysis, which is a regression method that generates a piecewise linear function of basis functions.

### Files for methods

`predict.R`: This function predicts the response variable based on the MARS model object and the new data given as input. It first checks if the new data is missing, and if so, uses the existing MARS model's coefficient matrix, otherwise it computes the coefficient matrix from the new data by using the MARS model's formula, B functions, and coefficients. The output is a vector of predicted values for the response variable.

`anova.R`: This function computes and prints an ANOVA table for the MARS model object. It uses the sum of squares of the residuals, total sum of squares, and degrees of freedom to calculate the R-squared and adjusted R-squared values. The table shows the number of terms, residuals, sum of squares, R-squared, and adjusted R-squared.

``plot.R`: This function creates a scatterplot of the observed response variable versus the fitted values obtained from the MARS model object. The y-axis label can be customized, and additional arguments can be passed to the plot function.

`print.R`: This function prints a summary of the MARS model object. It displays the call used to create the object and the basis functions used in the model.

`summary.R`: This function provides a more detailed summary of the MARS model object. It first displays the call used to create the object, and then provides information on the basis functions used in the model. Finally, it displays the response variable, fitted values, residuals, and the names of the predictors used in the model.
