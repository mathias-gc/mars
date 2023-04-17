## Example 1

# read in the data
data(swiss)


# Create a MARS model with the 'lotsize' variable as the predictor
model1 <- mars(Examination ~., data = swiss)

# Get the predicted values for the first five observations in the dataset
predicted1 <- predict(model, swiss[1:10,])

# Print the predicted values
print(predicted1)

# Print the summary of the model
summary(model1)

# Run ANOVA
anova(model1)

# Plot
plot(model1)


## Example 3
data(airquality)

# Create a MARS model with the 'lotsize' variable as the predictor
model2 <- mars(Ozone ~., data = airquality)

# predicted
predicted2 <- predict(model2, airquality[1:10,])

# Print the predicted values
print(predicted2)

# Print the summary of the model
summary(model2)

# Run ANOVA
anova(model2)

# Plot
plot(model2)


## Example 3
data(airquality)

# Create a MARS model with the 'lotsize' variable as the predictor
model3 <- mars(Ozone ~., data = airquality)

#predictive
predicted3 <- predict(model3, airquality[1:10,])

# Print the predicted values
print(predicted3)


# Print the summary of the model
summary(model3)

# Run ANOVA
anova(model3)

# Plot
plot(model3)