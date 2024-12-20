# TESTOWANIE REGRESJI LINIOWEJ --------------------------------------------

df

# Fit a linear regression model
model <- lm(Wskaznik ~ M.c, data = df)

# Print summary of the regression model
summary(model)

# Test for significance of the year coefficient
anova(model)

# Check assumptions of the regression model
plot(model)

# Make predictions
predicted_values <- predict(model)

# Plot observed vs. predicted values
plot(df$Wskaznik, predicted_values, 
     xlab = "Observed Wskaznik", ylab = "Predicted Wskaznik",
     main = "Observed vs. Predicted",
)
abline(0, 1, col = "red")

# TESTOWANIE REGRESJI WIELOMIANOWEJ -----------------------------------------------------------------------

polynomial_model <- lm(Wskaznik ~ poly(Rok, degree = 5), data = df)

# Print summary of the polynomial regression model
summary(polynomial_model)

# Test for significance of the polynomial terms
anova(polynomial_model)

# Check assumptions of the polynomial regression model
# plot(polynomial_model)

# Make predictions
predicted_values_polynomial <- predict(polynomial_model)

# Plot observed vs. predicted values
plot(df$Wskaznik, predicted_values_polynomial, xlab = "Observed Wskaznik", ylab = "Predicted Wskaznik (Polynomial Regression)", main = "Observed vs. Predicted (Polynomial Regression)")
abline(0, 1, col = "red")
