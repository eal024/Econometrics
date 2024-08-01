# Generate sample data with stronger endogeneity
set.seed(123)
n <- 100
Z <- rnorm(n)
W <- rnorm(n)
error_term <- rnorm(n)
X <- 0.5 * Z + 0.3 * W + error_term  # X is correlated with the error term
Y <- 2 * X + 0.5 * W + error_term  # Error term is included in both X and Y

df <- data.frame(Y = Y, X = X, Z = Z, W = W)

# First stage regression: X on Z and W
first_stage <- lm(X ~ Z + W, data = df)
df$residuals_first_stage <- residuals(first_stage)

# Second stage regression: Y on X, W, and residuals from first stage
second_stage <- lm(Y ~ X + W + residuals_first_stage, data = df)
summary(second_stage)

# Extract the coefficient and t-statistic for the residuals_first_stage
coef_residuals <- summary(second_stage)$coefficients["residuals_first_stage", "Estimate"]
t_stat_residuals <- summary(second_stage)$coefficients["residuals_first_stage", "t value"]
p_value_residuals <- summary(second_stage)$coefficients["residuals_first_stage", "Pr(>|t|)"]

# Print the results
cat("Coefficient of the residuals from the first stage:", coef_residuals, "\n")
cat("t-statistic for the residuals from the first stage:", t_stat_residuals, "\n")
cat("p-value for the residuals from the first stage:", p_value_residuals, "\n")
