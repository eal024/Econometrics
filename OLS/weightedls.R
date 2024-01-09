
# https://www.statology.org/weighted-least-squares-in-r/

# Data
df <- data.frame(
    hours = c(1, 1, 2, 2, 2, 3, 4, 4, 4, 5, 5, 5, 6, 6, 7, 8),
    score = c(48, 78, 72, 70, 66, 92, 93, 75, 75, 80, 95, 97, 90, 96, 99, 99)
)


# 2. LM
summary( model <- lm( data = df, score ~hours) )

# 3. Heteroscedasticity test

# Plot
plot(fitted(model), resid(model))
abline(0,0)

# Formal test, Breusch-Pagan test:
# Null Hypothesis (H0): Homoscedasticity is present (the residuals are distributed with equal variance)

library(lmtest)

# p-value from the test is 0.0466
# reject the null hypothesis
# conclude heteroscedasticity  
bptest(model)


# 
varw <- log(resid(model)^2)

fitted.varw <- lm(
    data = data.frame( varw = varw, hours = df$hours),
    varw ~ hours + I( hours^2)
)

w <- exp(fitted(fitted.varw)) 

summary( lm( score ~ hours, data = df, weights = 1/w) )

# The coeff. change a bit
# the overall fit to the model improve.
# Residual standard error: 1.937 vs. 9.224
# This indicates that the predicted values produced by the weighted least squares model are much closer to the actual observations
# R2 0.74 vs. 0.63m wls has a better fit.


