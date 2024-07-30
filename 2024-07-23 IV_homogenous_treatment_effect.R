
library(fixest)

# Instrument variable (IV) estimation
# When X1 is endogenous, the IV can be used to handle endogenous X

# The simple example: x2 is correlated with x1, and the true y explained of both x1 and x2
n  <- 100 
x1 <- rnorm(n = n, mean = 0, sd = 5)
x2 <- 0.5*x1 + rnorm(n = n, mean = 0, sd = 5)

u <- rnorm(n = n, mean = 0, sd = 1) + 0.7*x2
y  <- 1 + 0.5*x1 + u 

df <- data.frame( y = y, x1 = x1, x2 = x2)

model1  <- feols( data = df, y ~ x1)
model2  <- feols( data = df, y ~ x1 + x2)

# Printing the result: Model 1 shows a endogens causal effect of x1 on y
esttable( list(model1, model2))

# Omitted variable bias -- the direction of the bias.


# IV assumptions
#The IV estimator (and the Wald estimator)

# Inference: Standard errors

# Two stage least squares


# Weak instruments


# Hausman- and Sargan tests