

# OLS and standard errors

# data
n <- 100
x <- runif( n, 0,1) |> round(2)

# Constant treatment effect
y0 <- rnorm(n, mean = x)
y1 <- y0 + 0.35 # Treatment effect 0.35

# Treatment Z = 1
z <- sample( size = n, c(0,1), replace = T, prob = c(0.5,0.5) )

# If treated y1 if not y0
y <- ifelse( z == 1, y1, y0)

df <- data.frame( y = y, x = x, z = z, y1, y0)


# OLS calcualtion matrix--------------------

intercept <- rep(1,n)
Y <- as.matrix(df[, "y"])
X <- cbind(
    intercept,
    as.matrix(df[, c("x", "z")])
    )

# Estimator: Beta = [(x'x)^-1*x']x'y
betas <- solve(t(X) %*% X) %*% t(X) %*%Y

betas
# Ceck the result
lm(data = df, y ~ x + z)

# Standard error-------------------------------


# Residuals
res <- Y - X%*% betas

vars <- ncol(X) - 1  # Numbers of variables
degree <- nrow(X)-vars-1 # Degree of freedom: Number of variables

# Residual variance
res_var <- sum(res^2)/degree

# Covariance matrix
beta_xcov <- res_var * solve( t(X)%*% X)

# Standard deviations coeffisients
beta_se <- sqrt( diag(beta_xcov) )

# Standard Robust errors HC0 ------------------------

u2  <- res^2
XDX <- 0

for( i in seq_along(u2)) {
    XDX <- XDX + u2[i] * X[i,] %*% t(X[i,])
}

A <- solve( t(X)%*% X)
# Robust se. = [X'X^-1] X'Gamma X [(X'X)^-1] 
# A %*% XDX %*% A

# Standard se. robust
varcov_m <- A %*% XDX %*% A
beta_se <- sqrt( diag(varcov_m) )

# Ceck the result from function use
lm(df, formula = y~x+z ) |> summary()
estimatr::lm_robust(df, formula = y~x+z, se_type = "HC0")


# Standard Robust errors HC1 (Stata version) ------------------------

u2  <- res^2
XDX <- 0

for( i in seq_along(u2)) {
    XDX <- XDX + u2[i] * X[i,] %*% t(X[i,])
}

A <- solve( t(X)%*% X)
# Robust se. = [X'X^-1] X'Gamma X [(X'X)^-1] 
# A %*% XDX %*% A

correction <- nrow(X) / (nrow(X) - vars - 1)

# Standard se. robust
varcov_m <- correction* A %*% XDX %*% A
beta_se <- sqrt( diag(varcov_m) )

beta_se
estimatr::lm_robust(df, formula = y~x+z, se_type = "stata") # or HC1

