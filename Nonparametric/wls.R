
# Packages
library(tidyverse)

# Weighted Least Squares
# A more efficient alternative to OLS, in presens of heteroskedasticity 
#  If we are willing to make assumptions about the form of heteroskedasticity,
#  the use of the more efficient EGLS estimator is an option. (Verbeek, 2012: 95)

df <- wooldridge::k401ksubs |> as_tibble()  |> subset( fsize == 1)

# data : 
# nettfa: netto total fin. assets in $1000
# e401k:1 if participate in 401(k)
# fsize == 1;

# OLS
model1 <- lm( data = df, nettfa ~ inc + male)

# WLS
model2 <- lm( data = df, nettfa ~ inc + male, weights = 1/inc)

# Mannualy weighting the data
df$c <- 1
vars <- c("nettfa","c", "inc", "male")

df1 <- map_df( df[vars], \(x) x/df$inc^0.5)

model3 <- lm( data = df1, nettfa ~ 0 + c + inc + male)

stargazer::stargazer( list(model1, model2, model3), type = "text")

# The use of weights implies that observations with a higher variance get a smaller weight in estimation. 
# Loosely speaking, the greatest weights are given to observations of the highest quality
# and the smallest weights to those of the lowest quality. It is important (Verbeek, 2012: 84)


