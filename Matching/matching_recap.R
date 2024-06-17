
# Recap matching
library(tidyverse)

# 1) Exercise 1, Seminar 4, ECON5106

dat <- tibble::tibble(
    x = c(1,1,1, 2,2,2, 3,3,4,5),
    d = c(0,1,1,0,1,0,1,1,0,0),
    y = c(10,15,20,25,30,30,25,35,50,55),
    id = c(1:10)
)

# a) Estimate causal effect T linear regression
modela <- lm( data = dat,  y ~ x + d)
summary(modela)

# What assumption need for gamma causal?
    # Assumtion of d to be causal
    # Non omiited variables, where cov(y,z) != 0, and c(d,z) != 0
    # x is exogen to d 
    # CIA and CS

# b) The ATT using the exact matched method
dat2 <- dat |>
    summarise( y = mean(y), .by = c(x,d))  |> 
    pivot_wider( names_from = d, values_from = y)


# Need to drop 3:5, for CS:
dat3 <- dat2 |> filter(x %in% c(1,2)) |> mutate( diff = `1`-`0`)

# The 1:1 matched treatment effect
mean(dat3$diff)

# c) Why do a) differ from b) ?
# Assumption of continutiy vs. categorical
modelb <- lm( data = dat,  y ~ factor(x) + d)

modelsummary::modelsummary( list(modela, modelb))

# 2) Example two: The effect of union or non union on wage
dat <- haven::read_dta("data/nlsw88.dta")

# a) OLS with and without controls: Why does the result differ

names(dat)
model1 <- lm( data = dat, wage ~ union)
model2 <- lm( data = dat, wage ~ age + union + race + married + grade + collgrad + hours + south + smsa + c_city)
modelsummary::modelsummary(list(model1, model2))

# Comparing union vs. non union workers
