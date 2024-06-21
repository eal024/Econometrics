

# Selection on observables
# Exercises from seimar ECON5106 uio.

library(tidyverse)

data <- tibble(
    x = c(1,1,1,2,2,2,3,3,4,5),
    d = c(0,1,1,0,1,0,1,1,0,0),
    y = c(10,15,20,25,30,30,25,35,50,55),
    id = 1:10
)


# a) Estimate the causal effect of T, linear regression
summary(lm( data = data, y ~ x + d))

# Assumtion of d to be causal
# Non omiited variables, where cov(y,z) != 0, and c(d,z) != 0
# x is exogen to d 

# b) Exact matching
data_b <- data |> 
    # For calcualte, need x to both be for d = 1 and d = 0
    mutate(
        rep = n_distinct(d), .by = x
    )  |> 
    # keeping only those who both have d = 0 and d = 1
    filter( rep == 2)

data_b1 <- data_b  |> 
    group_by(x, d) |> 
    summarise(
        y = mean(y)
    ) |> 
    pivot_wider(
        names_from = d,
        values_from = y
    ) |> 
    mutate( diff = `1` - `0`)


mean(data_b1$diff) 

# There is a large difference between linear reg. and the exact matching

# c) reducting the difference

# By transforming the controlvariable as factor, the model treat x as level, and not as continues. 
data |> 
    lm( formula = y ~ factor(x) + d) |> 
    summary()


## 2) Effect of union status on log hourly wage
nlsw <- haven::read_dta("data/nlsw88.dta")

# First look at the difference between those who are members of union and those who are not.
nlsw |> 
    group_by(union) |> 
    summarise(
        count = n(),
        age = mean(age),
        race = mean(race),
         = mean(collgrad) 
    )


# Look at the effect on wage from union membersship
model1 <- nlsw |> lm( formula = log(wage) ~factor(union)) 
model2 <- nlsw |> lm( formula = log(wage) ~factor(union) + age + race + collgrad)

stargazer::stargazer( list(model1, model2), type = "text")







