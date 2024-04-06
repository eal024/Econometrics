
# packages
library(tidyverse)

# data: titanic
titanic <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/titanic.dta") |>  
    mutate(
        d = ifelse( class == 1, 1, 0)
    )

# Calcualte the naive mean
Ey1 <- titanic$survived[titanic$d == 1] |> mean()
Ey0 <- titanic$survived[titanic$d == 0] |> mean()

# The mean: 35% more likely to survive, if class 1.
Ey1 - Ey0

# Problem: More likely to be child and women, if you are class 1. or visa versa
# the estimate is biased. (ATE)

# Child or adult
# age == 1: adult
# s = 1, man
titanic <- titanic |> 
    mutate(
        s = as.numeric(survived),
        strata = paste0("s:",sex, "-a:", age) |> as.factor() 
    )

# Renaming variables
titanic <- titanic |> 
    mutate( 
        fage = ifelse(age == 1, "adults", "child") |> as.factor(),
        fsex = ifelse( sex == 1, "man", "female") |> as.factor()
    )


# Calculating the difference in survivel, based on age and sex
# Totalt numbers
number_not_first_class <- nrow(titanic |> subset(d == 0))


stat <- titanic |> 
    group_by(fsex, fage,d) |> 
    summarise(
        mean = mean(s),
        n = n()
    ) 

# Differences
stat_calcuated <- stat |>
    pivot_wider( names_from = d, values_from = c(n, mean)) |>   
    mutate( 
        diff =  mean_1 - mean_0,
        # The distribution amoungs them at class 0 = weights
        weight =( n_0/number_not_first_class ), 
        wdiff = weight*diff
    )


# For å ta høyde for at class 0 bestod av flere menn og eldre.
# 1) Kalulerer diff. mellom sex, age, d = 1 og d = 0.
# 2) Vekter de ulike differansene med fordelingen blant d == 0

# condition on the confounders gender and age

sum(stat_calcuated$wdiff)

# The naive difference 
mean(titanic$s[titanic$d == 1]) - mean(titanic$s[titanic$d == 0])

titanic |> group_by(d) |> summarise( s = mean(s)) 

