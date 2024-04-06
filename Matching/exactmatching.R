
# Example of exact matching: https://mixtape.scunning.com/05-matching_and_subclassification


# data and packages
library(tidyverse)

# Age for training group. and earnings
training <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/training_example.dta")


# data
df1 <- training |>  select( age = 2, earnings = 3) |> mutate( treat = 1, earnings = as.numeric(earnings)) |> na.omit() 
df2 <- training |>  select( age = 5, earnings = 6) |> mutate( treat = 0)   |> na.omit() 

data <- bind_rows(df1, df2) |> mutate( unit = 1:n())

# Problem: the agediff.
# Look at the distribution of age
data |> 
    ggplot( aes( age , fill = as.factor(treat) ) ) +
    geom_density( alpha = 0.5) +
    labs( title = "The treated group are younger than the non treated")
    

# Naive difference between traeted and non treated
data |> 
    group_by(treat) |> 
    summarise(
        m = mean(earnings, na.rm =T)
    ) |> 
    pivot_wider( names_from = treat, values_from = m) |> 
    mutate( e = `1` - `0`)


## Exact Matching

# 1) Keeping only the age for those in traning (treated)
age_treated <- data |> filter( treat == 1) |> distinct( age ) |> pull()

# Earnings grouped by age and treat
stat <- data |>
    filter( age %in% age_treated ) |> 
    group_by( treat, age) |> 
    summarise(
        mean_earnings = mean(earnings),
        .groups = "drop"
    )
    
# The diff for each age groups
wider <- stat |> pivot_wider( names_from = treat, values_from = mean_earnings) |> mutate( diff = `1` - `0`)

wider |> summarise_all( \(x) mean(x, na.rm= T) )

mean(wider$diff, na.rm =T)





