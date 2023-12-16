
library(tidyverse)
# 

# Bootstrapping: Using data from only the stat. sample.
# Randomsample from the original sample, and multiply to a given big number, with replacement...



# infer-packages
library(infer)

data %>% 
  specify( response = variable_of_interers) %>% 
  generate( reps = number_of_reps, type = "bootstrap") %>% 
  calculate( stat = "___")

# example data
manhatten <-  read_csv("https://assets.datacamp.com/production/repositories/846/datasets/bd62fb71666052ffe398d85e628eae9d0339c9c4/manhattan.csv")

manhatten

manhatten %>% infer::specify( response = rent) %>% generate( reps = 100, type = "bootstrap") %>% 
  calculate( stat = "mean")


#
df <- tibble(n = 100,  )