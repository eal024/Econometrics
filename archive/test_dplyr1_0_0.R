


# Test of latest version of dplyr:1.0.0

devtools::install_github("tidyverse/dplyr")
library(tidyverse)

# select abd renaming

df <- mtcars %>% as_tibble() %>% head()

# selecet by number and position
df %>% select(1,5,10)
df %>% select(1:4)

# By type
df %>% mutate_at( c("am","carb"), as.factor) %>% 
  select(is.factor )

# Combination of
df %>% mutate_at( c("am","carb"), as.factor) %>% 
  select(is.factor | starts_with("c") | ends_with("p") )

# all_of and any_of

vars <- c("am", "disp", "hp", "Eirik")

df %>% select(any_of(vars))

df %>% select(all_of(vars))

# Relocate
df <- df %>% mutate_at( c("am","carb"), as.factor)

df %>% relocate(is.factor)

df %>% relocate(hp)

df %>% relocate(wt, .before = vs)
df %>% relocate(wt, .after = last_col())





