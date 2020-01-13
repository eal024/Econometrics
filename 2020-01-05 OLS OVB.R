


# library and data --------------------------------------------------------

library(tidyverse)

n <- 1000

x1 <- rnorm(n, mean = 2, sd = 1)

#Helper:
x2 <- rnorm(n, mean = 1, sd = 1)
# Corr. variables omitted
x3 <- 0.5*x1 + 1*x2 + rnorm(n = n, mean = 0, sd = 1)



y <- 3 + 2*x3 - 3*x1 + rnorm(n, mean = 0, sd = 1)

df <- tibble( x1 = x1, x2 = x2, x3 = x3, y = y)

# true model:
df %>%  lm(y ~ x3 + x1, data =.) %>% summary

# Dont observe x1 
df %>% lm(y ~ x1, data = .) %>% summary


plot(df)

cor(df$x1, df$x3)




# The full example --------------------------------------------------------
# Omitted variable bias


create_data <- function(n_obs = 1000 ) {
  n <- n_obs
  
  x1 <- rnorm(n, mean = 2, sd = 1)
  x2 <- rnorm(n, mean = 1, sd = 1)
  x3 <- rnorm(n, mean = 3, sd = 1)
  x4 <- 4*x1 + 4*x2 + rnorm(n = n, mean = 0, sd = 1)
  
  y <- 3 + 3*x1 + 2*x2 - 3*x3 + rnorm(n, mean = 0, sd = 1)
  
  df <- tibble( x1 = x1, x2 = x2, x3 = x3, x4, y = y)
  
  return(df)
}

df <- create_data(1000)

df

plot(df)

df %>% lm(y ~ x1 + x2 + x3 + x4 , data = .) %>% summary

# Omitt x4: Both variables X1 and x2 are revised upward!
df %>% lm(y ~ x1 + x2 + x3  , data = .) %>% summary

bOLS_minus_bTRUE <- function( cor_x1_x2 = 0.5 ) {
  
  
}



# Example: Exam ~ presence (at seminars) ----------------------------------

# approx normaldistrib. integers 0 - 100. OBS -> Normal. distribued X is unbund and a real-number 
z <- rbinom(100000, 15, 0.5 ) %>% as.tibble()

z %>% ggplot( ) + aes( x = value ) + geom_histogram( binwidth = length(max(z$value))/1 )

  
  
  
  
  
  
  
  
  
  
  