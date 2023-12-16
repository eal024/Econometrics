


# Creat simulated data ----------------------------------------------------

library(MASS);library(tidyverse)

# create two correlated variables
set.seed(123)

matrix(c(1, 0.5, 0.5, 1), 2, 2)

mvrnorm(n = 5,mu = c(10,10), matrix(c(1, 0.5, 0.5, 1), 2, 2) )



two_var <- mvrnorm(n = 30,mu = c(10,10), matrix(c(1, 0.5, 0.5, 1), 2, 2) )

# x <- two_var[,1]
# z <- two_var[,2]

# Convert to df (tibble)
df <- tibble( x = two_var[,1], z = two_var[,2])

lm( x ~z, data = df) %>% summary

# y is a function of x and z. But z is unobserved.
df_new <- df %>% mutate( y = 0.5*x - 0.3*z + rnorm(length(df$x), mean = 0, sd = 0.1) )

# The wrong model:
df_new %>% 
  lm( y ~ x, data = .) %>% summary

# The right model:
options( scipen = 9)
df_new %>% 
  lm( y ~ x + z, data = .) %>% summary

df_new %>% 
  lm( x ~  z, data = .) %>% summary

# look at the graphical

df_new %>% 
  ggplot() + aes( y = y, x = z) + geom_point() + geom_smooth( method = "lm", se = F)



# Blogg -post -------------------------------------------------------------

# Common variance

xStarAndC <- MASS::mvrnorm(n = 500,mu = c(10,10), matrix(c(1, 0.5, 0.5, 1), 2, 2) )


# z 
w <- rnorm(n = 500)

x <- xStarAndC[,1] + w

y <- 1 + xStarAndC[,1] + xStarAndC[,2] + rnorm(100, 0 , 0.5)

# Modered correlated
cor(xStarAndC[,1], xStarAndC[,2])

# Uncorrelated
cor(w, xStarAndC[,2])

# Estimated the model:

df <- tibble(x = xStarAndC[,1] , z = xStarAndC[,2], w = w , y = 1 + x + z + w)

# x ~ y 
df %>% 
  lm( y ~ x, data = .) %>% summary

# y ~ z
df %>% 
  lm( y ~ z, data = .) %>% summary

# y ~ z + x: THe true model:
df %>% 
  lm( y ~ x + z, data = .) %>% summary

# This show that y ~ x gives a bias of 140%. 

# If z is unobserved: Used 2SLS


xhat <- lm(df$x ~ df$z)$fitted.values

df$xhat <- xhat


lm( df$y ~xhat)






















