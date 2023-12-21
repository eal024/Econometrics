
library(tidyverse)
library(fixest)
theme_set( theme_bw(base_size = 20))

# Data
n <- 20
df <- data.frame( 
    id     = seq(1,n, by = 1),
    hight  = rnorm( n = n, mean = 180, sd = 10 )
)

df1 <- df |> 
    within(
        {
         weigth = hight/2.5 + rnorm(n = n , mean = 5, sd = 2)   
         y =    hight*0.6 - weigth*0.5 + rnorm(n = n, mean = 10, sd = 1)
        })


# Viz the data
df1 |> 
    ggplot( aes(y = y, x = hight) ) + 
    geom_point()


# OLS by function---------------------------------------
lm( data =df1, y ~ hight) |> summary()
lm( data =df1, y ~ hight + weigth) |> summary()

# OLS by matrix calculation-----------------------------
X <- cbind( 1, df1$hight, df1$weigth)
y <- df1$y

solve(t(X)%*%X) %*% t(X)%*%y






