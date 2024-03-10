
# Library and data
library(tidyverse)

df <- tibble( 
    x = c(-1.75, -0.93, 0.1, 0.24, -0.09),
    y = c(12.6, -0.38, 0.32, 0.77, -0.26)
)


# ----------------------------------------------

# WLS with the kernel K as the weighting function.
# The Nadaraya Watson estimator as WLS

# The x and y
y <- df$y
x <- df$x

u <- map_dbl(x,\(x) (x-(-2))/2 )  # Calculate the u
k <- map_dbl(u, \(u) if(u < 1) (3/4)*(1-u^2) else{0}) # Calculate the K

# Transform the value to data
dat <- tibble( x = x, y = y, w = k)
lm( data = dat, y ~  1, weights = k)  # And do the regression (Local constant regression)

# Repeat the prosedure for each point -2, -1, 0, 1, 2
rm(x,y, u, k, dat)

# This function is with the Epanechnikov Kernel
fn_local_regression_at_point <- function(p, x , y, h = 2){

    u <- map_dbl(x,\(x) (x- p)/h )  # Calculate the u
    k <- map_dbl(u, \(u) if( abs(u) < 1) (3/4)*(1-u^2) else{0}) # Calculate the K

# Transform the value to data
dat <- tibble( x = x, y = y, w = k)
model <- lm( data = dat, y ~  1, weights = k)  # And do the regression (Local constant regression)

model$coefficients
}

# Testing the function
fn_local_regression_at_point(p = -2, x = df$x, y = df$y, h = 2)

# Do local regression at the point -2 to 2 
map_dbl( c(-2:2), \(p) fn_local_regression_at_point(p = p, x = df$x, y = df$y, h = 2) )

haty <- map_dbl( c(-2:2), \(p) fn_local_regression_at_point(p = p, x = df$x, y = df$y, h = 2) )


# Plotting the data
ggplot( data = df, aes(y = y, x = x)) +
    geom_point() +
    geom_line( 
        data = tibble( x = -2:2, yhat =haty ),
        aes( y = haty, x = x),
        inherit.aes = F
        )



## Centered regression
# The x and y
y <- df$y
x <- df$x

u <- map_dbl(x,\(x) (x-(-2))/2 )  # Calculate the u
k <- map_dbl(u, \(u) if(u < 1) (3/4)*(1-u^2) else{0}) # Calculate the K

# Transform the value to data
dat <- tibble( x = x, y = y, u = u, w = k)
lm( data = dat, y ~   u, weights = k)  # And do the regression (Local constant regression)

u <- map_dbl(x,\(x) (x-(-1))/2 )  # Calculate the u
k <- map_dbl(u, \(u) if(u < 1) (3/4)*(1-u^2) else{0}) # Calculate the K

# Transform the value to data
dat <- tibble( x = x, y = y, u = u, w = k)
lm( data = dat, y ~   u, weights = k)  # And do the regression (Local constant regression)



# Look
# https://bookdown.org/egarpor/NP-UC3M/course-overview.html

