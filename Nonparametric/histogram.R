
# The histogram, Simplest approach to a PDF/density estimation
# From: https://faculty.washington.edu/yenchic/18W_425/Lec6_hist_KDE.pdf (6.1)

# Histogram: 
# hist = (1/n)*nr. obs x mengde [a,b)]*(1/(b-a))
# hist = (1/n*(b-a))*sum( 1 indicator |(xi-x)/(b-a)| < 0.5 )


# Example data
x <- c(-1.75, -0.93, 0.10, 0.24, -0.09)

# Function for calcualting the hist point
fn_point <- function(x, a, b){

    n = length(x)                  # Number of observations
    mid.point = a + (b-a)/2        # midth point, x
    u  = abs( (x-mid.point)/(b-a)) # u functions 
    pr = sum( u <= 0.5 )/n # Calcuate pr 

    list(
        u = u,
        mid.point = mid.point,
        pr = pr
    )

}

# Example 1
intervals <- seq( from = -2, to = 1, by = 0.125)  # spesify the intervals, bindwidth

# Data frame
df  <-  tibble( 
    a = intervals,
    b = lead(intervals)
    ) |> 
    na.omit()

# Calcuate the point
df <- df |> 
    mutate(
        hist = map2(a,b, \(a,b) fn_point(x =x, a =a, b = b ))
    ) |> 
    unnest_wider( hist)

# Plotting the histogram
df |> 
    ggplot(aes( x = mid.point, y = pr) ) +
    geom_col( width = 1, color = "white", position = "dodge2" )

# Solution base R
hist( x, breaks = intervals, prob = T)


# Base R histogram with density line

library(tidyverse)

theme_set( theme_light())

# Data
df <- tibble( 
    inntekt = rnorm( n = 1000, 0, 1) 
    )

# Figur
df |> 
    ggplot( 
        aes( x = inntekt)
    ) +
    geom_histogram( 
        aes( y = ..density..),
        binwidth = 0.1,
        color = 1,
        fill = "white",
        ) +
    geom_density()


hist( 
    df$inntekt,
    breaks = 20,
    prob = T,
    main = "Inntekt (G)",
    ylab = "prob.",
    xlab = "G",
    lwd = 2
)
lines( density(df$inntekt))