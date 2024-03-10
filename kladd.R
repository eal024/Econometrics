

x <- c(-1.75, -0.93, 0.10, 0.24, -0.09)


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

