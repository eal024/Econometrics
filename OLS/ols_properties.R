
library(ggplot2)

# Simulated data
N <- 10000  # True size of the population
X <- runif(N, min = 0, max = 20)
u <- rnorm(N, sd = 10)

y <- -2 + 3.5*X + u

# Data.frame 
df <- data.frame( 
        x = X,
        y = y
    ) 


set.seed(1)

# Unbiased
reps <- 1000
fit <- matrix( ncol = 2, nrow = reps) # For storing data

n <- 100
#df[sample(1:N, n), ]

for( i in 1:reps){

    sample <- df[sample(1:N, n), ] # Choose a random sample
    fit[i, ] <- lm( y ~ x, data = sample)$coefficients
}

# as data.frame
fit <- as.data.frame(fit)  |>  setNames(c("b0", "b1"))

# Normal distr. around true value
ggplot(data =fit, aes( x = b1) ) +
 geom_histogram( color = "white") +
 geom_vline( xintercept = 3.5, color = "red", linetype = 2, size = 2)



# consistancy
reps <- 1000
fit <- matrix( ncol = 2, nrow = reps) # For storing data
n <- seq(from =10, to = 10000,  length.out = reps) |> as.integer( )
#df[sample(1:N, n), ]

for( i in 1:reps){

    sample <- df[sample(1:N, n[i]), ] # Choose a random sample
    fit[i, ] <- lm( y ~ x, data = sample)$coefficients
}

# as data.frame
fit <- as.data.frame(fit)  |>
    setNames(c("b0", "b1")) |>
    transform(
        index = 1:nrow(fit)
    )

# Norm
ggplot(data =fit, 
    aes( y = b1, x = index) ) +
    geom_line( )

