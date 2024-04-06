
library(tidyverse)

# Samplings-------------------

# nonparametric bootstrap involves randomly sampling
# random sampling from the sample of intereset, with replacement.

x <- letters[1:5] 

set.seed(1)
sample(x, size = length(x), replace = T)


# Parametric Statistics Prime

# X is the random variable of interest.
# Assume F(x) = P(X < x)
# Suppose distribution is a functoin on  tau = t(F) , t() some function of F
# tau is some constant describes the ~population

# Suppose:
# random sample x = (x1,x2,..,xn)m from population F,
# x ~iid realization of X
# If we calculate hat_tau = s(x), than s() (some function of x)
# hat_tau is also a raondom variable 

# hat_tau is a random variable with a pop. distribiton: G(hat_tau) = P(hat_tau < tau)

# Example 1: mean: mean( rnorm(n))

# hat_tau:  mean O = E(x)

# hat(θ) = (1/n) sum(x)
# X∼N(θ,σ2) , then the sampling distribution is Gaussian, i.e., θ^∼N(θ,σ2/n)

# Variance decrease with n
par(mfrow = c(1,2), mar = c(2,2))
for( n in c(5,50) ){
    x <- replicate( 10000, mean( rnorm(n = n)))

    # Plot
    hist(x, freq = F, breaks = 20, main = paste0("N: ", n))
    xseq <- seq(-1, 1, length.out = 1000) 
    lines( xseq, dnorm(xseq, sd = 1/sqrt(n)), col = "red" )

}

## Empirical distribution

# Normal distribution
# Shows how the Indicator function calucalte the ECDF 
fn_empircal_distribution <- function(size, f = rnorm){
    
    # Data normal
    x <- f(n = size)  # X ~N(0,1)
    x <- sort(x) # Sort the data

    first_point <- 1/length(x) # = P(X < lowest number)
    second_point <- 2/length(x) # = P(X < secound lowest number)

    fn_ecdf <- ecdf(x) # The empircal CDF
    # first number ecdf(x[1]) <- return 1/length(x)

    df <- tibble( x = x, ecdf = fn_ecdf(x) )

    df |> 
        ggplot( aes( x = x, y = ecdf)) +
        geom_line() +
        geom_point() +
        # Mark the first point
        geom_point( 
            data = tibble( x = x[1], y = first_point), 
            aes( x = x, y= y),
            inherit.aes = F,
            color = "red",
            size = 3
        ) +
        # Mark the secound
        geom_point( 
            data = tibble( x = x[2], y = second_point), 
            aes( x = x, y= y),
            inherit.aes = F,
            color = "green",
            size = 3
        )    +
        geom_line( 
            data = tibble( x = seq(-5,5, by = .1), y = pnorm(x) ),
            aes( y = y, x = x),
            inherit.aes = F,
            color = "red"
    )
}

# Normal distribtion
fn_empircal_distribution(size = 10, f = rnorm)
fn_empircal_distribution(size = 100)

# Usefull if we want the CDF, and we only have a sample, and dont know the CDF
# E(F^n(x))=F(x)
# Var(F^n(x))=1nF(x)(1−F(x))
# supx∈R|F^n(x)−F(x)|→as0

# Uniform
x <- runif(n = 100, min = -sqrt(3), max = sqrt(3))

# Teoretical
xseq <- seq(-2, 2, length.out = 100)
tpunif <- punif(xseq, min = -sqrt(3), max = sqrt(3))
df_theory <- tibble( x = xseq, y = tpunif)

# Emprical
fn_ecdf <- ecdf(x)

tibble( x = sort(x), y = fn_ecdf(x) ) |>
    ggplot( aes(x = x, y = y) ) +
    geom_line() +
    geom_point() +
    geom_line( data = df_theory, aes(x = x, y = y), inherit.aes = F, color = "red")

