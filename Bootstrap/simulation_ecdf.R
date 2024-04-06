
# true data
x <- rnorm( n = 10000, 0, sd = 2)

# True CDF
plot.ecdf(x)

# Sample
sim <- sample(x, size = 50, replace = T)

# order the data
sim <- sort(sim)

# The indcation function
indication <- sapply( 1:length(sim),\(x) x/length(sim), simplify = "numeric" )

# Set the I() in a frame
df <- data.frame( x = sim, indication = indication)

# Plot the "true" CDF and the empircal CDF
plot.ecdf(x)
lines( df$x, df$indication, col = "red")
points( df$x, df$indication, col = "black")


