
# Density estimation, at point X
# Example with Epanechnikov Kernel

# Example data
vec <- c(-1.75, -0.93, 0.10, 0.24, -0.09)

# Transform to u, with bandwidth h
h <- 2
u <- (vec-(-2))/h

# u as fuciton
fn_u <- function(x, point, band.width) { (x-(point))/band.width }

fn_u( x = vec[1], point = -2, band.width = 2)

# Density estimation, at pint -2. The Kernel is Epanechinikov = (3/4)*(1-u^2)
# only u < indicator is included 

estimation <- function(x, point, b.w, indicator){

    u = fn_u(x, point, b.w)  # U
    u = u[ abs(u) < indicator] # Only u < indicator is included

    n = length(x)


    (1/(b.w*n))*sum( (3/4)*(1-u^2) )
} 


## Calcuate the density at point -2
estimation(x = vec, point = -2, b.w = 2, indicator = 1)
estimation(x = vec, point = -2, b.w = 1, indicator = 1)

# Calculate the density at each integer from -2 to 2
at_point <- seq(from = -2, to = 2, by = .2)
with_bandwidth2 <- sapply( at_point, \(p) estimation(x = vec, point = p, b.w = 2, indicator = 1))
with_bandwidth1 <- sapply( at_point, \(p) estimation(x = vec, point = p, b.w = 1, indicator = 1))

plot(  at_point,with_bandwidth1, type = "l", col = "blue"  )
lines( at_point,with_bandwidth2, col = "red")






density <- density(
    x,
    bw= 2,
    kernel= c("epanechnikov"),
    from=-2,
    to=2
    )

plot(density)


# Calcuate the density manually.

point_at_density <- function( n, h, u, type = c("epanechnikov"), indicator){

    # u in each point that satisf. condtion
    u <- u[ abs(u) < indicator] 

    # u in the Kernel
    if(type == "epanechnikov"){ 
        k = (3/4)*(1-u^2) 
        }
    
    # return the Kernel point
    sum(k)/(n*h)

}

# Transform the vector to u
u <- function(z, zi, h){ (z-zi)/h }

# Example
s <- u(zi = x, z = -2, h = 2)

# Testing the density at point 2
point_at_density( 
    n = length(x),
    h = 2,
    u = u( zi = x, z = -2, h = 2),
    indicator = 1
    )


# Calculation density at point Z
Z <- c(-2:2)

lapply( Z, \(x) point_at_density(h = length(x), h = 2,     u = u( zi = x, z = -2, h = 2),indicator = 1 )) 