

# Boostrap example two: Confidens intervals
# Mean of in class midtermscore: https://web.stanford.edu/~rjohari/teaching/notes/226_lecture13_inference.pdf

# Mean = 33.8, n = 108 what is the standard errors?

# From CL-theorem: sample standard errors = st()/n^0.5 = 0.6 , st = 6.25

# DGP
scores <- rnorm( 108, 33.8, sd = 6.25)

# Calcualte
sd(scores)/length(scores)^0.5
lm( data= data.frame(score = scores), score ~ 1) |> summary()

# What does the boostrap give?
n_repa = 10000
n = 108

# N-rep. of mean
boot <- sapply(1:n_repa, \(x) mean( sample(scores, n, replace = T)))
sd(boot) # Close to the true value

# The boot library
mean_boot <- function(data, indices){
    d = data[indices]
    mean(d)
}

obj_boot <- boot::boot(scores, mean_boot, 10000)

plot(obj_boot)
c(obj_boot)
obj_boot$t # The mean parallel univers

