
# Example from: https://users.aalto.fi/~ave/ROS.pdf (page 75)
# remotes::install_github("avehtari/ROS-Examples",subdir = "rosdata")

# want to estaimate the ratio between men and womens earnings.

# data
earnings <- read.csv("data/earnings.csv")

earn <- earnings$earn
male <- earnings$male

# Illustration: One single bootstrap
n <- nrow(earnings) # Boostrap is don with replacemnet, but the data need to be same size as the sample
boot <- sample(n, replace = T) 
earning_boot <- earn[boot]
male_boot <- male[boot]
ratio <- median(earning_boot[male_boot == 0])/median(earning_boot[male_boot == 1])

# Boostrap is to repeat several times
boot_ratio <- function(data){
    
    n <- nrow(data) 
    boot <- sample(n, replace = T) 
    earning_boot <- data$earn[boot]
    male_boot <- data$male[boot]
    
    median(earning_boot[male_boot == 0])/median(earning_boot[male_boot == 1])
}

boot_ratio(data = earnings) # Test

# A set of boostrap simulations
n_sim <- 1000

output <- replicate(n_sim, boot_ratio( data = earnings) )

hist(output)  # Distribution 
sd(output)

ratio # mean
sd(output) # With sd of 0.03





