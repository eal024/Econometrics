
# OLS in R

# librarary and data ------------------------------------------------------
library(tidyverse);library(wooldridge);library(broom)

ceo_sal1 <- wooldridge::ceosal1 %>% as_tibble()

ceo_sal1

# The simple OLS-model: salary ~ reo (average return of equity for the ceo firm)

ceo_sal1 %>% lm(salary ~roe, data = .) %>% summary

ceo_sal1 %>% 
  ggplot( aes(salary , roe)) + geom_point() +
  geom_smooth( method = "lm", formula = y~x , se = F)

# 1 increase in roe, is corr. 18.5 $  increase in salary.
# When the roe = 0, the salary is 963$


# OLS estimator matrix ----------------------------------------------------

salary <-ceo_sal1 %>% pull(salary)

roe <- ceo_sal1 %>% pull(roe)

x <- as.matrix(cbind(1, roe))
y <- as.matrix(salary)

# OLS estimate
beta <- solve(t(x)%*%x)%*%t(x)%*%y


as.matrix(y-beta[1]-beta[2]*x[,2])

# OLS properties ----------------------------------------------------------

# Consistency B_ols = B

df <- tibble( x = seq(1:100), y = 10  + x + rnorm(100,0,15))
df %>% lm(y~x, data = .) %>% summary
df %>% ggplot( aes(y = y, x)) + geom_point() 

# simulate:

vec <- c()
xvec <- c()
yvec <- c()

for(i in 1:500) {
  xvec[i] <- i
  yvec[i] <- 10 + i + rnorm(10, 0, 15);
  
  df <- tibble(y = yvec, x = xvec)
  
  lm <- lm(y~x, data = df)
    
  vec[i] <- lm$coeff[2]
  
}

df %>% lm(y~x, data = .) %>% summary 

# The figure shows how the OLS estimator converge tow. the true value 
# 1
tibble(x = 1:500, y = vec) %>% 
  ggplot( aes(x = x, y = y)) + geom_line()



# Asymptotic normality and inference --------------------------------------










