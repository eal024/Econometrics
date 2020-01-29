
# OLS
options( scipen = 999)

# 1 a) Illustrate the law of lage number for the estimator of p ~Bernouilli(p), p = 0.6

# Bernoulli , p(x) = p(x)^x*(1-p)^(1-x)
# E(x) = P
# Var()
rbinom(n = 100, size = 1, prob = 0.5  )


df <- tibble( random_x_bernoulli  = rbinom(n = 1000, size = 1, prob = 0.6  ) ) %>% 
  mutate( index = row_number(), 
          cumsum = cumsum(random_x_bernoulli)/index )

# Check if the stats.  
map_dbl( df, compose( as_mapper(~.x, format(.x, digits = 2, scientific = NA)) ,mean))


# Graph to see if the sim is correct.
df %>% 
  ggplot( ) +  aes( x = index , y = cumsum) + geom_point()

# b) Chi-distribution
rchisq(seq(1:100), df = 1, ncp = 0)

hist(rchisq(seq(1:100), df = 1, ncp = 0))

# Many chi-distriution
df <- tibble( n = seq(from = 1, to = 10000, length.out = 9))

df_chi <-
  df %>% 
  group_by( n ) %>% 
  nest() %>% 
  mutate( df = map(n , function(x) {tibble( N = seq(1:n) )} ),
          df_chi = map(df, function(x) {x %>%  mutate( chi = rchisq(n = N, df = 1, ncp = 0)) }  ))

df_chi %>% 
  unnest(df_chi) %>% 
  select( n, N, chi) %>% 
  ggplot( aes( x = chi )) + geom_histogram() + facet_wrap(~n)


# 2 -----------------------------------------------------------------------

# a) Random sample N = 400, y = a + bx +e = XB + E

df <- tibble( x = runif(400, min = 0, max = 1), y = 1 + 1*x + rnorm(length(x), mean = 0, sd = 2) )


df %>% 
  lm( y ~ x, data = .) %>% broom::tidy() %>%
  mutate( min = estimate - std.error*1.96,
          max = estimate + std.error*1.96) %>% 
  ggplot( aes(term , y = estimate) ) +
  geom_point( shape = 4, size = 4) +
  geom_errorbar( aes(ymin = min, ymax = max) ) +
  geom_hline(yintercept = 0, linetype = 2)

# test b1 != 1


modelA <-  df %>% 
  lm( y ~ (x), data = .)

modelA %>% 
  broom::tidy() %>% 
  filter( ! str_detect(term, "Intercept")) %>% 
  mutate( test_diff = (estimate-1)/std.error,
          reject_h0 = ifelse(abs(test_diff) > 1.96, "Reject", "Not reject" ))

# c) using the Wald-test
modelA %>% 
  augment() %>% 
  summarise( n  = n(),
             sse = (sum(.resid^2))/(n-1),
             sigma =   (sse)^0.5 ) 

modelA %>% anova() %>% tidy
modelA %>% glance()

# variance

x_x_2 <- df %>% mutate( x_meanX_2 = (x-mean(x))^2 ) %>% 
  summarise( X_X_2 = sum(x_meanX_2)) %>% pull(X_X_2)

## For var_a
x_x_2_a <- df %>% mutate( x_2 = x^2, x_meanX_2 = (x-mean(x))^2 ) %>% 
  summarise( X_X_2 = sum(x_meanX_2), sum_x = sum(x_2)) 

var_b <- 4.43/x_x_2

var_b^0.5

# Look at the sum_stat
x_x_2_a
var_a <- (4.43*129/(400*33.1))

# Correct!
var_a^0.5


# c)
df %>% summarise( x_ = -mean(x), x_x2 = sum( (x-mean(x))^2) )

cov_a_b <- -(-0.5/33)*2.11

var_A_B <- var_a + var_b + 2*cov_a_b

var_A_B^0.5


modelA %>% summary




# Power -------------------------------------------------------------------

# Power = 1 - Pr(Type 2-error)  


alpha <- 0.05
p <- 0.5
sigma <- 1
N <- 34

# Power
k <- 1 - qnorm()


(2*(qnorm(.2) - qnorm(0.975))/1)^2


# Estimate N
N <- ((2*(qnorm(1-0.8)-1.96) )/1)^2

# Estimate K:

k <- 1 - dnorm(1.96 - (1/( (1/(p*(1-p)) )^0.5*(1/N)^0.5) ) ) 


1 - dnorm(1.96-0.5*(N^0.5*1))
# The inverste normal.distribution
ggplot( ) + aes( y = qnorm(seq(from = 0, to = 1, length.out = 50)), x = seq(from = 0, to = 1, length.out = 50)  ) + geom_point()


rct_sample_size <- function(N, sigma_m, p = 0.5) {
  
  se_b <-  ( (1/(p*(1-p)) )^0.5*(1/N)^0.5) 
  power <- 1 - pnorm(1.96 - (sigma_m/( (1/(p*(1-p)) )^0.5*(1/N)^0.5) ) ) 
  
    
  return(power)
}

rct_sample_size(31.29, sigma_m = 1, p = 0.5)

#
tibble( N  = seq(from = 10, to = 4000, by = 1) ,
        powers_.1 = rct_sample_size(N,sigma_m = .1),
        powers_.5 = rct_sample_size(N,sigma_m = .5),
        powers_1 = rct_sample_size(N,sigma_m = 1)
        ) %>%
  pivot_longer( names_to = "power", values_to = "value", powers_.1:powers_1) %>% 
  separate( power, sep = "_", into = c("power", "power_value")) %>% 
  select(-power) %>% 
  ggplot( ) +
  aes( x = N, y = value, fill = power_value, color = power_value) +
  geom_line() +
  geom_hline( yintercept = 0.8, linetype = 3) +
  geom_vline( xintercept =3140, linetype =3, color = "red") +
  scale_x_log10()




# Exercise 2 -Gen  and linear regression ----------------------------------

# Generate data
df <- tibble( x = runif(400, min = 0, max = 1), y = 1 + 1*x + rnorm(length(x), 0, 2) )


df %>% 
  lm(y ~ x, data = .) %>% summary

# Test
df %>% 
  lm(y ~ 0 + I(1+ x), data = .) %>% summary






  