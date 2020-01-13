
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

library(infer)

set.seed(123)

# Create the data
df_rep_sample <- 
  map(seq(1:1000), function(rep) {tibble(rep = rep, x = seq(1:100), y = 0.5*x + rnorm(100, 15, 3)  )} ) %>% 
  bind_rows()  

library(broom)

many_model <-
  df_rep_sample %>% 
  group_by(rep) %>% 
  nest() %>% 
  mutate( model1 = map(data, function(d) {lm( d$y ~ d$x, data = d)})) %>% 
  mutate( tidy_model1 = map(model1 , ~tidy(.x))) %>% 
  unnest( tidy_model1) %>% 
  filter( ! str_detect(term, "(Intercept)"))


many_model %>% 
  ggplot( ) + aes( x = estimate) %>% geom_histogram()

consistency_ols  <-  function( rep_x_in_sample = 100, rep_data = 100) {
  #create data
  df_rep_sample <- 
    map(seq(1:rep_data), function(rep) {tibble(rep = rep, x = seq(1:rep_x_in_sample), y = 0.5*x + rnorm(rep_x_in_sample, 15, 3)  )} ) %>% 
    bind_rows() 
  
  # do many models - B
  many_model <-
    df_rep_sample %>% 
    group_by(rep) %>% 
    nest() %>% 
    mutate( model1 = map(data, function(d) {lm( d$y ~ d$x, data = d)})) %>% 
    mutate( tidy_model1 = map(model1 , ~tidy(.x))) %>% 
    unnest( tidy_model1) %>% 
    filter( ! str_detect(term, "(Intercept)")) %>% 
    mutate( number_of_rep_data = rep_data) %>% 
    ungroup() %>% 
    dplyr::select( number_of_rep_data, rep, estimate)
  
  return( many_model)
}

consistency_ols( rep_x_in_sample = 10, rep_data = 100)

consistent_ols_many_df <- 
  map(list(10, 50, 100, 1000), function(x) { consistency_ols(rep_x_in_sample = 500, rep_data = x )}) %>% 
  bind_rows()number_of_rep_data


consistent_ols_many_df %>% 
  mutate( number_of_rep_data = str_c( "df samples: ", number_of_rep_data) ) %>% 
  mutate( number_of_rep_data = fct_infreq(number_of_rep_data)) %>% 
  ggplot( ) +
  aes( x = estimate ) +
  geom_histogram( alpha = 0.6) +
  facet_wrap( ~number_of_rep_data) +
  geom_viklinen(  )
lopoi86trfnmbnmjkløæåpiyrewwqaxvbnm,gfdsertyuiopåæ
4
othili







