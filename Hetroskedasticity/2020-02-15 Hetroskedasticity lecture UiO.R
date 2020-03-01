
# Part 1: Hetroskedasticity - WLS and Robust standard errors (Whit --------

library(tidyverse)

# Data
hprice1 <- wooldridge::hprice1 %>% as_tibble()


hprice1 <- foreign::read.dta("http://fmwww.bc.edu/ec-p/data/wooldridge/hprice1.dta") 


hprice1 <- as_tibble(hprice1)

hprice1 %>% 
  lm( price ~lotsize + sqrft, data = .) %>% 
  summary

# With robust standard error
hprice1 %>% 
  estimatr::lm_robust( price ~lotsize + sqrft, se_type = "HC1"
                       , data = .) %>%
  summary

# t-value decreased from 11 to 7.9 and 3 to 1.75.



# White-test of hetro -----------------------------------------------------

model2 <- hprice1 %>% 
  lm( price ~lotsize + sqrft + bdrms, data = .) 


options(scipen = 999)

model2 %>% broom::augment() %>% mutate(fitted_u_2 = .resid^2) %>% 
  lm(fitted_u_2 ~  lotsize + sqrft + bdrms ,data = .) %>% 
  summary

broom::augment(model1) %>% mutate(fitted_u_2 = .resid^2) %>% 
  lm(fitted_u_2 ~  lotsize + sqrft  ,data = .) %>% 
  summary

# White-test
lmtest::bptest(model1)




# Bootstrap in R ----------------------------------------------------------

get_data <- function(obs = 10) {
  tibble(x = rnorm(obs, mean = 1, sd = 0.5),
         y = 3 + x * 1 + rnorm(length(x), mean = 0, sd = 1))
}

tibble( index = 1:100, obs = 100) %>%
  group_by(index ) %>% 
  mutate( data = map(obs, function(x) {get_data(obs = obs)})) %>% 
  mutate( model = map(data, function(data) {lm(y ~ x, data = data)})) %>% 
  mutate( tidy_models = map(model, function(x) {summary(x) %>% broom::tidy()} ) ) %>% 
  unnest(tidy_models) %>% 
  filter( term != "(Intercept)") %>% 
  ungroup() %>% 
  summarise( mean_estimate = mean(estimate))



## generate data, with replacement

generate_data <- function( ) {
  sample_n(hprice1, replace = T, size = 88)
}


model <- function(x){  lm( lprice ~llotsize + sqrft , data = x)}

tibble( index = 1:4) %>% 
  group_by(index) %>% 
  mutate( data = map(index, function(x){ sample_n(hprice1, replace = T, size = 88)} )) %>% 
  mutate( model = map( data, model )) %>% 
  mutate( tidy_model = map(model, summary) ) %>% 
  mutate( tidy_model = map(tidy_model, broom::tidy)) %>% 
  unnest( tidy_model) %>% 
  filter( ! str_detect(term, "(Intercept)")) %>% 
  ungroup() %>% 
  group_by(term) %>% 
  mutate( var_estimate = (estimate - mean(estimate))^2) %>% 
  summarise(se_calc_step =  sqrt((1/(4-1))*sum(var_estimate) ) )   %>% 
  mutate( size =  4)



the_bootstrap <- function(size = 4) {
  se_bootstrap <-
  tibble( index = 1:size) %>% 
    group_by(index) %>% 
    mutate( data = map(index, function(x){ sample_n(hprice1, replace = T, size = 88)} )) %>% 
    mutate( model = map( data, model )) %>% 
    mutate( tidy_model = map(model, summary) ) %>% 
    mutate( tidy_model = map(tidy_model, broom::tidy)) %>% 
    unnest( tidy_model) %>% 
    #filter( ! str_detect(term, "(Intercept)")) %>% 
    ungroup() %>% 
    group_by(term) %>% 
    mutate( var_estimate = (estimate - mean(estimate))^2) %>% 
    summarise(bootstrap_std =  sqrt( (1/(size-1))*sum(var_estimate) ) )   %>% 
    mutate( mc_size =  size)
  
  coef_obs <- hprice1 %>% lm(lprice ~ sqrft + llotsize , data = . ) %>% broom::tidy() %>% 
    select( term, estimate) %>% 
    left_join( se_bootstrap, by = "term")
  
  coef_obs %>% 
    mutate( z = estimate/bootstrap_std)
  
  
}



the_bootstrap(10)
the_bootstrap(199)
the_bootstrap(1000)
the_bootstrap(10000)
the_bootstrap(100000)










