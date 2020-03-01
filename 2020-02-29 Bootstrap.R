



# Library and data --------------------------------------------------------

library(tidyverse)

# a) Illustrate the LLN mean of Bernouilli(p)-numbers

df <- tibble( rbern = Rlab::rbern(10000, prob = 0.6), index = c(1:length(rbern) ) )
              

df %>% mutate( cummean = cumsum(rbern)/index ) %>%
  ggplot( aes(x = index, y = cummean)) + 
  geom_line() +
  geom_hline( yintercept = 0.6, linetype = 2)


# b) chi-distribution. Ilstrate the Central limt theorem of k

# The 95th percentile of the Chi-Sq. distrib. with  degre. of freedom  equal 1
qchisq(.95, df = 1)

# Generate Chi data
rchisq(n = 1000, df = 1, ncp = 0 )

k <- 1

df <- tibble( chi_x = rchisq(n = 10, df = k, ncp = 0 ))

df <- tibble( N = c(1,5,10,50,100,500,1000) , index = c(1:length(N))) %>% select(index, N)

df %>%  
  #mutate( data = map(N, function(x) {chi = rchisq(n = x, df = 1, ncp = 0) } )) %>% 
  mutate( data_many = map(N, function(x) {map(1:100, function(y) {chi = rchisq(n = x, df = 1, ncp = 0)})})) -> df


df %>% mutate( mean_each = map(data_many, function(x) {map(x, function(x) {mean(x)})})) -> se

se %>% 
  unnest(data_many) %>% unnest(mean_each) %>% unnest(mean_each) %>% 
  select(index, N, mean_each) -> sim_data



sim_data %>% 
  ggplot( aes(mean_each) ) + geom_histogram() + geom_vline(xintercept = 1) + facet_wrap(~N, scales = "free_y" )

