
# https://www.analyticsvidhya.com/blog/2018/07/introductory-guide-maximum-likelihood-estimation-case-study-r/

# Determine coeff. of a model with any kind of distribution

library(tidyverse);library(lubridate)
theme_set(theme_light())

tickets <- read_csv("train_tickets.csv") 

df <- tickets %>% 
  mutate(dd = as.character(Datetime),
         index = row_number()) %>%
  select(-Datetime)

dd <- vector();
count <- vector();
day <- vector();
j <- 0
for(i in 1:length(df$index)) {   
  if(df$dd[i] == "00:00:00"){
    j = j + 1
    }
  day[i] <- j 
  dd <- df$dd
  count <- df$Count
}



df_ticket <- tibble( day = day, hour = map_chr(dd, function(x) {str_sub(x, 1,2)}) ,dd = dd, count = count) 

df_ticket %>% 
  group_by(hour) %>%
  count(count) %>%
  ggplot( aes( x = n )) + geom_histogram(aes( y = ..density..), bin = 80) +
  geom_density( color = "red", size = 1.1) 
  #scale_x_continuous( limits = c(0,500)) +
  #scale_x_log10()
  
# -------------------------------------------------------------------------

x <- as_tibble( x = rnorm(50,50,10))
# What is the mean and sd. of the data observed in histogram? 
x %>% 
  ggplot( aes(x = value)) + geom_dotplot()

# 60, 40? A method to get u and sigma that discribe the data (that we assume is normal distributed), can be estimated with MLE

# distribution parameters: It can be regarded as a numerical characteristic of a population or a statistical model. We can understand it by the following diagram

len <- 200
df <- tibble( x = rnorm(len,50,10), y = rnorm(len,10,10), z = rnorm(len,10,10), index = seq(1:len) )

df %>%
  pivot_longer(names_to = "var", values_to = "value", x:z) %>% 
  ggplot() + 
    aes( x = value , fill = var ) +
    geom_density( alpha = 0.3)

# Above we know the mean and se. In reell life the problem is iverse. We see the data, but dont know mean and se.
  # Given the observed data and a model of interest, 
  # we need to find the one Probability Density Function/Probability Mass Function (f(x|θ)), 
  # among all the probability densities that are most likely to have produced the data.



# MLE using R -------------------------------------------------------------

# case: It has the count of tickets sold in each hour from 25th Aug 2012 to 25th Sep 2014  (about 18K records). 
  #Our aim is to predict the number of tickets sold in each hour.

df_ticket %>% 
  count(count)

# This could be treated as a Poisson distribution or we could even try fitting an exponential distribution.

df_ticket %>% 
  mutate( index = str_c(day,"-",hour), index = as.factor(index)) %>% 
  group_by(day) %>%
  summarise( ticket_sold = sum(count)) -> sum_df_ticket

sum_df_ticket <- sum_df_ticket %>% 
  mutate(  day = seq(from = ymd("2012-01-01"), by = "day" ,length.out = nrow(sum_df_ticket))) 

sum_df_ticket %>% 
  ggplot( aes(x = day, y = ticket_sold) ) + 
  geom_point()

sum_df_ticket <- sum_df_ticket %>% mutate( age = row_number() )

sum_df_ticket %>% lm( log(ticket_sold) ~age, data = .) %>% summary()
sum_df_ticket %>% glm( log(ticket_sold) ~age, data = ., family = "poisson") %>% summary()
  
sum_df_ticket %>% ggplot( aes( y = ticket_sold, x = age)) + geom_smooth(method = "lm", se = F) + geom_point()

# -------------------------------------------------------------------------

















