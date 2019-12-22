
# The basic idea, from: https://www.princeton.edu/~otorres/DID101R.pdf

library(tidyverse);library(foreign)

options( scipen = 999)


# Import data -------------------------------------------------------------

# Import the data
data <- foreign::read.dta("http://dss.princeton.edu/training/Panel101.dta") %>%  as_tibble()

#  Data: div. into two time-periods, before and after 1994. Two groups of country is treated and not treated.

# Create variables:
data_did <-
  data %>% 
  mutate( time = ifelse(year >= 1994,1, 0),
          group_exposed_treat =  ifelse( country %in% c("E", "F", "G"), 1, 0),
          did = time*group_exposed_treat) 


# Have a look at the data:
data_did %>% 
  select(country, year, time, group_exposed_treat, did) %>% 
  sample_n( 15) %>% 
  head( n = 15)


data_did %>% names()

data_did %>% 
  lm( y/10^6 ~ time + group_exposed_treat + did , data = .) %>% summary()

# Alternativ:

data_did %>% 
  lm( y/10^6 ~  group_exposed_treat*time , data = .) %>% summary()






# Look at the data: -------------------------------------------------------

data %>% 
  ggplot( ) +
  aes( y = y/10^6 , x = year, fill = country) +
  geom_line( )

data_did %>% 
  group_by( group =  group_exposed_treat, year  ) %>% 
  summarise( y = sum(y, na.rm = T), y_mean = mean(y, na.rm =T)) %>% 
  ungroup( ) %>%   
  ggplot( ) +
  aes( y = y_mean/10^6 , x = year ,fill = factor(group) , color = factor(group)) +
  geom_line(  ) +
  #facet_wrap(~group_exposed_treat)  +
  geom_vline( xintercept = 1994)

data_did %>% 
  mutate( year = as.integer(year)) %>% 
  ggplot( ) +
  aes( y = y/10^6 , x = year ,fill = factor(country) , color = ifelse(group_exposed_treat == 1, "red","blue" )  ) +
  geom_line(  ) +
  #facet_wrap(~group_exposed_treat)  +
  geom_vline( xintercept = 1994) 


gapminder::gapminder %>% 
  filter( country %in% c("Afghanistan", "Albania")) %>% 
  ggplot( ) +
  aes( y = lifeExp, x = year, fill = country )+
  geom_line()
            


