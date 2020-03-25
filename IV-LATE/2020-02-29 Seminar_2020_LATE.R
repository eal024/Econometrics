

# Data and library
library(tidyverse)
library(broom)
library(estimatr) 

lottery <- haven::read_dta("Seminar/RawData/lottery.dta")


# Z and Y --------------------------------------------------------

lottery %>% 
  group_by(d) %>% 
  summarise( n = n(),
             y = mean(lnw)) 


  # First stage:
rf_stage1_model1 <- lottery %>% lm_robust( d ~ z , data = .)

rf_stage1_model1 %>% summary()

help_augment(model = rf_stage1_model1, data =  lottery) %>% 
  select( index, fitted) %>% 
  right_join( lottery %>% mutate(index = row_number()), by = "index") %>% 
  lm( lnw ~ fitted, data = .) %>% 
  summary()


lottery %>% estimatr::iv_robust( lnw ~ d | z, data =  .)



# Count number compliers:
lottery %>% 
  group_by(z) %>% 
  count(d) %>% 
  ungroup() %>% 
  summarise( a = sum(n[d == 1 & z == 0])/sum(n[z==0]),
             n = sum(n[d == 0 & z == 1])/sum(n[z==1]),
             c = 1- a - n) %>% 
  mutate( stat = "P") %>% 
  add_row( stat = "N", a = 0.41*1476, n = 0.0696*1476, c = 0.52*1476) %>% 
  select( stat, a, n,c) %>% 
  rowwise() %>% 
  mutate( total = sum(a+c+n)) -> desc_anc


prob_c <- desc_anc %>% filter(stat == "P") %>% pull(c)
  
# Describe the Compliers
  # Size allready counted
  # Gender

lottery %>% 
  group_by(z) %>% 
  add_count( z_obs = n()) %>% 
  group_by(z_obs,female,z, d) %>% 
  summarise(n = n() ) 

lottery %>% skimr::skim()

lottery %>%   count(female)
  

# mean Y1 and mean Y0 for compliers
lottery %>%
  group_by(z,d) %>% 
  summarise( lnw = sum(lnw), n = n() ) %>% 
  ungroup() %>%
  mutate( y = lnw/n) %>% 
  group_by(z) %>% 
  summarise( y = sum(lnw)/sum(n),
             d = sum(d*n)/sum(n)) %>% 
  summarise( (y[z==1]-y[z==0])/(d[z==1]-d[z==0] ) )



  


  
  
# lottery %>% 
#   group_by(d,z) %>% 
#   summarise( y = sum(lnw), n = n() ) %>% 
#   mutate( p = n/sum(n))
# 
# (1-0.209-0.165)*nrow(lottery)


# d) E[y1] and E[y0] complieres
lottery %>% 
  mutate( y_1 = ifelse( z == 1, lnw*d, 0), y_0 = ifelse( z == 0, lnw*d,0 )) %>% 
  mutate( d_1 = ifelse( z == 1, d, 0), d_0 = ifelse( z == 0, d,0 )) %>% 
  summarise_at( vars(y_1:d_0), sum)
  

lottery %>% 
  mutate( y_1 = ifelse( z == 1, lnw*(1-d), 0), y_0 = ifelse( z == 0, lnw*(1-d),0 )) %>% 
  mutate( d_1 = ifelse( z == 1, (1-d), 0), d_0 = ifelse( z == 0, (1-d),0 )) %>% 
  summarise_at( vars(y_1:d_0), sum)

lottery %>% 
  mutate( y_1 = ifelse( z == 1, lnw*d, 0), y_0 = ifelse( z == 0, lnw*d,0 )) %>% 
  mutate( d_1 = ifelse( z == 1, d, 0), d_0 = ifelse( z == 0, d,0 )) %>% 
  mutate( y = (y_1-y_0)/(d_1-d_0)) %>% 
  lm( y ~ z , data = .) %>% summary()
  





















 
# lottery %>% map_df( function(x) {mean(x)} )
# 
# 
# lottery %>% add_count( ) %>% group_by( n, lotcateg) %>% 
#   summarise( obs = n()) %>%
#   mutate( freq = obs/n)  

# Compare C and 



help_augment <- function(model, data) {
  
  outcome <- model$outcome
  terms <- model$term 
  terms <- terms[!str_detect(terms, "(Intercept)")] 
  
  fitted <- model$fitted.values
  resid <- model$df.residual
  
  df <- data %>% select(outcome, terms)
  df$fitted <- fitted
  resid <- df[1]-df$fitted
  
  df["resid"] <- resid
  %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% 