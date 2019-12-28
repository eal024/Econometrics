
library(tidyverse);library(haven);library(broom)

options( scipen = 9)

# DID ----------------------------------------------------------------

# Example from Wooldridge 13.2: 
# CPS: Current Population survey - randomly sample households each year -indep. pooled dataset
# Changes to the Return of Education and the Fender Wage gap

cps78_85 <- wooldridge::cps78_85 %>% as_tibble()


cps78_85 %>% names()

cps78_85 %>% 
  group_by( year) %>% 
  summarise( mean_edu = mean(educ, na.rm =T),
             sout_m = sum(south),
             mean_age = mean(age, na.rm = T))



# Model 1
cps78_85 %>% 
  lm( lwage ~ year * educ , data = .) %>% summary


# Model 2
cps78_85 %>% 
  lm( lwage ~ y85*educ + y85*female + union + exper + I(exper^2)   , data = .) %>% summary


# cps78_85 %>% 
#   group_by( year) %>% 
#   nest() %>% 
#   mutate( model = map(data, function(x) { tidy(lm(x$lwage ~ x$educ)) })  ) %>% 
#   unnest( model) %>% 
#   filter( ! str_detect(term, "Inter") )