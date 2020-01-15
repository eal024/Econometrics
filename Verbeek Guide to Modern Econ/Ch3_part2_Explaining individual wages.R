



# Data and library --------------------------------------------------------

library(tidyverse);library(haven)

bwages_raw <- read.delim("Verbeek Guide to Modern Econ/data_verbeek/bwages.dat")

# bwages <- read_delim("Verbeek Guide to Modern Econ/data_verbeek/bwages.dat", delim = " ")

bwages <- bwages_raw %>% as_tibble()

# The data:
  # wage before tax hourly wage rate, in euro per hour
  # male 1 if male, 0 if female
  # educ education level, 1 = primary school,
  # 2 = lower vocational training, 3 = intermediate level,
  # 4 = higher vocational training, 5 = university level
  # exper experience in years

# Summary
bwages %>% skimr::skim()
  

# Model1:
model1 <- bwages %>% lm(WAGE ~ EXPER + EDUC + MALE , data = .)

summary(model1)

# How much of the variation does each expl. variables explain?
model1 %>% anova() %>% tidy %>% mutate( expl = sumsq/sum(sumsq))


# model2
model2 <- bwages %>% lm(WAGE ~ EXPER + I(EXPER^2) + EDUC + MALE , data = .)


summary(model2)

# Hetroskedasticity?
model2 %>% augment() %>%
  ggplot( ) + aes( y = .resid, x = .fitted ) + geom_point()

# Clearly!
# One solution is to change the f-form:
GGally::ggpairs(bwages)

# Alternativ model

# log-linear

model3 <- bwages %>% lm( I(log(WAGE)) ~ EDUC + EXPER + I(EXPER^2) + MALE, data = .)

model3 %>% summary


# Model 4:  ---------------------------------------------------------------

bwages %>%
  #filter( EXPER != 0) %>% 
  mutate( EXPER = ifelse(EXPER == 0, 0.001, EXPER)) %>% 
  lm( I(log(WAGE)) ~ I(log(EDUC))  + I(log(EXPER)^2)  +I(log(EXPER))  + MALE, data = .) %>% summary


bwages %>% skimr::skim()

bwages %>% filter(EXPER == 0)

model4 <- bwages %>%
  mutate( EXPER = ifelse(EXPER == 0, 0.001, EXPER), EXPER_2 = EXPER^2, ) %>% 
  lm( I(log(WAGE)) ~ I(log(EDUC))  + LNEXPER  +LNEXPER^2  + MALE, data = .) 


# Test EXPR restriction ---------------------------------------------------
 model4_basic <-
  bwages %>%
  lm( I(log(WAGE)) ~ I(log(EDUC))    + MALE, data = .) %>% summary


R2_4 <- model4 %>% glance() %>% pull(r.squared)

R2_4_basic <- model4_basic %>% glance() %>% pull(r.squared)

F_model4_exper <- ((R2_4 - R2_4_basic)/2)/((1-R2_4)/(1472-5))

# F_stat. with strong rejection.
F_model4_exper




# Dummie for education ----------------------------------------------------


bwages %>%
  mutate( index = row_number()) %>%
  pivot_wider(names_from = EDUC, values_from = EDUC, values_fill = list(EDUC =0) ) %>% 
  janitor::clean_names() %>% 
  lm( log(wage) ~   male  + x2 + x3 + x4 + x5 + lnexper, data = .) %>% 
  summary


bwages %>%
  mutate( EDUC = as.factor(EDUC)) %>%
  lm( log(WAGE) ~ MALE + EDUC + LNEXPER, data = .) %>% 
  summary



# Model with interaction and sloop ----------------------------------------

bwages %>%
  mutate( EDUC = as.factor(EDUC)) %>%
  lm( log(WAGE) ~ MALE*EDUC  + LNEXPER*MALE, data = .) %>% 
  summary

# Educ level with exper
bwages %>%
  mutate( EDUC = as.factor(EDUC)) %>%
  lm( log(WAGE) ~ MALE  + LNEXPER*EDUC, data = .) %>% 
  summary





