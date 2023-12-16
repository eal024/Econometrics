


# Example 1 Card 1995

library(Ecdat)
library(tidyverse);library(magrittr)

schooling_raw <- Ecdat::Schooling %>% as_tibble()

?Ecdat::Schooling

# The ols model
schooling_raw %>% 
  select( ed76, wage76, black, exp76 , smsa66, south66) %>% 
  mutate( exp76_2 = exp76^2) %>% 
  lm( log(wage76) ~ ., data = .) %>% 
  summary()


# the reduced form model , predication of schooling
schooling_raw %>% 
  select( ed76, -wage76, black, -exp76 , smsa66, south66, age76, nearc4) %>% 
  mutate(  age_2 = age76^2) %>%
  lm( ed76 ~., data =.) %>% 
  summary()


# IV -estimate
library(AER)

schooling_raw %>% 
  select( ed76, wage76, black, exp76 , smsa66, south66,nearc4, age76) %>% 
  mutate( exp76_2 = exp76^2) %>% 
  ivreg( log(wage76) ~ ed76 + exp76 + exp76_2 + smsa66 + south66  + black | nearc4  + black + age76 + I(age76^2) +smsa66 + south66  , data = .) %>% 
  summary()




# Verbeek (2012) Exercises 5.2 -------------------------------------------- x
# SCHOOLING. The purpose of this 
# exercise is to explore the role of parents’ education as instruments to estimate the
# returns to schooling.

schooling_raw %>% 
  skimr::skim()

# a) RF - schooling 
schooling_raw %>% 
  # Exluce nearc4
  mutate( age_2 = age76^2) %>% 
  select( ed76, age_2 ,age76, black, smsa76, south76 ,momed, daded ) %>%  
  lm( ed76 ~ ., data = .) %>% summary()


# b) momed and daded - as IV for schooling. age and age2 IV for experience.
model_iv <- 
  schooling_raw %>% 
  select( ed76, wage76, black, exp76 , smsa76, south76, age76, momed, daded ) %>% 
  mutate( exp76_2 = exp76^2) %>% 
  ivreg( log(wage76) ~ ed76 + exp76 + exp76_2 + smsa76 + south76  + black | momed + daded  + black + age76 + I(age76^2) +smsa76 + south76  , data = .) 


# c) Test over-id -> Sargan
schooling_raw$residu <- model_iv$residuals

aux_reg_sargan <- 
  schooling_raw %>% 
  # On all exogen variables
  lm( residu ~ age76 + I(age76^2) + smsa76 + south76  + black + momed + daded , data = .)


aux_reg_sargan %>% summary()

chi_test_stat <- summary(aux_reg_sargan)$r.squared*nrow(schooling_raw)

p_val <- 1- pchisq(chi_test_stat, 1)

p_val 

# can not reject cov(z, u) = 0

# d) Re-estimate model with near college dummy
model_iv_pedu_livenear <- 
  schooling_raw %>% 
  ivreg( log(wage76) ~ ed76 + exp76 + I(exp76^2) + smsa76 + south76  + black | momed + daded + nearc4  + black + age76 + I(age76^2) +smsa76 + south76  ,
         data = .) 


schooling_raw$residu <- model_iv_pedu_livenear$residuals

aux_reg_sargan <- 
  schooling_raw %>% 
  # On all exogen variables
  lm( residu ~ age76 + I(age76^2) + smsa76 + south76  + black + momed + daded + nearc4 , data = .)

chi_test_stat <- summary(aux_reg_sargan)$r.squared*nrow(schooling_raw)

p_val <- 1- pchisq(chi_test_stat, 1)

p_val 


# d) 
model_types <- tibble( model_index = c(1:3)) 

model_ols <- function(x) {lm( x$lwage76 ~ x$ed76 + x$black + x$exp76 + I(x$exp76^2)  + x$smsa66 + x$south66, data = x) }
model_iv <- function(x) { AER::ivreg( x$lwage76 ~ x$ed76 + x$exp76 + I(x$exp76^2) + x$smsa76 + x$south76  + x$black | x$momed + x$daded + x$black + x$age76 + I(x$age76^2) +x$smsa76 + x$south76  , data = x)  }
model_iv_5_4 <- function(x) { AER::ivreg( x$lwage76 ~ x$ed76 + x$exp76 + I(x$exp76^2) + x$smsa76 + x$south76  + x$black | x$nearc4 + x$black + x$age76 + I(x$age76^2) +x$smsa76 + x$south76  , data = x)  }
model_iv_w_near <- function(x) { AER::ivreg( x$lwage76 ~ x$ed76 + x$exp76 + I(x$exp76^2) + x$smsa76 + x$south76  + x$black | x$momed + x$nearc4 + x$daded + x$black + x$age76 + I(x$age76^2) +x$smsa76 + x$south76  , data = x)  }

model_types %>% 
  mutate( data = list(schooling_raw)) %>% 
  mutate( model_iv = map2(data,model_index, function(x,y) {ifelse(y==1, model_iv(x), model_iv_w_near(x)) })) %>% 
  mutate( model_ols = map2(data, model_index ,function(x,y) {ifelse(y==3, model_ols(x), NA) })) %>% 
  mutate( model_tible = map2(model_ols, model_index , function(x,y) {ifelse(y == 3, tidy(x), NA)} ))



model_ols(schooling_raw) %>% summary()
model_iv_5_4(schooling_raw) %>% summary()
model_iv(schooling_raw) %>% summary()
model_iv_w_near(schooling_raw) %>% summary()
