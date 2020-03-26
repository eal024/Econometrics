


# Library and data --------------------------------------------------------

library(tidyverse)
library(AER)
library(broom)

angev98 <- haven::read_dta("Seminar/RawData/angev98.dta")


angev98 %>% 
  map_df(function(x) {mean(x, na.rm = T)})

skimr::skim(angev98 %>% select(incomem, workedm, hourswm, morekids, ageqk))

angev98 %>% select(incomem, workedm, hourswm, morekids) %>% summarise( mean = mean(incomem, na.rm = T), median = median(incomem, na.rm = T))

table(cut(angev98$incomem, 10) )

angev98 %>% filter(incomem < 100000) %>% ggplot( aes(incomem, fill = factor(morekids))) + geom_histogram( position = "fill")

angev98 %>% names()

# a)
angev98 %>%  
  lm( workedm ~ morekids, data = .) %>% summary()

# 1.2
angev98 %>% 
  select(workedm, morekids , ageqk ,agem, agefstm, ageq2nd, boy1st, boy1st, boy2nd, blackm, hispm) %>% 
  estimatr::lm_robust( workedm ~ ., data = .) %>% 
  summary()

angev98 %>% 
  select(workedm, morekids ,agem, agefstm, ageq2nd, boy1st, boy1st, boy2nd, blackm, hispm) %>% 
  lm_( workedm ~ ., data = .) %>% 
  summary()


cov_m_a <- ((-.12089+.1705)/0.0066)*0.4903^2

p_a_m <- (cov_m_a/(0.49^0.5*14.11^2))

p_a_m

m <- angev98$morekids
a <- angev98$ageqk

mat <- cbind(m,a) %>% cov()

mat


# c) mix -gender

# samesex
angev98 %>% 
  lm( kidcount ~ samesex, data = .) %>% summary()
  
t <- 
  angev98 %>% 
  mutate( more_than_two = ifelse(kidcount > 2, T, F))

samesex_tabel <- (table(t$more_than_two, t$samesex)) 

prop.table(samesex_tabel) 

angev98 %>% 
  mutate( more_than_two = ifelse(kidcount > 2, T, F)) %>% 
  group_by( more_than_two,samesex) %>% 
  summarise( n = n()) %>% 
  pivot_wider( names_from = samesex, values_from = n)

# OLS


#f ) Wald estimate

# E[y|z = 1]
angev98 %>% summarise(workedm_z_1  = mean(workedm[samesex == 1]), 
                      workedm_z_0  = mean(workedm[samesex == 0]),
                      morekids_z_1 = mean(morekids[samesex==1]),
                      morekids_z_0 = mean(morekids[samesex==0]) ,
                      b_wald = ((workedm_z_1-workedm_z_0)/(morekids_z_1-morekids_z_0) ) )

# ii) First stage 
angev98 %>% estimatr::lm_robust( morekids ~ samesex, data = .)

angev98 %>% lm( workedm ~ samesex, data = .)


# iii) 2SLS by hand:
angev98 %>% 
  lm(morekids ~samesex, data = .) %>% 
  augment() %>% 
  mutate( index = row_number(), .fitted) %>% 
  select( index, .fitted) %>% 
  right_join( angev98 %>% mutate( index = row_number()), by = "index" ) %>% 
  lm( workedm ~ .fitted, data = .) %>% 
  summary()

# iv)

AER::ivreg(  workedm ~morekids + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm | samesex + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm, data = angev98) %>% 
  summary()


lm(  workedm ~morekids + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm , data = angev98) %>% 
  summary()


# g) 
# The short model, t - below 1 -> cant conclude that there is endogenity
angev98 %>%
  lm( morekids ~ samesex, data = .) %>% augment() %>% 
  mutate( index = row_number()) %>% select(index, .resid) %>%  right_join(angev98 %>% mutate(index = row_number() ) , by = "index") %>% 
  lm( workedm ~ morekids + .resid, data = . ) %>% 
  summary()

# The extended model - can reject H0, cov(x,u) = 0.
angev98 %>%
  lm( morekids ~ samesex + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm, data = .) %>% augment() %>% 
  mutate( index = row_number()) %>% select(index, .resid) %>%  right_join(angev98 %>% mutate(index = row_number() ) , by = "index") %>% 
  lm( workedm ~ morekids + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm + .resid, data = . ) %>% 
  summary()



# h) Twin birth as IV, instead of samesex 
angev98 %>% 
  select(workedm, morekids, samesex, twins2) %>% map_df( function(x) {mean(x, na.rm = T)})

angev98 %>% 
  count( twins2)

# first stage
rf_twins <- angev98 %>% select(workedm, morekids, samesex, twins2) %>%  lm(morekids ~twins2, data = .)

# 2SLS
rf_twins %>% augment() %>% mutate( index = row_number()) %>%  select(index, .fitted) %>% right_join( angev98 %>% mutate(index = row_number()), by = "index" ) %>% 
  select( workedm, .fitted) %>% 
  lm( workedm ~ .fitted, data = .) %>% 
  summary()

# Long model
AER::ivreg(  workedm ~morekids + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm | twins2 + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm, data = angev98) %>% 
  summary()



## Hausman

# 1.stage 
rf_twins_long <- angev98 %>%  lm(morekids ~twins2 + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm, data = .)


rf_twins_long %>% augment() %>% 
  mutate( index = row_number()) %>% 
  select( index, .resid) %>% 
  right_join( angev98 %>% mutate(index = row_number()), by = "index") %>% 
  lm( workedm ~morekids + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm + .resid, data = .) %>% 
  summary()


## Sargan

iv_reg_2IV <- AER::ivreg( workedm ~ morekids + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm | 
                            twins2 + samesex + agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm,
                          data = angev98)


model_v_sargan <- 
  iv_reg_2IV %>% augment() %>% 
  mutate( index  = row_number()) %>% 
  select(index, .resid) %>% 
  right_join( angev98 %>% mutate(index = row_number() ), by = "index") %>% 
  lm( .resid ~ agem + agefstm + ageqk + ageq2nd + boy1st +  boy2nd + blackm + hispm + twins2 + samesex, data = .)

model_v_sargan %>% summary()

test_sargan <- model_v_sargan %>% glance() %>% pull(r.squared)*nrow(angev98)

1-pchisq(test_sargan,1)
# P-value 14. can not rejct cov(z2,u) = 0. Shold be included.



# Marrided vs unmarried: --------------------------------------------------


angev98 %>% 
  lm( workedm ~ morekids + msample, data = .) %>% summary()


