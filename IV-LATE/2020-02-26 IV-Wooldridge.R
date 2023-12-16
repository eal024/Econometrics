


# libraray  ---------------------------------------------------------------

library(tidyverse);library(broom)


mroz <- wooldridge::mroz %>% as_tibble()

mroz %>% skimr::skim()

mroz_iv_ex <- 
  mroz %>% 
  filter(wage > 0) %>% 
  select(wage, educ, exper, expersq, motheduc, fatheduc,huseduc) %>% 
  mutate( index = row_number())


# OLS
mroz_iv_ex %>% lm(log(wage) ~ educ + exper + expersq, data = .) %>% summary()

# First stage
rf_model <-mroz_iv_ex %>% 
  lm(educ ~ motheduc + fatheduc + exper + expersq, data = .)

# 2SLS
augment(rf_model) %>% 
  mutate( index = row_number()) %>% 
  select(.fitted, index) %>% 
  right_join( mroz_iv_ex, by = "index") %>%
  rename( hat_edu = .fitted) %>% 
  lm( log(wage) ~ hat_edu + exper + expersq , data = .) %>% 
  summary()

# iv-reg-packages
AER::ivreg(formula = log(wage) ~ educ + exper + expersq | motheduc + fatheduc + exper + expersq, data = mroz_iv_ex) %>% summary()

mroz_iv_ex %>% iv_robust(formula = log(wage) ~ educ + exper + expersq | motheduc + fatheduc + exper + expersq, data = .) %>% tidy() %>% as_tibble()
      


## Testing for endogeneity

# 1) calculate resid
rf_resid <- rf_model %>% augment() %>% mutate( index = row_number()) %>% select( index , hat_resid = .resid)

# 2) Add resid - and test H0 = cov(x1, u) = 0
mroz_iv_ex %>% left_join(rf_resid, by = "index") %>% 
  lm(formula = log(wage) ~ educ + exper + expersq + hat_resid,
                                                               data = .) %>% 
  summary()

# Moderat evidence that hat_residu. is signifcant differ from 0. We can concl. to alfa = 0.1, that cov(x,u) differ from 0. 
  

## Testing exclution restriction
iv_model <- AER::ivreg(formula = log(wage) ~ educ + exper + expersq | motheduc + fatheduc + exper + expersq, data = mroz_iv_ex)


res_aux_sargan <- augment(iv_model) %>% mutate(index = row_number() ) %>% select(index, .resid) %>% 
  left_join(mroz_iv_ex, by = "index" ) %>% 
  lm( .resid ~  exper + expersq + motheduc + fatheduc  , data = .)

# Chi-test
test_stat <- nrow(mroz_iv_ex)*(glance(res_aux_sargan) %>% pull(r.squared))

#p-value must be below 5% q = 1 (excluded iv -- only one)
1-pchisq(test_stat,1)

# parents education var pass the overid.-test (cant refject cov(z2,u_z1) = 0) p-value to big.
# z2 is father education. The reason we say parent edu. is because we assume mother cov(z1, u) = 0

iv_model2 <- AER::ivreg(formula = log(wage) ~ educ + exper + expersq | motheduc + fatheduc + huseduc + exper + expersq, data = mroz_iv_ex)

res_aux_sargan_2 <- augment(iv_model2) %>% mutate(index = row_number() ) %>% select(index, .resid) %>% 
  left_join(mroz_iv_ex , by = "index" ) %>% 
  lm( .resid ~  exper + expersq + motheduc + fatheduc  + huseduc, data = .)

res_aux_sargan_2 %>% summary()

# Chi-test

test_stat2 <- nrow(mroz_iv_ex)*(glance(res_aux_sargan_2) %>% pull(r.squared))



1-pchisq(test_stat2,1)





