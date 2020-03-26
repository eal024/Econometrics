

# Data and packages -------------------------------------------------------


library(tidyverse)
library(broom)
options(scipen =9)
lalonde <- foreign::read.dta("Seminar/RawData/lalonde.dta") %>% as_tibble()

lalonde %>% filter(as.integer(sample) == 3) %>% count( )

# Filter for sample in seminar: 
lalonda_sem4 <-
  lalonde %>% 
  filter( as.integer(sample) == 3 ) %>% 
  select(-c(sample, treatment)  ) %>% 
  select( earnings78, age, married, black, education, earnings75, hispanic,nodegree )

lalonda_sem4


# a) Model linear-linear and log-linear -----------------------------------

# Model 1
model1 <- lalonda_sem4 %>%
  select(earnings78, age, married, black, education, earnings75, hispanic, nodegree ) %>% 
  lm( earnings78 ~., data = .)

summary(model1)

# Filter away earning that is 0 
model2 <- lalonda_sem4 %>%
  filter( earnings78 != 0) %>% 
  select(earnings78, age, married, black, education, earnings75, hispanic, nodegree ) %>% 
  lm( I(log(earnings78)) ~., data = .)

summary(model2)

# Interpretation of models -->

# b) Breusch-Pagan test ---------------------------------------------
# 
# Two altern. way to get the e.

auxiliary_model <- function(model) {
  # lm-object
  e_2 <- (model$residuals)^2  
  
  # data
  data <- broom::augment(model) %>%
    mutate( .resid = as.numeric(.resid)) %>% 
    mutate( e_2 = .resid^2) %>% 
    select(-contains("."), -1)
  
  data %>% lm(e_2 ~., data = .)
}

aux_model2 <- auxiliary_model(model2)
aux_model1 <- auxiliary_model(model1)

# models:
map(list(aux_model1, aux_model2), ~summary(.x))

aux_model2$model %>% nrow()


# Igjen:
# c) Multipl. hetroskedasticity ----------------------------------------------

aux_model2 <- augment(model2) %>% 
  mutate(.resid = as.numeric(.resid), e_2 = .resid^2) %>%
  select(age:nodegree, e_2) %>% 
  lm( I(log(e_2)) ~ . , data = .) 

aux_model2 %>% summary

lmtest::bgtest(model2)

# Extra: Vis. residuals ag. x --------------------------------------------------

# model1 - linear-linear model
summary(model1)



model_stp1 <- 
  lalonda_sem4 %>%
  select( earnings78, age,married,black,education,earnings75,hispanic,nodegree) %>% 
  lm( I((earnings78)) ~ ., data = .) 

summary(model_stp1)


# 2
fitted <-  augment(model_stp1) %>%
  mutate_at( vars(contains(".")) , as.numeric) %>%
  mutate( log_e_2  = (log(.resid^2)) ) %>% 
  select( log_e_2, age:nodegree) %>% 
  lm( log_e_2 ~., data = .) %>% 
  augment( ) %>% 
  mutate( index = row_number()) %>% 
  select(earnings75, .fitted)

# 3)
h <- fitted %>% mutate( h = ( exp(.fitted) )^0.5) %>% pull(h)

length(h)

fgls_model2 <- 
  lalonda_sem4 %>% 
 
  mutate( w = 1) %>%
  rename_all(function(x) {str_c("w",x)}) %>% 
  mutate_all( function(x) {x/h}) %>% 
  # The FGLS model
  lm( wearnings78 ~ 0 +. , data = .)

fgls_model2 %>% summary()

lalonde %>%
  filter( as.integer(sample) == 3) %>%
  mutate( c = 1) %>% 
  lm(earnings78 ~ 0 + c + age + education + black + hispanic + married + nodegree + earnings75 , weights=(1/h^2), data = .) %>% summary()



# Robust st. error --------------------------------------------------------

# model1 
model1_robust <- 
  lalonde %>% 
  filter( as.integer(sample) == 3) %>% 
  select(earnings78, age, married, black, education, earnings75, hispanic, nodegree ) %>% 
  estimatr::lm_robust( earnings78 ~., data = .)

model1_robust %>% 
  summary() 



# Example FGLS - Labour-demand --------------------------------------------

labour <- Ecdat::Labour %>% as_tibble()

model1 <- labour %>% lm(labour ~ wage + output + capital , data = .)

summary(model1)

# Breuch_Pagan test:
model1_augment <- broom::augment(model1)

auxilliary_model1 <- model1_augment %>% 
  mutate(e2 = .resid^2) %>% 
  lm(e2 ~ wage + output + capital, data = .)

summary(auxilliary_model1)
labour %>% nrow()

model1_augment %>% 
  mutate(e2 = .resid^2) %>% 
  lmtest::bptest(e2 ~ wage + output + capital, data = .) 

  # We can compute the Breusch–Pagan
  # test statistic by computing N = 569 times the R2 of this auxiliary regression, which
  # gives 331.0. As the asymptotic distribution under the null hypothesis is a Chi-squared
  # with three degrees of freedom, this implies a very sound rejection of homoskedasticity.


# Handling the hetroskedasticity - change functional form

labour_log <- modify(labour, function(x) {log(x)}) %>% 
  rename_all(function(x) {str_c("log_", x)}) 

model2 <- labour_log %>%
  lm(log_labour ~ ., data = .)

summary(model2)

# Breuch-Pagan
auxillary_model2 <- broom::augment(model2) %>% mutate(e2 = .resid^2) %>% lm(e2 ~ wage + output + capital, data = .)

# White-test
auxillary_white_model2 <-  broom::augment(model2) %>%
  mutate(e2 = .resid^2) %>%
  mutate( log_capital_2 = log_capital^2, log_output_2 = log_output^2, log_wage_2 = log_wage^2) %>% 
  mutate(
    log_cap_out = log_capital * log_output,
    log_out_wage = log_output * log_wage,
    log_cap_wage = log_capital * log_wage
  ) %>%
  select(contains("log"), e2, -log_labour) %>% 
  lm(e2 ~ ., data = .) 
  

summary(auxillary_white_model2)

# Robust st.errors
model2_robust <- labour_log %>% estimatr::lm_robust(log_labour ~ ., data = .) %>% summary()

model2_robust
# FGLS

# h(.) estmiated from White.test

aux_loge2_model2 <- broom::augment(model2) %>% 
  mutate( log_e2 = log(.resid^2) ) %>%
  lm( log_e2 ~ log_capital + log_output + log_wage, data = .)

summary(aux_loge2_model2)
  

h <- exp(aux_loge2_model2$fitted.values)^0.5


modify(labour_log %>% mutate(c = 1), ~(.x/h) ) %>% 
  lm(log_labour ~ 0 +c +  log_capital + log_wage + log_output , data = .) %>% 
  summary()


# ols
labour_log %>% 
  lm(log_labour ~  log_capital + log_wage + log_output , data = .) %>% 
  summary()
















