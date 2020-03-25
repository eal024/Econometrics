



# lib and data ------------------------------------------------------------
library(tidyverse)
library(broom)

benefits <- Ecdat::Benefits %>% as_tibble()

benefits %>% skimr::skim()

lpm <- benefits %>% 
  mutate( ui =ifelse(ui == "yes", 1, 0)) %>% 
  lm(ui  ~  rr + I(rr^2) + age + I(age^2/10) + tenure + joblost + 
       married + dkids + dykids + smsa + nwhite + yrdispl + school12  + sex + statemb + stateur + head, data = .)

summary(lpm)


logit <- benefits %>%
  mutate(ui = ifelse(ui == "yes", 1, 0)) %>%
  glm(
    ui  ~  rr + I(rr ^ 2) + age + I(age ^ 2 / 10) + tenure + joblost + married + dkids + dykids + smsa + nwhite + yrdispl + school12  + sex + statemb + stateur + head,
    family = binomial(link = "logit"),
    data = .
  )


summary(logit)

broom::glance(logit)



probit <- benefits %>%
  mutate(ui = ifelse(ui == "yes", 1, 0)) %>%
  glm(
    ui  ~  rr + I(rr ^ 2) + age + I(age ^ 2 / 10) + tenure + joblost + married + dkids + dykids + smsa + nwhite + yrdispl + school12  + sex + statemb + stateur + head,
    family = binomial(link = "probit"),
    data = .
  )

summary(probit)

glance(probit)
logLik(probit)
logLik(logit)


# R2 ----------------------------------------------------------------------

# by hand (logit)
logit_null  <- benefits %>% mutate(ui = ifelse(ui == "yes", 1, 0)) %>%
  glm( ui ~ 1, data = ., family = binomial(link = "logit"))

summary(logit_null)

logit_full <- logit

# McFadden R2 = 1- log L1/log L0

1- logLik(logit_full)/logLik(logit_null)

# pseudo R2 = 1 − (1/1 + 2 {(logL1 − logL0)/N} )

1 - (1/(1+2* (logLik(logit) - logLik(logit_null))/nrow(benefit) ))





benefit <- benefits %>% mutate(ui = ifelse(ui == "yes", 1, 0))

rms::lrm(formula = ui  ~  rr + I(rr ^ 2) + age + I(age ^ 2 / 10) + tenure + joblost + married + dkids + dykids + smsa + nwhite + yrdispl + school12  + sex + statemb + stateur + head, data = benefit)


# Cross-tabulation  -------------------------------------------------------


augment(logit) %>%
  mutate( pred_y = ifelse(.fitted  >= 0.5, 1, 0)) %>% 
  count( ui, pred_y) %>% 
  pivot_wider( names_from = pred_y, values_from = n) %>% janitor::clean_names() 



predict <- ifelse( predict(logit, type = "response") > 0.5, 1, 0)


benefit_cross_tab <-
  benefits%>% 
  mutate( predict = predict,
          ui = ifelse( ui == "yes", 1, 0)) %>% 
  select( ui, predict) %>% 
  mutate(y_hat  = ifelse( predict > 0.5, 1, 0 ))

benefit_cross_tab %>% 
  count(ui, y_hat)  %>% 
  pivot_wider(names_from = y_hat, values_from = n)


# Conf.matrix -------------------------------------------------------------


predict_caret <- ifelse( predict(logit, type = "response") > 0.5, "yes", "no") %>%
  as.factor()

p_class <- factor(predict_caret, levels = levels(benefits[["ui"]]))

levels(predict_caret)

caret::confusionMatrix(predict_caret, benefits$ui)



# Example class rom -------------------------------------------------------


