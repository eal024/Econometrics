



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



# Example lectures -------------------------------------------------------

priv_insurance <- haven::read_dta("mus14data.dta")

priv_insurance %>% skimr::skim()

# modeling
lpm <- priv_insurance %>% lm( ins ~ retire + age + hstatusg  +hhincome + educyear + married + hisp , data  = .)
logit <- priv_insurance %>% glm( ins ~ retire + age + hstatusg  +hhincome + educyear + married + hisp , data  = ., family = binomial(link = "logit"))
probit <- priv_insurance %>% glm( ins ~ retire + age + hstatusg  +hhincome + educyear + married + hisp , data  = ., family = binomial(link = "probit"))

summary(lpm)
summary(logit)
summary(probit)

stargazer::stargazer(logit, probit, lpm, type = "text")

#  Descriptive statistic of the models of ins:
list_models <- list("plm" = lpm, "logit" = logit, probit = probit )

df_models_fitted <- tibble(
  model = names(list_models),
  models = walk(list_models, function(x) {x}),
  augment = map(models, function(x) { augment(x, type.predict = "response" )})) %>% 
  unnest(augment ) %>% 
  select(model, ins, .fitted ) %>% 
  pivot_longer( names_to = "var", values_to = "value", ins:.fitted) %>% 
  filter( ! (var == "ins" & model %in% c("plm", "logit"))) %>% 
  mutate( model  = ifelse(var == "ins", "ins", model))


df_models %>% 
  group_by(model) %>% 
  summarise(
    obs = n(),
    mean = mean(value),
    sd_dev = sd(value),
    min_value = min(value),
    max_value = max(value)
  )


mode_private <- priv_insurance %>%   glm(ins ~hhincome, data = ., family = "binomial")

priv_insurance %>% 
  ggplot( ) + aes( y = ins, x = hhincome) +
  geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02))  +
  geom_smooth( method = "glm", se = F, method.args = list(family = "binomial"), color ="blue") +
  geom_line(data = augment(mode_private, type.predict = "response"),
            aes(y = .fitted), color = "red")















