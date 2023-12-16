

# Data and librarys -------------------------------------------------------

library(tidyverse)
theme_set(theme_light())

mus15_raw <- haven::read_dta("Seminar/RawData/mus15data.dta")

mus15 <-  mus15_raw %>% 
  filter( dcharter == 1 | dpier == 1) %>% 
  mutate( y = ifelse(dcharter == 1, 1, 0)) %>% 
  select(y, everything(), -c(mode, dbeach:dcharter)) %>% 
  mutate( category = ifelse(y == 1, "Charter", "pier") ) %>% 
  group_by( category) 


# Descriptive -------------------------------------------------------------

desc_tabel <- function(x) {
  tibble( names = names(x ), expl_var =  sapply(x, function(x) attr(x, "label"))  %>% as.character() )
}

desc_tabel(mus15_raw)

mus15 %>% 
  count( category ) %>% 
  ungroup() %>% 
  mutate( freq = n/sum(n))

mus15 %>% 
  summarise( mean_prive = mean(price, na.rm = T), sd_price = sd(price), mean_income = mean(income), sd_income = sd(income))

# Descriptive graph 
mus15 %>% 
  select(y, category, price, income, ppier, pcharter, -y) %>% 
  mutate( x = log(pcharter/ppier)) %>% 
  pivot_longer( names_to =  "var", values_to = "value", price:x) %>% 
  mutate( range = ifelse(var %in% c("crate", "income", "x") | str_detect(var, pattern = "^q") , "Low", "Hige" )) %>%
  ggplot( ) +
  aes( y = value, x = var, fill = category) +
  geom_boxplot( ) + 
  coord_flip() +
  facet_wrap(~ range, scales = "free")

  
  
mus15 %>%
  ungroup() %>% 
  pivot_longer( names_to = "var", values_to = "value", price:income) %>% 
  select(y,var,value) %>%
  mutate( range = ifelse(var %in% c("crate", "income") | str_detect(var, pattern = "^q") , "Low", "Hige" )) %>% 
  ggplot( ) + 
  aes( y = value, x = var, fill = as.factor(y) ) +
  geom_boxplot( ) +
  coord_flip() +
  facet_wrap(~range, scales = "free") + theme(legend.position = "bottom") 


# create models:

plm <- mus15_simple %>% lm( y_dep ~ x_pred, data = .) 
logit <- mus15_simple %>% glm(y_dep ~ x_pred, data = ., family = binomial(link = "logit"))
probit <- mus15_simple %>% glm(y_dep ~ x_pred, data = ., family = binomial(link = "probit"))

model_list <- list(plm = plm, logit = logit, probit = probit)

# b) Three models estmiated
stargazer::stargazer(plm, logit, probit, type = "text")


# i) The The slope: calculate and interpret the slopes: 
margins::margins(logit, data = mus15_simple, type = "response")
margins::margins(probit, data = mus15_simple, type = "response")
margins::margins(plm, data = mus15_simple, type = "response")

margins::dydx(mus15_simple, logit, "x_pred") %>% summarise_all(mean)
margins::dydx(mus15_simple, probit, "x_pred") %>% summarise_all(mean)
margins::dydx(mus15_simple, plm, "x_pred") %>% summarise_all(mean)

# i) graphical viz. of the slope
qplot(y = y_dep, x = x_pred, data = mus15_simple) + 
  geom_smooth( method = "lm", se = F , color = "gray") +
  stat_smooth(method="glm", method.args=list(family= binomial(link = "logit")), se=F, linetype = 2, color = "red", alpha = 0.5) +
  geom_smooth(method="glm", method.args=list(family= binomial(link = "probit")), se=F, linetype = 3, color = "blue", alpha = 0.5) +
  scale_y_continuous( limits = c(0,1))

slope_plot <- mus15_simple %>% 
  ggplot( ) + aes( y = y_dep, x = x_pred) + 
  geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02)) +
  geom_smooth( method = "lm", se = F , color = "gray", alpha = 0.3) +
  stat_smooth(method="glm", method.args=list(family= binomial(link = "logit")), se=F, linetype = 2, color = "red") +
  geom_smooth(method="glm", method.args=list(family= binomial(link = "probit")), se=F, linetype = 1, color = "blue") +
  scale_y_continuous( limits = c(0,1))

slope_plot + coord_cartesian( ylim = c(0.1, 0.7))

# ii) Compare the models by significans:

# All estimates are significant.
stargazer::stargazer(plm, logit, probit, type = "text")



# iii) cross tabulation 
mus15_simple %>% mutate( pred_y = predict(logit, type = "response")) %>% 
  mutate( pred_y  = ifelse(pred_y >= 0.5, 1, 0)) %>% 
  count(y_dep, pred_y) %>% 
  pivot_wider( names_from = pred_y, values_from = n)

mus15_simple %>% mutate( pred_y = predict(probit, type = "response")) %>% 
  mutate( pred_y  = ifelse(pred_y >= 0.5, 1, 0)) %>% 
  count(y_dep, pred_y) %>% 
  pivot_wider( names_from = pred_y, values_from = n)

# 
df <- tibble(
  names = names(model_list),
  model = map(model_list, ~ .x) ,
  predicted = map(model_list, function(x) { predict(x, type = "response") })
) 

df <-  df %>% add_row( names = "real_obs", model = NA, predicted = list(mus15_simple$y_dep))

# Descriptive of the predication.
df %>% 
  unnest(predicted) %>% 
  group_by(names) %>% 
  summarise( mean_pred = mean(predicted), max_  = max(predicted), min_ = min(predicted), sd_ = sd(predicted))




# iv compare the log-likehood between Probit and Logit

logLik(logit)
logLik(probit)

# Allmost the same

# v) Graph the models
mus15_simple %>% 
  ggplot( ) + aes( y = y_dep, x = x_pred) + 
  geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02)) +
  geom_smooth( method = "lm", se = F , color = "gray", alpha = 0.3) +
  stat_smooth(method="glm", method.args=list(family= binomial(link = "logit")), se=F, linetype = 2, color = "red" ) +
  geom_smooth(method="glm", method.args=list(family= binomial(link = "probit")), se=F, linetype = 1, color = "steelblue", alpha = 0.1) +
  scale_y_continuous( limits = c(0,1)) + theme_minimal(base_size = 15)


# c) ----------------------------------------------------------------------

# i) Evaluate the model partial effect at sample average
margin_probit_at_mean <- margins::margins(probit, data = map_df(mus15_simple, ~mean(.x)))
margin_logit_at_mean <- margins::margins(logit, data = map_df(mus15_simple, ~mean(.x)))

margin_probit_at_mean
margin_logit_at_mean
# ii) average of the complete sample
margin_probit <- margins::margins(probit, data = mus15_simple)
margin_logit <- margins::margins(logit, data = mus15_simple)

# iii) Average of those who choose to fish at piear
margin_probit_at_mean_piear <- margins::margins(probit, data = map_df(filter(mus15_simple, y_dep == 0) , ~mean(.x)))
margin_logit_at_mean <- margins::margins(logit, data = map_df(filter(mus15_simple, y_dep == 0), ~mean(.x)))

margin_probit_at_mean_charter <- margins::margins(probit, data = map_df(filter(mus15_simple, y_dep == 1) , ~mean(.x)))
margin_logit_at_mean_charter <- margins::margins(logit, data = map_df(filter(mus15_simple, y_dep == 1), ~mean(.x)))

margin_probit_at_mean_charter
margin_logit_at_mean_charter

# By hand
tibble(
  names = names(model_list),
  model = map(model_list, ~ .x) ,
  predicted = map(model_list, function(x) { predict(x, type = "response") })
) 



# d) Create a new variable - quntile income -------------------------------

# Create  
mus15_income <- mus15 %>% 
  ungroup() %>% 
  mutate( qt = cut(income, quantile(income, probs = 0:4/4), include.lowest = T) ,
          x_pred = log(pcharter/ppier)) %>% 
  filter( y %in% c(0:1)) %>%
  select( y, x_pred, qt, income)  %>%
  rename( y_dep = y)

# mus15_income %>% distinct(qt) %>% pull(qt) %>% levels()  

logit_income <- mus15_income %>% 
  glm( y_dep ~ x_pred + factor(qt), data = ., family = binomial(link = "logit"))

probit_income <- mus15_income %>% 
  glm( y_dep ~ x_pred + factor(qt), data = ., family = binomial(link = "probit"))

lpm_income <- mus15_income %>% 
  lm( y_dep ~ x_pred + factor(qt), data = .,)


list_model_qt <- list(logit = logit_income, probit = probit_income, lpm = lpm_income)

# Print the result
list_model_qt %>% stargazer::stargazer( type = "text")

# graphical interpretation
list_models <- list(
    logit = logit,
    probit = probit,
    lpm = plm,
    logit_income = logit_income,
    probit_income = probit_income,
    lpm_income = lpm_income
  ) 

df_qt <- tibble( names = names(list_models),
        models = map(list_models, ~.x), 
        augment = map(models, function(x) { broom::augment(x, type.predict = "response") })) 
  

#1  Viz --
library(broom)

augement_unnest <- df_qt %>% 
  unnest(augment ) %>% 
  rename(qt = factor.qt.) %>% 
  mutate( qt = ifelse(is.na(qt), 1, qt), qt = as.factor(qt)) 


# the simple case
augement_unnest %>% 
  filter(names == "logit_income") %>% 
  ggplot( aes(x = x_pred, y =  y_dep, color = qt)) +
  geom_line( mapping =  aes( y = .fitted , linetype = qt))

# for all:
augement_unnest %>% 
  separate(names, into = c("model", "type"), remove = F) %>% 
  mutate( type = ifelse(is.na(type), "simple", type)) %>% 
  ggplot( aes(x = x_pred, y =  y_dep, color = qt)) +
  geom_line( mapping =  aes( y = .fitted , linetype = qt)) +
  facet_grid(model ~ type)


## Compare the margins effect:
margin_probit_at_mean <- margins::margins(probit, data = map_df(mus15_simple , ~mean(.x)) )
margin_probit_income_at_mean <- margins::margins(probit_income, data = map_df(d , ~mean(.x)) )

summary(margins::margins(probit_income, data = probit_income$model %>% as_tibble() %>% 
                           mutate(qt = `factor(qt)`), 
                         at = list(x_pred = c( mean(probit_income$model$x_pred) ) )))


# Exatra ------------------------------------------------------------------


logit_null <- mus15_simple %>%  glm( y_dep ~ 1, family = binomial(link = "logit"), data = .)

logLik(logit_null)

1/ ( 1 + (2*( logLik(logit) [[1]] -logLik( logit_null)[[1]] )/nrow(mus15_simple)))

McF <- 1- (logLik(logit)[[1]]/logLik(logit_null)[[1]])


# Extra
library(caret)
predict_caret <- ifelse( predict(logit, type = "response") > 0.5, "yes", "no") %>% as.factor()

mus15_simple_caret <- mus15_simple %>% mutate( y = ifelse(y_dep == 1, "yes", "no") )

mus15_simple_caret <- mus15_simple_caret %>% mutate( y = fct_relevel(y, "yes", "no"))

p_class <- factor(predict_caret, levels = levels(mus15_simple_caret[["y"]]))

levels(predict_caret)

caret::confusionMatrix(predict_caret, mus15_simple_caret$y)



# i:2 -> Compare the slope for the three different models
x_new_data <- mus15_simple %>% arrange(x_pred) %>%  summarise( q = list(quantile(x_pred))) %>% unnest(q) %>% rename(x = q)




# 
  

