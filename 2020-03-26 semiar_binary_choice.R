


# Data and librarys -------------------------------------------------------

library(tidyverse)
theme_set(theme_light())


mus15_raw <- haven::read_dta("Seminar/RawData/mus15data.dta")

mus15 <-  mus15_raw %>% 
  select(-mode) %>% 
  pivot_longer(names_to = "y", values_to = "value", dbeach:dcharter) %>% 
  filter( value == 1,  y %in% c("dpier", "dcharter")) %>% 
  mutate( y_dep = ifelse( y == "dcharter", 1, 0)) %>% 
  select(-value)
  
# 
mus15 %>% 
  pivot_longer( names_to = "var", values_to = "value", price:income) %>% 
  select(y,var,value) %>%
  mutate( range = ifelse(var %in% c("crate", "income") | str_detect(var, pattern = "^q") , "Low", "Hige" )) %>% 
  ggplot( ) + 
  aes( y = value, x = var, fill = y ) +
  geom_boxplot( ) +
  coord_flip() +
  facet_wrap(~range, scales = "free") 
  
mus15 %>% 
  pivot_longer( names_to = "var", values_to = "value", price:income) %>% 
  select(y,var,value) %>%
  mutate( range = ifelse(var %in% c("crate", "income") | str_detect(var, pattern = "^q") , "Low", "Hige" )) %>% 
  ggplot( ) + 
  aes( y = value, x = var, fill = y ) +
  geom_boxplot( ) +
  coord_flip() +
  facet_wrap(~range, scales = "free") 

#
mus15 %>% 
  select(contains("charter") | contains("pier"), y) %>% 
  mutate( q = log(qcharter/qpier), p = log(pcharter/ppier)) %>% 
  ggplot( ) +
  aes(p,q) + geom_point( alpha = 0.9, position = "jitter") + geom_smooth(method = "lm")

# 
mus15_simple <- 
  mus15 %>% 
  mutate( x_pred = log(pcharter/ppier)) %>% 
  select(y_dep, x_pred)



plm <- mus15_simple %>% lm( y_dep ~ x_pred, data = .) 
logit <- mus15_simple %>% glm(y_dep ~ x_pred, data = ., family = binomial(link = "logit"))
probit <- mus15_simple %>% glm(y_dep ~ x_pred, data = ., family = binomial(link = "probit"))


# b)
stargazer::stargazer(plm, logit, probit, type = "text")




# 
mus15_simple %>% 
  ggplot( ) + aes( y = y_dep, x = x) + 
  geom_point(alpha = 0.5, position = position_jitter(w=0, h=0.02)) +
  stat_smooth(method="glm", method.args=list(family= binomial(link = "logit")), se=F, linetype = 2, color = "red") +
  geom_smooth(method="glm", method.args=list(family= binomial(link = "probit")), se=F, linetype = 1, color = "blue")
  


  
  
# b) Esitmate the model
# tibble(
#   type = c("lpm", "logit", "probit"),
#   model = map(  type, 
#                 function(x) { ifelse(x == "lpm", lm(y_dep ~ x_pred, data = mus15_simple), 
#                                      glm(y_dep ~ x_pred, family = binomial(link = x)  ,data = mus15_simple) ) 
#                   }),
#   
#   )
