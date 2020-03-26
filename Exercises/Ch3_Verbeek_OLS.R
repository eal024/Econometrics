



# library -----------------------------------------------------------------

library(tidyverse);library(broom);library(Ecdat)

fs::dir_ls("Verbeek Guide to Modern Econ/data_verbeek")

clothing <- Ecdat::Clothing


clothing <- as_tibble(clothing)

# Have a look at the data
clothing %>% 
  skimr::skim()


# A)
clothing %>% 
  lm( sales ~ hoursw + ssize , data = .) %>% summary

clothing %>% 
  lm( sales ~ hoursw + ssize , data = .) %>% 
  augment() %>% 
  mutate( y_hat_2 = .fitted^2) %>% 
  lm( sales  ~ hoursw + ssize  +y_hat_2 , data = .) %>% summary

# Cant reject the H0. Cant reject that the f-form is wrong.

# c) Does number of owner effect shope sales? 
modela <- clothing %>% 
  lm( sales ~ hoursw + ssize , data = .)

modela_extend <- clothing %>% 
  lm( sales ~ hoursw + ssize + nown, data = .)

modela_extend %>% summary
# No, number of owner does not affect shop sales
  

# d)
modelb <- clothing %>% 
  lm( sales ~ nown + ssize + nfull , data = .)


modelb %>% 
  glance() %>% 
  mutate( model = "B") %>% 
  bind_rows(modela %>%
              glance %>%
              mutate(model = "A")
            ) %>% 
  select(model, everything()) %>%
  summarise( adj.R = r.squared[model == "A"] - r.squared[model == "B"],
             AIC = AIC[model == "A"] - AIC[model == "B"],
             BIC = BIC[model == "A"] - BIC[model == "B"]
             )


# g -- non nested F-test
# var ssize is in both models

# test model B

clothing %>% lm( sales ~ nown + ssize + nfull + hoursw, data = .) %>% summary
# Can reject B


# test A
clothing %>% lm( sales ~ ssize  + hoursw + (nfull + nown), data = .) %>% summary



# J-test of model A
modelb %>% augment() %>% select(sales , .fitted)

modela %>% 
  augment() %>% 
  left_join(modelb %>% 
              augment() %>%
              select(sales , fitted_modelb = .fitted), by = "sales") %>% 
  lm( sales ~ hoursw + ssize + fitted_modelb, data = .) %>% 
  summary

# There is indices that modelA can be rejceted.

# h)
clothing %>% names()

modelD <-
  clothing %>% lm(sales ~ nfull + npart + hoursw + ssize, data = .)

# 
modelD %>% summary

modela %>% summary

# Effect from hour is lower after inculded npart and nfull. 

modelD %>% augment() %>% 
  lm( sales ~ nfull + npart + hoursw + ssize + I(.fitted^2), data = .) %>% 
  summary



# Exercise 3.3 Empircal regression ----------------------------------------



housing <-  Ecdat::Housing

housing <-
  housing %>% as_tibble()


model_1 <-
  housing %>% 
  mutate( bedrooms_less = ifelse(bedrooms %in% seq(0:2), 1, 0 ) ) %>% 
  select( price, lotsize,  bathrms, airco, bedrooms, bedrooms_less) %>% 
  lm( log(price) ~ log(lotsize) + bathrms + airco + bedrooms_less, 
      data = .) 

  housing %>% 
  mutate( bedrooms_less = ifelse(bedrooms %in% seq(0:2), 1, 0 ) ) %>% 
  select( price, lotsize,  bathrms, airco, bedrooms, bedrooms_less) %>% 
  lm( log(price) ~ log(lotsize) + bathrms + airco + (bedrooms), 
      data = .)



model_1 %>% summary

model_tabel3_1 %>% summary

# Test those two models.


housing %>% 
  mutate( bedrooms_less = ifelse(bedrooms %in% seq(0:2), 1, 0 ) ) %>% 
  select( price, lotsize,  bathrms, airco, bedrooms, bedrooms_less) %>% 
  lm( log(price) ~ log(lotsize) + bathrms + airco +  bedrooms_less, 
      data = .) 


