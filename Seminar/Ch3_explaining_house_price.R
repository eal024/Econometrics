



# library and data --------------------------------------------------------

library(tidyverse);library(haven)

fs::dir_ls("Verbeek Guide to Modern Econ/data_verbeek")

housing <- read.delim("Verbeek Guide to Modern Econ/data_verbeek/housing.dat")

housing_raw <- read_delim("Verbeek Guide to Modern Econ/data_verbeek/housing.dat", delim = " ")

housing <-
  housing_raw %>% 
  mutate_at( vars(price, lotsize), function(x) { as.numeric(str_trim(x, side = "both"))})

# Sold house during July, August and September 1987. Windsor, Cananda
housing %>% str()

# Plot the data
plot(housing)

GGally::ggpairs(housing %>% select(price, lotsize, bedrooms, bathrms, airco))



# Hedonic price function: implisit price of certain attribute: number of bedrom

# Model 1 :To start our analysis, we shall first estimate a model that explains the log of the
# sale price from 
# the log of the lot size,
# the numbers of bedrooms and bathrooms and the
# presence of air conditioning.

model1 <- housing %>% 
  lm( log(price) ~ I(log(lotsize)) + bedrooms + bathrms + airco, data = .)

new_data <- tibble( lotsize = 5000, bedrooms = 4, bathrms = 1, airco = 0 )

summary(model1)

sigma <- broom::glance(model1) %>% pull(sigma)

model1 %>% broom::augment( newdata = new_data    ) %>% 
  mutate( fitted_price = exp(.fitted + 0.5*sigma^2))

# proof of way we need to transform that way
model1 %>% broom::augment(   ) %>% 
  mutate( a_price = exp(log.price.),
    fitted_price_wrong = exp(.fitted + 0.0*sigma^2),
    fitted_price_right = exp(.fitted + 0.5*sigma^2)
    ) %>% 
  select( contains("_price") ) %>% 
  summarise_all( function(x){ mean(x)})



# From the book:
# ...The average predicted price is 66 679 dollars, while the sample
# average of actual prices is 68 122. This indicates that without any corrections we would
# systematically underpredict prices.



# RESET-test --------------------------------------------------------------

model2 <-
  model1 %>% 
  broom::augment() %>%
  mutate( yhat_2 = .fitted^2) %>% 
  lm( log.price. ~ I.log.lotsize.. + bedrooms + bathrms + airco + yhat_2 , data = .) 

# The yhat_2 does not have power, does not indicate a wrong f-form.

R2_reset <- model2 %>%
  glance %>%  pull(r.squared)

R2_m1 <- model1 %>% glance() %>% pull(r.squared)

f_test <- ((R2_reset-R2_m1)/(6-5))/((1-R2_m1)/(541-6))

# Which is not enough to reject the H0.


# Model3 - Include all variables ------------------------------------------
options(scipen = 999)

model3 <- 
  housing %>% 
  mutate( log_lotsize = log(lotsize)) %>% 
  select( - lotsize) %>% 
  lm(I(log(price)) ~  ., data = .)

# calculation of R2 - manually
model3 %>% augment() %>%
  mutate( sst_part =  (I.log.price.. - mean(I.log.price..))^2 ) %>% 
  summarise( sse = sum(.resid^2),
             sst = sum(sst_part),
             R2 = 1- (sse/sst) 
             )


# RESET-test
model3_reset <- model3 %>% 
  augment() %>% 
  mutate( y_hat_2 =  .fitted^2, y_hat_3 =  .fitted^3 ) %>%
  select(1:12, y_hat_2, y_hat_3) %>% 
  lm( I.log.price.. ~ ., data = .)

r2_m3_reset <- model3_reset %>% glance %>% pull(r.squared)
r2_m3 <- model3 %>% glance %>% pull(r.squared)


# F-test: close to 0.04 as in the book
f_test_reset <- ((r2_m3_reset-r2_m3)/(2))/((1-r2_m3_reset)/(546-12))


# F-test between model1 and model 2 (3)

R2_model1 <- model1 %>% glance() %>% pull(r.squared)
R2_model3 <- model3 %>% glance() %>% pull(r.squared)

Ftest <- ((R2_model3-R2_model1)/(12-5))/((1-R2_model3)/(546-12))

Ftest
#Which is significant.



# Non nested model test ---------------------------------------------------

# Alternativ model for houseprice:

model4 <- housing %>% 
  lm( price ~ ., data = . )

log_fitted_model3 <-
  model3 %>%
  augment() %>% 
  mutate( index = row_number()) %>% 
  select(index, everything()) %>% 
  select(index, .fitted)


PE_test <-
  model4 %>% 
  augment() %>% 
  mutate( index = row_number( ) , log_linear_fitted =log(.fitted) ) %>% 
  left_join( log_fitted_model3, by = "index") %>% 
  mutate( log_price_test = log_linear_fitted - .fitted.y) %>% 
  lm( price ~ lotsize + bedrooms + bathrms + airco + driveway + recroom + fullbase + gashw + garagepl + prefarea + stories + log_price_test, data = .) %>% 
  summary

# As we can see, the t-value for Koff(log(fitted.linear price) - fitted.log Pirce)) is significant. 











