

library(tidyverse)
library(broom)
# -------------------------------------------------------------------------

data("cars", package = "datasets")


cars <- cars %>% as_tibble()

# The model:
lm_cars <- lm(dist ~ speed, data = cars)

summary(lm_cars)

# Critacal t-value, degree of freedom = df
crit_val <- qt( ( 1-0.05/2), df = nrow(cars)-2)

# Residual stanard error:
res_standard_error <- glance(lm_cars) %>% pull(sigma)

# Create the plot, with low and high area.
lm_cars <-
  cars %>% 
  lm(dist ~speed, data = .  ) %>% 
  augment(  ) %>% 
  mutate( .se.pred = sqrt(res_standard_error^2 + .se.fit^2)) %>% 
  mutate( lowDist = .fitted - crit_val*.se.pred,
          highDist =.fitted + crit_val*.se.pred )  

# Plotting prediction intervals
lm_cars %>%   
  ggplot( ) +
  aes( x = speed, y = dist) +
  geom_point( ) +
  stat_smooth( method = "lm", se = F) +
  geom_ribbon( aes(ymin = lowDist, ymax = highDist, alpha = 0.01))



# Plotting CI for average response ----------------------------------------




