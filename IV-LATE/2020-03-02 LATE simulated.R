

# library( )
library(tidyverse)

# Generate random numbers
mean(sample(c(0,1), replace =  T, size = 10000))
unif

# Create never-takers (d00), always-takers( d11), compliers (d01), independent of z

N <- 20000

df <-  tibble( index = seq(1:N),  z = sample(c(0,1), replace = T ,size = N), id = case_when( between(index, 1, 5000) ~"d00",
                                                                                   between(index, 5001,10000) ~ "d11",
                                                                                   T ~ "d01") ,
        # Observe outcome - Always 0 for n-t, always 1 for a-t, and dependent on IV for Compliers
        D = case_when( id == "d01" ~ z,
                       id == "d11" ~ 1,
                       # For compliers, d dependent on Z - the instrument
                       id == "d00" ~ 0
                       ) 
        ) %>% 
  mutate( late =  case_when( id == "d00" ~ -1,
                             id == "d11" ~ 0,
                             # d11 - d 00 = 1 -> Treatment effect for compliers 
                             id == "d01" ~ 1)
          ) %>% 
  # Generate potential outcome
  mutate( y0  = 0.25*rnorm(n = N, mean = 0, sd = 1),
          y1 = y0 + late,
          # Observe outcome is dependent on treatment:
          y = D*y1 + (1-D)*y0)

# Average treatment effect
skimr::skim(df)

df %>% lm( y ~D ,data = .) %>% summary()

# IV gives you LATE for the compliers
AER::ivreg( y ~D | z, data = df) %>% summary()







