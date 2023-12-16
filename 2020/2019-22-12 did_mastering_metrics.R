
library(tidyverse)

# -------------------------------------------------------------------------

banks <- read_csv(here::here("/Differences in Differences/mm_banks.csv"))

banks2 <-
  banks %>% 
  unite( "new_date", year,month,day, sep = "-") %>% 
  mutate( new_date = lubridate::ymd(new_date))


banks2 %>% 
  ggplot() +
  aes( bib6, x = new_date) +
  geom_smooth( se = F)


