



# Introduction to Stata ---------------------------------------------------

library(haven);library(tidyverse)  

fs::dir_ls("Seminar/RawData")

seminar1_ex_data <- haven::read_dta("Seminar/RawData/econ4137sem1ex1.dta")

angev98 <- haven::read_dta("Seminar/RawData/angev98.dta") %>% as_tibble()

angev98 <- as_tibble(angev98)

# first look
head(seminar1_ex_data)

summary(seminar1_ex_data)


# Data set 2: avgev98
summary(angev98)



angev98 %>% 
  select( marital, educm) %>%
  summarise( n_n = n(),
             mean_marital = mean(marital),
             mean_educm = mean(educm),
             sd_marital = sd(marital),
             sd_educm = sd(educm)) %>% 
  pivot_longer( names_to =  "var", values_to = "value", n_n:sd_educm) %>% 
  extract(var, c("stat", "var"),  "([[:alnum:]]+)_([[:alnum:]]+)") %>% 
  pivot_wider( names_from = stat, values_from = value)  




  