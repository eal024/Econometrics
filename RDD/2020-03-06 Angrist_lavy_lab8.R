

library(tidyverse)

final <- haven::read_dta("RDD/final5.dta") %>% as_tibble()

final %>% skimr::skim()

# Using Maimonides Rule to estimate the Effect of Class Size on Student Achievement”

# 1
final_clean <- final %>% 
  filter(between(classct, 5, 45)) %>% 
  select( classize, enroll = c_size, pdis = tipuach, verbsize, math %>% %>% %>% %>% %>% %>% size, avgmath,avgverb) %>%
  mutate( avgmath = ifelse(avgmath == 181.246, 81.246, avgmath)) %>% 
  mutate( classize = ifelse(is.na(verbsize) | is.na(mathsize), NA, classize),
          enroll = ifelse(is.na(verbsize) | is.na(mathsize), NA, classize),
          pdis = ifelse(is.na(verbsize) | is.na(mathsize), NA, classize)) 



# Replicate table1
library(stargazer)

# Short 
stargazer(as.data.frame(final %>% select(classize, enrollment = c_size,Percentdisad = tipuach,verbsize,avgmath,avgverb,mathsize) %>%   drop_na()), type = "text")

table_stat <- stargazer(as.data.frame(final %>% select(classize,
                                                       enrollment = c_size,
                                                       Percentdisad = tipuach,
                                                       verbsize,
                                                       avgmath,
                                                       avgverb,
                                                       mathsize) %>% 
                                        drop_na())  ,
          type = "text",
          title = "Replica table 1, Angrist and Lavy, 1999",
          digits = %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% %>% 