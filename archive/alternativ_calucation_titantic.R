
library(stargazer)
library(magrittr) # for %$% pipes
library(tidyverse)
library(haven)

titanic <- titanic |> 
  mutate(d = case_when(class == 1 ~ 1, TRUE ~ 0))


titanic %<>%
  mutate(s = case_when(sex == 0 & age == 1 ~ 1,
                       sex == 0 & age == 0 ~ 2,
                       sex == 1 & age == 1 ~ 3,
                       sex == 1 & age == 0 ~ 4,
                       TRUE ~ 0))

ey11 <- titanic %>% 
  filter(s == 1 & d == 1) %$%
  mean(survived)

ey10 <- titanic %>% 
  filter(s == 1 & d == 0) %$%
  mean(survived)

ey21 <- titanic %>% 
  filter(s == 2 & d == 1) %$%
  mean(survived)

ey20 <- titanic %>% 
  filter(s == 2 & d == 0) %$%
  mean(survived)

ey31 <- titanic %>% 
  filter(s == 3 & d == 1) %$%
  mean(survived)

ey30 <- titanic %>% 
  filter(s == 3 & d == 0) %$%
  mean(survived)

ey41 <- titanic %>% 
  filter(s == 4 & d == 1) %$%
  mean(survived)

ey40 <- titanic %>% 
  filter(s == 4 & d == 0) %$%
  mean(survived)

diff1 = ey11 - ey10
diff2 = ey21 - ey20
diff3 = ey31 - ey30
diff4 = ey41 - ey40

obs = nrow(titanic %>% filter(d == 0))

wt1 <- titanic %>% 
  filter(s == 1 & d == 0) %$%
  nrow(.)/obs

wt2 <- titanic %>% 
  filter(s == 2 & d == 0) %$%
  nrow(.)/obs

wt3 <- titanic %>% 
  filter(s == 3 & d == 0) %$%
  nrow(.)/obs

wt4 <- titanic %>% 
  filter(s == 4 & d == 0) %$%
  nrow(.)/obs

wate = diff1*wt1 + diff2*wt2 + diff3*wt3 + diff4*wt4



ey1 <- titanic %>% 
  filter(d == 1) %>%
  pull(survived) %>% 
  mean()

ey0 <- titanic %>% 
  filter(d == 0) %>%
  pull(survived) %>% 
  mean()

sdo <- ey1 - ey0

stargazer::stargazer(wate, sdo, type = "text")
