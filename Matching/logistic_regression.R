
# https://www.r-bloggers.com/2024/01/binary-logistic-regression-in-r/
# library and data
library(tidyverse)

# Heart Disease data
heart <- kmed::heart |> tibble()  

skimr::skim(heart) # Stats

# CP: chest pain
# thalach: max heart rate
# class: Diagnostics

heart1 <- heart |> 
  rename(
    chest_pain = cp,
    max_heartrate = thalach,
    heart_disease = class
  ) |> 
  mutate( 
    sex           = factor(sex, levels = c(F,T), labels = c("fe", "male")),
    chest_pain    = factor(chest_pain),
    heart_disease = ifelse( heart_disease == 0, 0, 1)
  )







