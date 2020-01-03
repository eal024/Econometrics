

library(tidyverse)
# IV-regression -----------------------------------------------------------

#' causality with IV. The logic og IV:
#' 6 step for IV:
#' 1) Observe a variable called Instrument - that is correlated with the outcome.
#' - Unit with higher outcome (y) tend to have higher of in IV (or negative)
#' 2) Assume - IV dont have a casual-effect on y. The cause comes from a confounding variable.
#' 3) Assume - IV has causal effect on treatment
#' 4) Assume the IV is randomly assigned (or as if)
#' 5) The IV- is corr. with treatment.
#' 6) Since randomly assigned, IV- is not corr. with any other confounding v. , exepct the treatment.

#' Roled out any other effect on treatment, expect the IV. Since it is randomly assigned. (or as if)


# Some IV - terminology ---------------------------------------------------

#' Exogenous variation: Unrealated to all other variables that might affect the outcome.
#' Endogenous variation: Tied to the variables that affect the outcome.
#' - THe IV proveides a source of exogenous variation in the treatment.
#' - IV-reg. is a source to random assigned variation.


#' Exclusion restriction: The IV does not have a direct causal effect on the outcome
#' Ex. living close to college, does not have effect on wage (other than corr. to effect from education)


# Eductions effect on wage ------------------------------------------------

set.seed(123)

# Dont observe ability. Ability is not caused by distance to collage, but are corr. to education.
df <- tibble( ability = rnorm(100, mean = 1, sd = 0.1),
              educ = sample(100, replace = T), 
              wage = 1 + 1*educ + 0.7*ability + rnorm(100),
              distance_to_college = runif(100)*educ)
  

ggplot(df, aes(y = ability, x = distance_to_college)) + 
  geom_point() + 
  geom_smooth( method = "lm", se = T)


df %>% 
  lm( wage ~ ability + educ, data = .) %>% summary










