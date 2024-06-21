


# Lalonde data from Smith and Todd (J Ectrics, 2005)

library(tidyverse)
library(fixest)
library(margins)

# Lalonde data
dat <- haven::read_dta("data/lalonde2.dta")

# 1) The NSW sample is the randomized data. Compute the T-effect and look at the ballancing properites
dat_random <- dat |> filter( sample == 1)  

# Mean value for each variable
stat_mean <- dat_random |> group_by( treated )  |> summarise_all(mean) # look simular between groups

stat_mean |>pivot_longer(-treated) |>  pivot_wider(names_from =treated, values_from = value) 

# t-test for each variable
tbl <- tibble( n = names(dat_random), data = list(dat_random))

# t-test



# 2) using CPS data as control. (CPS: Regular persons, not assigned to program/selected as randomized controls)

# Data preperation. Sample selections
dat1 <- dat |>
    mutate( 
        d = case_when(
            sample == 1 & treated == 1 ~ 1,
            sample == 2 ~ 0),
        age2 = age^2,
        age3 = age^3/1000,
        educ2 = educ^2,
        zero74 = ifelse( re74 == 0, 1, 0),
        zero75 = ifelse( re75 == 0, 1, 0)
        )  |> 
    # Keeping treated from the NSW (randomized data), and the CPS as controls
    filter( sample %in% c(1,2), d %in% c(0,1) )  |> 
    select(d, everything(), -c(treated, early_ra, dwincl) )


# Calculate the effect with OLS
vars <- c( "d", "age", "age2", "age3", "educ", "educ2", "hisp", "black", "re74", "re75")
formula1 <- as.formula(  paste("re78 ~ ", paste(vars, collapse = " + ")))
model1 <- feols( data = dat1, fml = formula1)

# Wrong from selekction.
esttable( list(model1))


# Prepering data for regression for more accurate OLS

# i) variable

# Spesifal changes
max_re74 <- max(dat2$re74[dat2$d == 0])
max_re75 <- max(dat2$re75[dat2$d == 0])

dat3 <- dat1 |>
    mutate(
        re74 = ifelse( d == 1 & re74 > max_re74, max_re74, re74),
        re75 = ifelse( d == 1 & re75 > max_re75, max_re75, re75),
        max74 = ifelse( re74 == max_re74, 1, 0),
        max74 = ifelse( re75 == max_re75, 1, 0)
    ) 

# Global vars
vars           <- c( "age", "age2", "age3", "educ", "educ2", "black", "hisp" ,"married", "nodegree", "re74", "re75", "kids18", "kidmiss", "max74", "max74", "zero74", "zero75")
vars_string    <- paste(vars, collapse =  " + ")
formula_string <- paste("re78 ~ black * (", vars_string, ")", sep = "")
formula        <- as.formula(formula_string)

# Model fitted on d == 0
model_ols2 <- feols( data = dat3 |> filter( d == 0 ), fml = formula , vcov = "HC1")

# Residal calcualted for every obs
dat3$u <- dat3$re78 - predict(model_ols2, newdata = dat3)

# See result for d == 1
skimr::skim(dat3$u[dat3$d == 1])


# Alternative
formula_string <- paste("re78 ~ d * black * (", vars_string, ")", sep = "")
formula <- as.formula(formula_string)

model_interaction <- lm(formula =  formula, data = dat3)

library(margins)
margins_model <- margins(
    data = dat3 |> filter( d == 1) |> na.omit(),
    model_interaction,
     variables = "d"
     )

# 
summary(margins_model)

# Nearest neighbor PS matching
library(MatchIt)

# 
m_data <-  matchit(
    d ~ age + educ + black + hisp + nodegree + married + re74 + re75,
    data = dat3,
    method = "nearest"# ,
   # caliper = 0.2
    )


# Check the ballancing
summary(m_data)

matched_data <- match.data(m_data)

model1 <- feols( data = matched_data, re78 ~ treated)
model2 <- feols( data = matched_data, re78 ~ treated +age + educ + black + hisp + nodegree + married)
fixest::esttable( list(model1, model2))

