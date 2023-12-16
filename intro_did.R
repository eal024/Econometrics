
# Packages
library(tidyverse)
library(fixest)

# DID -- empircal strategi to investeage causal inferance

# Basic example from: https://bookdown.org/cuborican/RE_STAT/difference-in-differences.html

b0 <- 5
# β1: Average change in y from the first to the second time period that is common to both groups
b1 <- 1
# β2: Average difference in y between the two groups that is common in both time periods
b2 <- 1
# βdid : Average differential change in y from the first to the second time period of the treatment group relative to the control group
b_did <- 2

# Groupe c
grc0 <- b0
grc1 <- b0 + b1

# Groupe treated
grt0 <- b0 + b2
grt1 <- b0 + b2 + b1 + b_did
pt  <-  b0 + b2 + b1

# Line1 data
line1 <- tibble( 
    gr = c("c", "c", "t", "t"),
    time = c(0,1, 0,1),
    value = c(grc0, grc1, grt0, grt1)
    ) 

# Paralell trend
line_pt <- tibble( 
    time = c(0,1),
    value = c(grt0, pt  ) 
)

# The graph
line1 |> 
    ggplot( aes(y = value, x = time, color = as.factor(gr))
    ) +
    geom_line() +
    geom_line( 
        data = line_pt,
        aes( y = value, x = time),
        linetype = 2,
        inherit.aes = F
    )  +
    geom_path( 
        data = tibble( value = c(7,grt1), time = c(1,1 )), aes( y = value, x = time),
        inherit.aes = F,
        linetype = 2,
        color = "red"
    )

## DiD, 2 ways in R

# 1) data: Country, year treated from 1994 for some countries
panel1 <- haven::read_dta("http://dss.princeton.edu/training/Panel101.dta") #  write_csv(panel1, "data/panel1.csv")


# Adjust the data
panel2 <- panel1 |> 
    mutate( 
        country = LETTERS[country],
        y    = y/10^6,  # Now in 1000
        time = ifelse( year >= 1994, 1, 0),
        treated = ifelse( country %in% c("E","F","G") , 1, 0),
        did = time*treated
    )

# 1) Base R
model1 <- lm( y ~time + treated + did, data = panel2)

# 2) fixest
model2 <- fixest::feols( y ~did | country + year, data = panel2)

# 3) lm with lm_robust (robust standard error)
model3 <- estimatr::lm_robust(y ~time + treated + did, data = panel2)

# Print the result
modelsummary::modelsummary( 
    list(
        model1 = model1,
        model2 = model2,
        model3 = model3
        ))

coeftest(model1, vcov = vcovHC(model1, type = "HC3"))

## Event study-----------------------

# Event study: Estimated effect before and after the start of treatment. 
# Explainted. treatment at year 1994. event coef 95 = did95-did94 

# Event study effect in the fixest packages
event <- feols( y ~ i(year, treated, 1994)| country + year, data = panel2 )

# The lm model:
model_lm_evnt <- panel2 |>
    mutate( d = did*year ) |> 
    lm( 
        formula =  y ~ I(factor(year)) + factor(country)  +  I( factor(d)) 
        ) 

# Substrack the effekt, e.g did95-did94 
lmevent <- tibble( 
    names = names(coef(model_lm_evnt)),
    coef = coef(model_lm_evnt)
    ) |> 
    filter( str_detect(names, "d")) |> 
    mutate( coef_diff_since_start = coef - coef[names == "I(factor(d))1994" ]
    )

# Compare
lmevent$coef_diff_since_start[ lmevent$names == "I(factor(d))1995"]
(ev_coef <- event |> coef())[names(ev_coef) |> str_detect("1995") ]


coefplot(event)




