

# Based on the Mixtape: https://mixtape.scunning.com/05-matching_and_subclassification#bias-correction

# The Propensity score mathcing method

# CIA
# Sceptical to the CIA-assumptions.
# More converned about selection on unobserveable than selection on observable.

# Estimate the Probl. of being to treatment and collapse the predictied value into single scalar = Propensity Score
# Comparing T and C by the PS- value

# The idea with propensity score methods is to compare units who, based on observables, 
# had very similar probabilities of being placed into the treatment group even though those units differed with regard to actual treatment assignment

# If conditional on, two units have the same probability of being treated, then we say they have similar propensity scores,
# and all remaining variation in treatment assignment is due to chance. 

# Interpretation: two units A and B have the same propensity score of 0.6, but one is the treatment group and one is not, and the conditional independence assumption credibly holds in the data, then differences between their observed outcomes are attributable to the treatment.

# CS
# common support assumption. Common support simply requires that there be units in the treatment and control group across the estimated propensity score

# Example NSW jobb training program
library(tidyverse)
library(data.table)
library(fixest)

# Mixtape data
# Randomized data
dt <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/nsw_mixtape.dta") |> as.data.table()

stat <-  dt[ , .( income = mean(re78) ) , by = treat]

stat$income[stat$treat == 1] - stat$income[stat$treat == 0] # Effect in the randomized data
 
# With non-experimental data (non random data)-------------------

dt1 <- dt[ treat == 1,, ]

# The control group suffers from extreme selection bias since most Americans would not function as counterfactuals for the distressed group of workers who selected into the NSW program
dt_cps <-haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/cps_mixtape.dta")

dt_dw_cps <- bind_rows(dt1, dt_cps)

# Prepering data for PS-calcualtion
dt_dw_cps2 <- dt_dw_cps[ ,
                         let(
                           age2 = age^2,
                           age3 = age^3,
                           educ2 = educ*educ,
                           # If observed with income
                           u74 = ifelse( re74 == 0, 1, 0),
                           u75 = ifelse( re75 == 0, 1, 0)
                           ),
                         ][ ,
                          let(
                           inta1 = educ*re74,
                           re74_2 = re74^2,
                           re75_2 = re75^2,
                           inta2 = u74*hisp
                           ),
                         ]

# The propensity calcualtion
logit_nsw <- glm(
  treat ~ age + age2 + age3 + educ + educ2 + marr + nodegree + black + hisp + re74 + re75 + u74 + u75 + inta1,
  family = binomial(link = "logit"),
  data = dt_dw_cps2
  )

summary(logit_nsw)
# Fitted
dt_dw_cps2$ps <- logit_nsw$fitted.values

# Balancing testing ----------------------

# Common support
# Common support requires that for each value of, there is a positive probability of being both treated and untreated

# Much higher score for treated than control group
dt_dw_cps2[ , .(psore = mean(ps)), by = treat]

# Histogram inspection
ggplot( data = dt_dw_cps2, aes( x = ps, fill= factor(treat) ) ) + geom_histogram( aes( y = ..density..) ) + facet_wrap(~treat, scales = "free")

# These two simple diagnostic tests show what is going to be a problem later when we use inverse probability weighting.

# These individuals are younger, less likely to be married, and more likely to be uneducated and a minority. The lesson is, if the two groups are significantly different on background characteristics, then the propensity scores will have grossly different distributions by treatment status. We will discuss this in greater detail later.

# All of the information from the  ma trix has been collapsed into a single number: the propensity score.


# Weighting the PS for calculating the effect -----------------------------

# Non normalized weights
dt_dw_cps2[ ,
            let(
              d1 = treat/ps,
              d0 = (1-treat)/1-ps,
              y1 = treat*re78/ps,
              y0 = (1-treat)*re78/(1-ps)
              ),
            ][
            ]

#
N <- nrow(dt_dw_cps2)
s1 <- sum(dt_dw_cps2$d1)
s0 <- sum(dt_dw_cps2$d0)


dt_dw_cps2$ht <- dt_dw_cps2$y1 - dt_dw_cps2$y0

# ATT: Non normaized -11 877
mean(dt_dw_cps2$ht)

# Normaized
dt_dw_cps2[ ,let(
  y1n = (treat*re78/ps)/(s1/N),
  y0n = ((1-treat)*re78/(1-ps))/(s0/N),
  norm = y1n - y0n
  ),]

mean(dt_dw_cps2$y1n)
mean(dt_dw_cps2$y0n)
mean(dt_dw_cps2$norm)

dt_dw_cps2[ , .( yn1 = mean(y1n), yn0 = mean(y0n) ),]

## Trimming th PS
dt_dw_cps3 <- dt_dw_cps2[ dplyr::between(ps, 0.1, 0.9),,]


dt_dw_cps3[ ,
            let(
              d1 = treat/ps,
              d0 = (1-treat)/(1-ps),
              y1 = treat*re78/ps,
              y0 = (1-treat)*re78/(1-ps)
            ),
][
]

#
Nt <- nrow(dt_dw_cps3)
s1t <- sum(dt_dw_cps3$d1)
s0t <- sum(dt_dw_cps3$d0)


dt_dw_cps3$ht <- dt_dw_cps3$y1 - dt_dw_cps3$y0

# ATT: Non normaized -11 877
mean(dt_dw_cps3$ht)

# Normaized
dt_dw_cps4 <- dt_dw_cps3[ ,
            let(
              y1 = (treat*re78/ps),
              y0 = ((1-treat)*re78/(1-ps)),
              ht = y1 - y0,
              ny1 = (treat*re78/ps)/(s1t/Nt),
              ny0 = ((1-treat)*re78/(1-ps))/(s0t/Nt),
              norm = ny1 - ny0
              ),]

mean(dt_dw_cps4$ht)
mean(dt_dw_cps4$norm)

dt_dw_cps4[ , .( yn1 = mean(ny1), yn0 = mean(ny0) ),]




