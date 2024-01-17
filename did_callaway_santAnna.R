
# DID Callway and Ssant`Anna (2021)
# https://asjadnaqvi.github.io/DiD/docs/code_r/07_did_r/

# Paper:  https://www.sciencedirect.com/science/article/abs/pii/S0304407620303948
# Packages: 

# Provide the ATT
# install.packages("did")

# https://bcallaway11.github.io/did/articles/index.html
library(did)
library(tidyverse)

# No treat == Inf
# The basic set up, 2x2 DID# Test easy example

# Two groups
notreat <- dat$id[ dat$first_treat == Inf ] |> unique()
treated <- dat$id[ dat$first_treat == 12 ]  |> unique()

# 2x2 time set up DID
dat1 <- dat |> 
    filter( 
        id %in% c(notreat, treated),
        # Keeping two extra periods (from the 2x2)  for the next example
        time %in% c(11:14)
    ) |> 
    mutate(
        t = ifelse( id %in% treated, 1, 0),
        post = ifelse( time > 11, 1, 0) 
    )

# The DID
lm(data = dat1 |> filter( time %in% c(11:13)), y ~ post + t  + I(t*post))

# did packages ----------------------

# Model
cs21 = att_gt(
    yname         = "y",
    tname         = "time",
    idname        = "id",
    gname         = "first_treat",
  # xformla       = NULL,            # No additional controls in this dataset 
    control_group = "notyettreated", # Too few groups for "nevertreated" default
    clustervars   = "id", 
    data          = dat |> filter( id %in% c(treated, notreat), time %in% c(11:13) )
    )

# The estimate
aggte(cs21)


# Two periods
dat2 <- dat |> 
    filter( 
        time %in% c(11:14),
        id %in% c(treated, notreat )
        ) |> 
    mutate(
        post = time > 11,
        t =  ifelse(id %in% treated, 1, 0),
        tt = factor(t*time*post) 
    )

#
model2 <- lm(data = dat2, y ~ post + t  + tt)

# The DID-coef
df_coef <- model2 |> broom::tidy() |> filter( str_detect(term, "tt1\\d"))

mean(df_coef$estimate) # DID - 2X2 is mean each period

cs22 = att_gt(
    yname         = "y",
    tname         = "time",
    idname        = "id",
    gname         = "first_treat",
  # xformla       = NULL,            # No additional controls in this dataset 
    control_group = "notyettreated", # Too few groups for "nevertreated" default
    clustervars   = "id", 
    data          = dat |> filter( id %in% c(treated, notreat), time %in% c(11:14) )
    )

aggte( cs22) # Group effect
aggte( cs22, type = "dynamic") # The effect across periods
aggte( cs22)$att.egt  

# 
ggdid(cs22, title = "(cs)did") # The dynamic effect

#
# df_coef |>
#    filter( 
#        term %in% c("tt12", "tt13")
#    ) |> 
#    ggplot( 
#        aes( x = term, y = estimate)
#    ) +
#    geom_point(size = 4) +
#    geom_hline( yintercept = 0, linetype = 2)

## Several time periods, two groups
treated <- dat$id[ dat$first_treat %in% c(12,16) ]  |> unique()

cs23 = att_gt(
    yname         = "y",
    tname         = "time",
    idname        = "id",
    gname         = "first_treat",
  # xformla       = NULL,            # No additional controls in this dataset 
    control_group = "notyettreated", # Too few groups for "nevertreated" default
    clustervars   = "id", 
    data          = dat |> filter( id %in% c(treated, notreat), time %in% c(11:17) )
    )

aggte(cs23)
aggte(cs23, type = "dynamic")
aggte(cs23, type = "calendar")


treated <- dat$id[ dat$first_treat %in% c(16) ]  |> unique()

cs24 = att_gt(
    yname         = "y",
    tname         = "time",
    idname        = "id",
    gname         = "first_treat",
  # xformla       = NULL,            # No additional controls in this dataset 
    control_group = "notyettreated", # Too few groups for "nevertreated" default
    clustervars   = "id", 
    data          = dat |> filter( id %in% c(treated, notreat), time %in% c(15:17) )
    )
    

aggte(cs24)

dat3 <- dat |> 
    filter( 
        id %in% c(notreat, treated),
        # Keeping two extra periods (from the 2x2)  for the next example
        time %in% c(15:18)
    ) |> 
    mutate(
        t = ifelse( id %in% treated, 1, 0),
        post = ifelse( time > 15, 1, 0),
        tt = factor( time*t*post)
    )

# The DID
lm(data = dat3 |> filter( time %in% c(15:17)), y ~ post + t  + I(t*post))

cs25 = att_gt(
    yname         = "y",
    tname         = "time",
    idname        = "id",
    gname         = "first_treat",
  # xformla       = NULL,            # No additional controls in this dataset 
    control_group = "notyettreated", # Too few groups for "nevertreated" default
    clustervars   = "id", 
    data          = dat |> filter( id %in% c(treated, notreat), time %in% c(15:18) )
    )
    

aggte(cs25, type = "dynamic")


lm(data = dat3 |> filter( time %in% c(15:18)), y ~ post + t  + tt )

# Estimate - t*post, first year
8.1164+-0.3663
15.077+(-.03663)
