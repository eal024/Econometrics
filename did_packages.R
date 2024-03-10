

library(did)
library(tidyverse)

dat

# The DID

# 2x2 time set up DID

# Two groups
notreat <- dat$id[ dat$first_treat == Inf ] |> unique()
treated <- dat$id[ dat$first_treat == 12 ]  |> unique()

dat1 <- dat |> 
    filter( 
        id %in% c(notreat, treated),
        # Keeping two extra periods (from the 2x2)  for the next example
        time %in% c(11:13)
    ) |> 
    mutate(
        t = ifelse( id %in% treated, 1, 0),
        post = ifelse( time > 11, 1, 0) 
    )
lm(data = dat1 |> filter( time %in% c(11:13)), y ~ post + t  + I(t*post))

cs21 <- att_gt(
    data = dat |> 
        filter(
            id %in% c(notreat, treated),
            # Keeping two extra periods (from the 2x2)  for the next example
            time %in% c(11:13)
        ),
    yname = "y",
    tname = "time",
    idname = "id",
    gname= "first_treat",
    control_group = "notyettreated",
    allow_unbalanced_panel = F,
    clustervar = "id"
    )

aggte(cs21)


# Include all

cs22 <- att_gt(
    data = dat,
    yname = "y",
    tname = "time",
    idname = "id",
    gname= "first_treat",
    control_group = "notyettreated",
    allow_unbalanced_panel = F,
    clustervar = "id"
    )

aggte(cs22)
