

# Based on a go through: https://asjadnaqvi.github.io/DiD/docs/code_r/07-twfe_r/

library(ggplot2)
library(fixest)

# data
df <- data.frame( 
    id = rep(c(1,2), times = 2),
    tt = rep(c(1,2), each = 2)
 )


df <- df |>
    within(
        {
    D = ifelse(id == 2 & tt == 2, 1, 0)
    y = id + 3*tt + 2*D 
        }
    )


fn_graph <- function(df, vline){
    df |> 
    ggplot( 
        aes( y = y, x = tt, color = factor(id)) 
        ) +
    geom_line() +
    geom_vline(xintercept = vline, lty = 2) +
    geom_point() +
    theme_minimal( base_size = 15)
}

# Ploting the data
fn_graph( df = df, vline = 1.5) 

# The 2x2 case
# Give  the did-coef = 2
lm( data= df, y ~ id + tt + I(id*tt))  |> coef()


## More time periods
df2 <-  
    data.frame(
        id = rep(c(1,2), times = 10), # 10 time period
        tt = rep(c(1:10), each = 2)  # each time period repeated 1 
    )


df2 <- df2 |>
    within(
        {
    D = ifelse(id == 2 & tt > 5, 1, 0)
    y = id + 3*tt + 3*D 
        }
    )

fn_graph( df = df2, vline = 5.5) 

## LM
lm( data = df2, y ~ id + tt + factor(D)) |> summary()

## fixest packages
fixest::feols( y ~ D | id + tt, data = df2)

# Give the coef-result 3

## Hetoregenic treatment effect:
    ## Adding more unit
    ## Different treatment effect

df3 <- data.frame(
    id = rep(1:3, times = 10),
    tt = rep(1:10, each = 3)
    ) |>
    within({
        D     = ifelse(id >= 2 & tt > 5, 1, 0)  # id 2 and 3 and after tt 5
        # id 3 has 4, id2 has 2 (average 3)
        treat_effect = ifelse( D == 1 & id == 3, 4, ifelse(D == 1 & id == 2, 2, 0) )
        y     = id + 1 * tt + treat_effect * D
    })


fn_graph( df = df3, vline = 5.5)

# TWFE model (average effect 3)
fixest::feols( y ~ D | id + tt, data = df2)

# All combination of time and unit fixed effects
feols( y ~D | mvsw(tt, id), data = df3)

lm( data = df3, y ~ id + tt + factor(D)) |> summary()

# lm( y ~ id, data = df3)
# lm( y ~ tt, data = df3) 

## The problem with the TWFE-model

# Data
df4 <- data.frame(
    id = rep(1:3, times = 10),
    tt = rep(1:10, each = 3)
    ) |>
    within({
        # id 2 treated t= 5 , id 3 treated t = 7
        D     = ifelse(id == 2 & tt >= 5, 1, ifelse(id == 3 & tt > 7, 1, 0) )  
        # id 3 has 4, id2 has 2 (average 3)
        treat_effect = ifelse( D == 1 & id == 3, 4, ifelse(D == 1 & id == 2, 2, 0) )
        y     = id + 1 * tt + treat_effect * D
    })


# Graphical illustration of the problem:
fn_graph( df = df4, vline = c(4.5, 7.5))

#  Staggered treatment = Hetro treatetment effect
#  The staggered treatment timing introduces new confounding factors above the simple unit and time fixed effects.
#  to recover the actual ATT, we need to average out time and panel effects for treated and non-treated observations. 

# The TWFE
feols( y ~D | id + tt, data = df3)

# 2.9! What does this mean? 
# We have two treatments happening at different times with different treatment effects. Therefore the definition of “pre” and “post” is not clear anymore. Neither is “untreated” versus “treated”. 











