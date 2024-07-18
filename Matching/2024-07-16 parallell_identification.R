
# Seminar econ -- Partial Identification

# Packages
library(tidyverse)
library(fixest)

# data Head Start
# Head Start is a major federally funded preschool program in the U.S.
# targeted at children from low-income parents and provides these children with schooling
df <- haven::read_dta("data/headstart.dta") 

# Conventional names
df$D <- df$headstart|> as.numeric()
df$Y <- df$highschool_child |> as.numeric()

# 1) Estimate the effect Pr. of obtaining high school degree w. OLS
model1 <- feols( data = df, highschool_child ~ headstart)
model2 <- feols( data = df, highschool_child ~ headstart + edu_parent)

# Result
fixest::esttable(list(model1, model2))

# Can the estimate be interpreted as causal?
# Should be suspicious. Can be interpreted as causal effect if there is no omitted variable.
# Omitted variable: Variable that is relevant for getting a high school degree and releted to selection into 
#                   into treatment. Since T depend on low-income backgruound, and this must likely is corr. to 
#                   education, OVB is likely.   

# 2) The share of children with high school degree for:

# a) Head start participans
df |> aggregate( highschool_child ~headstart, FUN = mean)
yd1 <- mean(df$Y[df$D == 1])
yd0 <- mean(df$Y[df$D == 0])


# b) Antall deltagere
df |> aggregate( highschool_child ~headstart , FUN = length)

df |> filter( D == 0) |> summarise( m = mean(Y))


# 3) The worst case bounds
ymin <- 0
ymax <- 1
p <- sum(df$D == 1)/nrow(df) 
`1-p` <- 1-p

# Y0: yd1 not observed (p)
y0_low  <- ( ymin*p + yd0*`1-p` )
y0_high <- ( ymax*p + yd0*`1-p` )

# Y1: yd0 not observed (1-p). Replace yd0 with min and max
y1_low  <- ( ymin*`1-p` + yd1*p )
y1_high <- ( ymax*`1-p` + yd1*p )

# Lower bound:
y1_low - y0_high

# Upper bound
y1_high - y0_low


# 4)

# 6) Compute MTS lower and upper bounds

# a) E[y(0)]: Mean potential Pr. of completing High chool with no Head start
ey0 <- c( "y0_low" = `1-p`*yd0 + ymin*p, "y0_high" = mean( df$Y[df$D== 1]))


# b) E[y(1)]
ey1 <- c( "y1_low" = mean( df$Y[df$D== 1]), "y1_high" = p*yd1 + ymax*`1-p`)

# ATE: LB(Y1) - UB(Y0) < ATE < UB(Y1) - LB(B0)
ate <- c( 
    "ate_low"   = ey1["y1_low"] - ey0["y0_high"],
    "ate_high" = ey1["y1_high"] - ey0["y0_low"]
    ) 

# 7) Variable edu_parents 
fn_group_sum <- function(gr_var) {
    df |>
    group_by( {{gr_var}}) |> 
    summarise(
        c1_p  = sum(D == 0)/n(),
        cp    = sum(D == 1)/n(),
        yd0   = mean(Y[D == 0]),
        ymin  = min(Y),
        ymax  = max(Y),
        y0_low =  c1_p*yd0 + ymin*cp,
        y0_high = mean(Y[D == 1]),
        y1_low =  mean(Y[D == 1]) ,
        y1_high = mean(Y[D == 1])*cp + ymax*c1_p,
        `ate_l` = y1_low - y0_high,
        `ate_h` = y1_high - y0_low  
    ) 
}

# calculation of the MTS-data, grouped by parents education.
df_iv <- fn_group_sum(gr_var = edu_parent)  

 
df_iv_long <- df_iv |> pivot_longer(-edu_parent)

df_iv_long |> 
    filter( str_detect( name, "y0_")) |> 
    ggplot( ) +
        aes(
            x = edu_parent,
            y = value,
            fill = factor(name)
        ) +
    geom_line( ) +
    geom_point( color = "red", size = 5)  + 
    geom_line(
        data = tibble(
            edu_parent = c(4,5),
            value = df_iv$y0_high[df_iv$edu_parent == 4]
            ),
        aes(  
            x= edu_parent,
            y = value
        ),
         inherit.aes = F,
         color = "red",
         linetype = 2,
         size = 1
         )
         

# 9 The MTS-MIV upper and lower bound
df1 <- df_iv |> 
    transmute( edu = as.numeric(edu_parent), p = cp, y0_low, y0_high) 

df1 |> 
    mutate( )

# a) E[y0]
ivey0 <- c( "y0_low" = `1-p`*yd0 + ymin*p, "y0_high" = mean( df$Y[df$D== 1]))

# Last part:
# 8. Write down the MIV assumption that we are imposing when we would use parent’s

# The lower value for Pro. of potenial outcome (highschool) is no lower than the highest value for z <= z*
# (see the stipled lined in the plot)
# Same as the higher bound (y0-high): 

# level of education as a (positive) monotone instrument and explain this MIVassumption in words.
# 9. Use the results obtained in question 7) to compute MTS-MIV upper and lower bounds on

# Notes: How take the weigheted average over Z? /See page 22 
# (a) the mean potential probability of completing high school with no Head Start as potential treatment E[y(0)].

# (b) the mean potential probability of completing high school with Head Start as potential treatment E[y(1)].

# 10. Compute MTS-MIV lower and upper bounds on the average causal effect of Head  Start on the probability of obtaining a high school degree. Interpret the estimated lower and upper bounds




