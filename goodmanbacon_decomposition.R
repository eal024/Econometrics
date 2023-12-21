

# basesd on: https://asjadnaqvi.github.io/DiD/docs/code_r/06_bacon_r/
# Go through the logic of Andreq Goodman Bacon TWFE decompositon

library(ggplot2)
library(fixest)
library(bacondecomp)
theme_set( theme_linedraw())

# Graph the effect
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

# TWFE: Hard do draw a brigth line between pre and post.

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


# Show the individuals
fn_graph( df = df4, vline = c(4,7))


# The TWFE
feols( y ~D | id + tt, df4)

# D: 2.91
# D-represent: The weighted average of 4 distinct 2X2 groups

# The decomposition

# Difference between
# i) treated vs. untreated
# ii) early vs. late

# 1. t vs. unt.(c) early
dat1 <- df4 |> subset( id %in% c(1,2))

# 2. t. vs. c, late vs. early
dat2 <- df4 |> subset( id %in% c(1,3))

# 3. early t-early vs. t-late
dat3 <- df4 |> subset( id %in% c(2,3) & tt < 8 )

# 4. late treated vs. unt (c)
dat4 <- df4 |> subset( id %in% c(1,3) & tt > 4 )

data <- rbind(
    dat1 |> transform( role= ifelse(id == 2, "T", "C"), comp = "1. early t vs c"),
    dat2 |> transform( role= ifelse(id == 3, "T", "C"), comp = "2. late t vs c") ,
    dat3 |> transform( role= ifelse(id == 3, "T", "C"), comp = "3. late t early t"),
    dat4 |> transform( role= ifelse(id == 2, "T", "C"), comp = "4. late late t vs c")
)

# The effect (D) is the weighted average between the foure effects:
fn_graph( df = data, vline = c(4,7)) + facet_wrap( ~comp)

# The Goodman-Bacon decomposition isolates each of these 2x2 comparisons and 
# assigns them a weight, based on their relative coverage in the data (i.e., 
# how long each comparison lasts relative to the overall timespan, and how many units were involved)

# Implement the G-B-decomp:
bgd <- bacon( y ~ D, df4, id_var = "id", time_var = "tt")

bgd
weighted.mean(bgd$estimate,    w =bgd$weight ) # Gives the TWFE estimate

# Graph the different estimate:
bgd |> 
    ggplot( 
        aes( y = estimate, x = weight, color= factor(type), shape = factor(type))
    ) +
    geom_point(size = 16) +
    geom_hline( yintercept = 2.9, linetype = 2)


