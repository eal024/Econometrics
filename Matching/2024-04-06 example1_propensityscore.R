
# packages
library(tidyverse)

# data
data_path <- "https://github.com/scunning1975/mixtape/raw/master/" 
data <- haven::read_dta("https://github.com/scunning1975/mixtape/raw/master/nsw_mixtape.dta")  # vroom::vroom_write(data, "data/nsw_mixtape_lalonda.csv")

# groups mean effect of treatemnt.
# The data is RCT, so the effect is the true effect.
earning_stat <- data |> 
    group_by(treat) |> 
    summarise(
        `earnings 74` = mean(re74),
        `earnings 75` = mean(re75),
        `earnings 78` = mean(re78)
    )

# Earnings 1978, caused by traningprogram: 1794$ (in the data)
earning_stat$`earnings 78`[earning_stat$treat == 1] - earning_stat$`earnings 78`[earning_stat$treat == 0]



## 2) Using non experimental data to calcualte the effect from the program:
cps_data <- haven::read_dta(paste0(data_path, "cps_mixtape.dta")) # vroom::vroom_write(cps_data, "data/nsw_mixtape_lalonda_cps_nonranodm.csv")


# Merging trated and the non random data
df <- 
    data |> 
    # Keeping only treated from RCT-data
    filter( treat == 1) |> 
    bind_rows( cps_data )

# Long data
df_long <- df |> select(treat, age:re78) |> pivot_longer( -treat)

# look at the distribition to the data
ggplot(
    data = df_long,
    aes( 
        x = value,
        color = factor(treat)
        )  
    ) +
    geom_density( alpha = 0.5) +
    facet_wrap( 
        ~ factor(name), scales = "free"
    ) 

# Fit the Propensity score
# Keeping only treated from the RCT-data and the CPT-data

# 1) First prepare the data
df11 <- df |> 
    mutate(
        agesq = age^2,
        agecube = age^3,
        educsq = educ^2,
        # Unemploey
        u74 = ifelse( re74 == 0, 1, 0),
        u75 = ifelse( re75 == 0, 1, 0),
        edu_re74 = educ*re74,
        re74sq = re74^2,
        re75sq = re75^2,
        u74_hisp = u74*hisp
    ) # Prepered data: vroom::vroom_write(df2,"data/lalonda_prepered.csv")

# 2) calcualte the PS
df1 <- df |>
    mutate(
        agesq = age^2,
        agecube = age^3,
        educsq = educ*educ,
        u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
        u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
        interaction1 = educ*re74,
        re74sq = re74^2,
        re75sq = re75^2,
        interaction2 = u74*hisp
    )

model1 <-  glm(treat ~ age + agesq + agecube + educ + educsq + 
                marr + nodegree + black + hisp + re74 + re75 + u74 +
                u75 + interaction1,
                family = binomial(link = "logit"), 
                data = df1
                )

summary(model1)

# Predict values
df2 <- df1 |> mutate( ps = predict(model1, type = "response"))

# PS grouped by data id
# The non random sample has a much lower value: -- means lower Prop. of being treated
df2 |> 
    group_by(treat) |> 
    summarise(
        ps = mean(ps, na.rm= T)
    )

# Plotting the PS
df2 |>
    ggplot(aes( x = ps, color = factor(treat)))  +
    geom_density( alpha = 0.5) +
    facet_wrap(~i, scales = "free") 

# The probability of treatment is spread out across the units in the treatment group, but there is a very large mass of nearly zero propensity scores in the CPS
# These two simple diagnostic tests show what is going to be a problem later when we use inverse probability weighting.
# the common support assumption. That is, 0 < P(D = 1|x) < 1 This simply means that for any probability, there must be units in both the treatment group and the control group. The conditional independence assumption simply means that the backdoor criterion is met in the data by conditioning on a vector 


## 3) Weighting on the PS

# Horvitz and Thompson (1952)
# ATE: E[ Y*(D-p(x))/P(x)*(1-p(x))]
# ATT: E[y1-y0|D =1] = (1/P(D=1))*E[E*(D-p(x))/(1-p(x)) ]

# Sample version: Obtained by a two-step estimation procedure
# 1) estimate PS 2) sample version of ATE or ATT
# Ex. 
df3 <- df2 |> select(  treat, ps, yi = re78 )

# ATT
n_t <- sum(df3$treat==1)

# Calcualte
df3 |>
    mutate( 
        gamma_ate = yi*(treat-ps)/(ps*(1-ps)),
        gamma_att = yi*(treat-ps)/((1-ps))
        ) |> 
    summarise( 
        ate = sum(gamma_ate)*(1/n()),
        att = sum(gamma_att)*(1/n_t)
    )


## Millimet and Tchernis (2009)
# Normalized estimator

N <- nrow(df3)
s1 <- sum(df3$treat/df3$ps)
s0 <- sum((1-df3$treat)/(1-df3$ps))

df4 <-  df3 |> 
    mutate(
       y11 = (yi*treat/ps),
       y00 = yi*(1-treat)/(1-ps),
       ht =  y11 - y00,  
       y1 = (yi*treat/ps)/(s1/N),
       y0 =  yi*(1-treat)/(1-ps)/(s0/N) ,
       norm = y1 - y0
    )

mean(df4$norm)
mean(df4$ht)

## Trimming the data
# There is a problem with the control-group; that there is a cluster at P(x) near 0. 
# For improving the method, the extreme values need to be trimed.
# Crump, Richard K., V. Joseph Hotz, Guido W. Imbens, and Oscar A. Mitnik. 2009. “Dealing with Limited Overlap in Estimation of Average Treatment Effects.” Biometrika 96 (1): 187–1999.

df5 <- df4 |> 
    filter( between(ps, 0.1, .9)
    ) 

N <- nrow(df5)
s1 <- sum(df5$treat/df5$ps)
s0 <- sum((1-df5$treat)/(1-df5$ps))

# Normalizing again.
df5 <- df5 |> 
    mutate(
       y11 = (yi*treat/ps),
       y00 = yi*(1-treat)/(1-ps),
       ht =  y11 - y00,  
       y1 = (yi*treat/ps)/(s1/N),
       y0 =  yi*(1-treat)/(1-ps)/(s0/N) ,
       norm = y1 - y0
    )

mean(df5$norm)
mean(df5$ht)

