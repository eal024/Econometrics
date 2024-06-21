
# Recap matching
library(tidyverse)

# 1) Exercise 1, Seminar 4, ECON5106

dat <- tibble::tibble(
    x  = c(1,1,1, 2,2,2, 3,3,4,5),
    d  = c(0,1,1,0,1,0,1,1,0,0),
    y  = c(10,15,20,25,30,30,25,35,50,55),
    id = c(1:10)
)

# a) Estimate causal effect T linear regression
modela <- lm( data = dat,  y ~ x + d)

summary(modela)

# What assumption need for gamma causal?
    # Assumtion of d to be causal
    # Non omiited variables, where cov(y,z) != 0, and c(d,z) != 0
    # x is exogen to d 
    # CIA and CS

# b) The ATT using the exact matched method

# 1) Filter only treated
df1 <- dat |> 
    filter( d == 1) 

# Use non treated as counterfactual
df_counter <- dat |> 
    filter( d == 0)  |> 
    group_by(x) |> # x is the characteristics
    summarise(
        y0 = mean(y) # Mean outcome as non treated
    )

#  Match 
df2 <- df1 |> 
    select( id, x, d, y1 = y) |> 
    # Keep only those with counterfact
    inner_join(
        df_counter, join_by(x)) |> 
    summarise(
        ef = y1 - y0
    )

mean(df2$ef)

## c) Why is y~x +D different from model? 

# OLS allows us to make comparisons across covariats
# Dont need common support.
# Downside: the lineartiy assumption may be poor 

# Avoid problem:
summary(lm(y ~ as.factor(x) + d, data =dat))

# What does OLS do:


## 2) Effect of uion membersship-------------------------

library(tidyverse)

# Membership union and the effect on wage.
dat <- haven::read_dta("data/nlsw88.dta")  |> mutate( lwage = log(wage))

skimr::skim(dat)

df_names <- tibble( n = names(dat), labels = map( dat, \(x) attr(x, "labels"))  )  

df_names <- df_names  |>
    mutate( 
        labels    = map( labels, \(x) if(is.null(x)){ "Ingen" }else{x} ),
        lab_names = map( labels, \(x) if(is.null(x)){ "Ingen"}else{ names(x) } )
        )

# a) OLS with and without controls: Why does the result differ
model1 <- lm( data = dat, lwage ~ union)
model2 <- lm( data = dat, lwage ~ union + ttl_exp + tenure + smsa + south + collgrad + as.factor(race) )
stargazer::stargazer( list(model1, model2), type ="text")

# b) Compare distribution between union and non union
var <- c("union", "south" ,"collgrad" ,"age", "race", "married", "grade", "smsa", "c_city")

# Variables
dat |> 
    filter(!is.na(union)) |> 
    select( var) |> 
    pivot_longer(-union) |> 
    summarise( 
        mean = mean(value,na.rm = T), .by = c(union,name),
        sd = sd(value,na.rm = T)
    ) |> 
    pivot_wider( names_from = union, values_from = c(mean, sd) ) |> 
    select( name, contains("_1"), contains("_0") )


## t-test of different variables

# cofoundings X distribution
dat |> 
    select(union, industry, married, collgrad, grade) |> 
    pivot_longer(-union) |> 
    group_by( union, value, name) |> 
    count() |> 
    group_by(name, union ) |> 
    mutate( andel = n/sum(n))  |> 
    ggplot(
        aes(y = andel, x = as.factor(value), fill = as.factor(union)) 
    ) +
    geom_col( position =  position_dodge2()) + 
    facet_wrap(~name, scales = "free") +
    labs( x = "Factor X", y = "Andel")


# Comparing union vs. non union workers
model_union      <- lm( data = dat, union ~ factor(industry)  +  grade + collgrad + hours) 
model_wage       <- lm( data = dat, lwage ~  factor(industry))
model_wage_union <- lm( data = dat, lwage ~  factor(union)) 

# Look at the result
stargazer::stargazer(
    list(union = model_union, wage = model_wage, wage_union = model_wage_union),
    type = "text"
    )


# Membership in union is correlated with industry 5, and industry 5 is correlated with higher wage.
# while union membership is also correlated with higher wage

# c) Use the weighting techinique to calcaulate the ATT

# 1) Propensity score
model_ps <- glm( data = dat, formula = union ~ ttl_exp + tenure + smsa + south + collgrad + factor(race), family = binomial("logit"))
# stargazer::stargazer(model_ps, type = "text")

# Predicted data
pred    <- predict( model_ps, newdata = dat, "response")
df_pred <- tibble( union = dat$union, predicted = ifelse( pred> 0.5, 1,0 ), p = pred )

# weigh the untreated
dat1 <- dat |>
    mutate( 
        union  = as.numeric(union),
        weight = pred/(1-pred),
        pred   = pred 
        ) |> 
    select( wage, lwage, union, weight, p = pred)  |> 
    filter(!is.na(union))

# Calculating the treatment effect----------------------

vec <- c(2,5)
vekt <- c(0.8,0.2)
0.8*2 + 0.2*5
weighted.mean(vec, w = vekt)
sum(vec)
sum(vekt)

dat2 <- dat1 |> mutate( w_lwage = ifelse(union == 1, 1*lwage, weight*lwage) ) |> na.omit()
#dat2 |> group_by(union) |>  summarise( antall = n(),  y1 = sum(w_lwage)/sum(weight), y11 = mean(lwage))

w_wage_union0 <- dat1 |> filter(union == 0) |> mutate( w_lwage = lwage*weight)  |> na.omit()
skimr::skim(w_wage_union0)
sum( w_wage_union0$w_lwage)/sum(w_wage_union0$weight)

dat_non <- dat2 |> filter( union == 0)  |> na.omit()
y10     <- weighted.mean(dat_non$lwage, w = dat_non$weight, na.rm= F)

# How each individual is weighed
weigthed_wage <- dat_non$lwage/dat_non$weight

dat_treat <- dat1 |> filter( union == 1) |> na.omit()
y11 <- mean(dat_treat$lwage)
y11 - y10 # The ATT (using the T = 0 as counterfactual)

data <- group_split(dat1, union)

ggplot() +
    geom_density( 
        data = dat2 |> filter( union == 1),
        aes( x = wage, color = "red")
        ) +
    geom_density( 
        data = dat2 |> filter( union == 0),
        aes( x = wage, color = "blue")
        ) +
    geom_density( 
        data = tibble( wage = weigthed_wage ),
        aes( x = wage)
        )   +
        ylim( c(0, .2))


# Example 3 -----------------------------

# Lalonda 1986: Investigated how non-experimental methods could reproduce experimental estimate.
# Data: National Support Work (NSW)

# You are interested in the average effect on Real Earnings in 1978 of the treatment for the treated.
# The variable -treated- identifies the observations that were treated (participate in a subsidized work experience program) in the NSW (from April 1975 to August 1977).

lalonde <- haven::read_dta("data/lalonde2.dta") |> 
    transform( sample_chr = labelled::to_character(sample) )

# g) Check if the data is consistent with randomization of the treatment
lalonde_nsw <- lalonde |> 
    subset( 
        sample_chr == "NSW",
        select = -c(sample_chr, sample)
    )

# Function for calculation sample statistics
fn_stat <- function(df){ 
    df |>
        summarise( across( 
            .cols = is.numeric,
            .fns = function(x)  list( mean = mean(x), min = min(x), max = max(x))) ) |> 
        mutate( stat = c("mean", "min", "max")) |> 
        relocate(stat, .before = treated ) 
    }

# statistics
stat <- group_split(lalonde_nsw, treated) |> map_df( \(x)fn_stat(x) ) |> unnest(cols = everything())

# Graphing the result
lalonde_nsw |> 
    pivot_longer(-treated) |> 
    ggplot( 
        aes( y = value, fill = as.factor(treated) )
    ) +
    geom_boxplot() +
    facet_wrap(~name, scales = "free")

# t-test
tbl <- tibble( 
    y = names(lalonde_nsw),
    data = list(lalonde_nsw )
    ) 


tbl1 <- tbl |> 
    mutate( t = map2( data, y, \(df,y){
        lm( data = df, formula = as.formula( paste( y, "~ treated")) ) |> broom::tidy()
        } )
        ) 

# t-test of different variables, based on treatemnt
# Should be insignificant and close to 0
tbl1 |> unnest(t) |> filter( term == "treated")

# h) The effect of treatement in the randomized data
# You are interested in the average effect on Real Earnings in 1978 of the treatment for the treated.
# The variable -treated- identifies the observations that were treated (participate in a subsidized work experience program) in the NSW (from April 1975 to August 1977).

library(fixest)
names(lalonde_nsw)

# Models
rmodel1 <- feols( data = lalonde_nsw, fml = re78 ~ treated, vcov = "HC1")
rmodel2 <- feols( data = lalonde_nsw, fml = re78 ~ treated + educ + black + married + nodegree + metro + hisp + kids18 + kidmiss, vcov = "HC1" )

# The results: the program gives between 886 and 750 increased wage.
fixest::esttable(list(rmodel1,rmodel2)) 

# j) Use Treated from NSW and comparing group from CPS
df2 <- as_tibble(lalonde) |>
    select( -c(dwincl,early_ra) ) |> 
    filter(
        sample_chr %in% c("CPS", "NSW"),
        treated %in% c(1, NA_real_)
        ) |> 
    mutate(
        treated = ifelse( is.na(treated), 0, treated)
    )

# how is the balancing suppert
tbl <- tibble( 
    y = names(df2),
    data = list(df2)
    ) 


tbl1 <- tbl |> 
    head(12) |> 
    mutate( 
        t = map2( data, y, \(df,y){
            lm( data = df, formula = as.formula( paste( y, "~ treated")) ) |> broom::tidy()
            } 
        )
    )  |> 
    select(-data) |> 
    unnest( t) |> 
    filter( term == "treated")

# Balancing graph: t-test of each variable. each variable is tested in the model: treated ~ var 
tbl1 |>
    ggplot( aes(y = estimate, x = y )) +
    geom_point( ) +
    geom_errorbar(
        aes( 
            ymin = estimate - std.error,
            ymax = estimate + std.error
        )
    ) +
    facet_wrap( ~y, scales = "free") +
    geom_hline( yintercept = 0, linetype = 2, color ="red")


# Sample statistics
group_split(df2, treated) |> map( \(x) fn_stat(x) |> unnest(treated:sample)) 


# Check for common support
df2 |> 
    select(treated, age, educ, kids18) |> 
    pivot_longer(-treated)  |> 
    ggplot(
        aes( x = value, fill = factor(treated))
    ) +
    geom_histogram( aes( y = ..density..), alpha = 0.5 ) +
    geom_density(alpha = 0.4) + 
    facet_wrap(~name, scales = "free")

# Treated are younger, fewer kids and lower education. 
# Age and kids seems not well balanced

# k) Nearest nabor PS matching

# 1) Propensity score
model_ps <- glm( data = df2, formula = treated ~ age + educ + black + married + nodegree + metro + hisp + re75 + re74 + kids18 + kidmiss, family = binomial("logit"))
# stargazer::stargazer(model_ps, type = "text")

# Predicted data
pred    <- predict( model_ps, newdata = df2, "response")
df_pred <- tibble( t= df2$treated, predicted = ifelse( pred> 0.5, 1,0 ), p = pred )

# Viz. common support
df_pred |> 
    filter( 
        p > 0.05, 
        p < 0.80
        ) |> 
    ggplot( aes( x = p, fill = factor(t) )) +
    geom_histogram( aes( y = ..density..), alpha = 0.4) 



# Matching nearest neighbor
library(MatchIt)

df3 <- df2  |> mutate( i = 1:n()) |>  left_join( df_pred |> mutate( i = 1:n()), join_by(i) )

model_nearest <- matchit(
    treated ~ age + educ + black + hisp + nodegree + married + re74 + re75, data = df3 |> filter( between(p, 0.3, 0.75 )),
    method = "nearest"
    )

# See the result
summary(model_nearest)
plot(model_nearest)

# Get the matched data
matched_data <- match.data(model_nearest)

model1 <- feols( data = matched_data, re78 ~ treated)
model2 <- feols( data = matched_data, re78 ~ treated +age + educ + black + hisp + nodegree + married)
fixest::esttable( list(model1, model2))


matched_data |>
     group_by(treated) |>
    summarise( 
        meanre78 = mean(re78)
    )

df3 |> 
    ggplot(
        aes( y = p)
    )



