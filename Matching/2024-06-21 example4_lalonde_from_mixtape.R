

# https://mixtape.scunning.com/05-matching_and_subclassification

library(tidyverse)
library(fixest)


path_data <- "https://github.com/scunning1975/mixtape/raw/master/"
df <- haven::read_dta( paste(path_data, "nsw_mixtape.dta", sep = "") )  # The randomized dataset

# Balanced properites
df |> group_by(treat) |> summarise_all(mean)

# For reuse of the data transformation
fn_transform <- function(dat){
    dat |> 
        mutate(
            age2 = age^2,
            age3 = age^3,
            educ2 = educ^2,
            # This is done differently by UiO-seminar example.
            u74 = re74 == 0,
            u75 = re75 == 0,
            re74_2 = re74^2,
            re75_2 = re75^2,
            interaction1 = educ*re74,
            interaction2 = u74*hisp
        )
}

# Transform varibales
df <- fn_transform(df)    

# First OLS regrression
xvars <- c("age" ,"age2", "educ" , "educ2", "black", "hisp", "marr", "nodegree", "re74", "re75")

model1 <- feols( data = df, fml = as.formula("re78 ~ treat"))
model2 <- feols( data = df, fml = as.formula( paste0( "re78 ~ treat +", paste(xvars, collapse = " + ") ) ) )

esttable( list(model1, model2)) # The effect from the program is round  1 795$. The result is based on randomized data.

# How does the result get effected by using popualtion as controls`?`
# 2. controlgrop from Current Population Survey (CPS).
# It is very important to stress that while the treatment group is an experimental group, the control group now consists of a random sample of Americans from that time period.

df2 <- haven::read_dta( paste(path_data,"cps_mixtape.dta", sep = "") )

# data joining: data with NSW
# Keeping only treated from the NSW-data
df3  <- 
    # Bind data
    bind_rows(
        df |> filter( treat == 1), # NSW
        df2 # CPS
        ) |>
        fn_transform() # Transfrom data

# First estimating the PS-model:
xvars     <- c("age" ,"age2","age3" ,"educ" , "educ2", "marr", "nodegree","black", "hisp", "re74", "re75", "re74_2", "re75_2","interaction1", "interaction2")
string_fm <- paste("treat ~", paste(xvars, collapse = " + ") )
logit <- glm( data = df3,  formula = as.formula(string_fm), family = binomial( link = "logit") )

summary(logit)

# Look how well the model predict
pred <- predict(logit,  type  = "response")
thh <- 0.5
est <- ifelse( pred > thh, 1, 0)
test <- tibble( treat = df3$treat, pscore = pred, est =  est)

# Telling how wel the model predicts
caret::confusionMatrix(as.factor(est), as.factor(df3$treat) )# Accuracy heigh

# Testing assumption about CS:
# Pscore by group: treatment group has higher Pr. for being treated.
test |> group_by(treat) |> summarise( pscore = mean(pscore))

# Can also see this in a histogram
ggplot(
    data = test,
    aes( x = pscore, fill = factor(treat))
 ) +
 geom_histogram( ) +
 facet_wrap(~factor(treat), scales = "free") +
 labs(
    title = "Pr(Treat = 1|X) by treated and control group",
    subtitle = "Contrl (D = 0) has low P for being treated" 
    ) +
    lims( x = c(-0.1,1))

# These two simple diagnostic tests show what is going to be a problem later when we use inverse
#  probability weighting. The probability of treatment is spread out across the units in the treatment group,
#  but there is a very large mass of nearly zero propensity scores in the CPS. How do we interpret this? 
#  What this means is that the characteristics of individuals in the treatment group are rare in the CPS sample.
#  This is not surprising given the strong negative selection into treatment.

# 2) Non normalized mean (effect) and normalized
N <- nrow(df3)
df3$pscore <- pred

#
df4 <- df3 |>
    select( treat, pscore) |> 
    mutate( 
        d1 = treat/pscore,
        d0 = (1-treat)/(1-pscore)
        )

nsw_dw_cpscontrol|> select(treat, pscore, d1,d0) |> group_by(treat) |>  summarise_all(mean)
df4 |> select(treat, pscore, d1,d0) |> group_by(treat) |>  summarise_all(mean)

mean(nsw_dw_cpscontrol$d1)
mean(df4$d1)

s1 <- sum(df4$d1) # The PScore actulay make a difference. Small adjustment had impact on the result
s0 <- sum(df4$d0)

# Non normalized weights
df5 <- df3 |> 
    group_by(i = 1:n(), treat) |> 
    transmute(
        y1 = treat*re78/pscore,
        y0 = (1-treat)*re78/(1-pscore),
        ht = y1 - y0
    ) |> 
    ungroup()


# Manual with norm. weights
df6 <- df3 |> 
    group_by(i = 1:n(), treat) |> 
    transmute(
        y1 = (treat*re78/pscore)/(s1/N),
        y0 = (re78*(1-treat)/(1-pscore))/(s0/N),
        norm = y1 - y0
    )

mean(df5$ht)
mean(df6$norm)

## The result is far from randomized data.
# Because the CS dont holds. 
# Trimming the data: The CS become satizf.
df7 <- df4 |>
    mutate( re78 = df3$re78) |> 
    filter( 
        pscore > 0.1,
        pscore < 0.9 
    )

# New numbers
NN <- nrow(df7)
s11 <- sum(df7$d1)
s00 <- sum(df7$d0)

# Calcuate
df8 <- df7 |>
    mutate(
        y1  =  treat * re78/pscore,
        y0  =  (1-treat) * re78/(1-pscore),
        ht  =  y1 - y0,
        ny1 =  (treat*re78/pscore)/(s11/NN),
        ny0 =  ((1-treat)*re78/(1-pscore))/(s00/NN),
        norm = ny1 - ny0
        )

# Closer to the real effect
mean(df8$ht)
mean(df8$norm )



## Example 2: Nearest neighbor matching on the PS