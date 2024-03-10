

library(tidyverse)
set.seed(23456788)

# Boostrap seminar solution
# Marriage behavior US Black woman
# Model: P(married = 1|x) = normal(xB)

df <- haven::read_dta("data/cps09mar.dta")

# Look at the labels
labels <- tibble( var = names(df),  labels =  map(df, \(x) x |> attr("labels") )  
    ) |>  
    mutate( 
        labels = map(labels, \(x) if( is.null(x)){ tibble( lab = "NULL", value = 0)}else{ tibble( lab = names(x),  value = x)})
    ) |> 
    unnest( labels)


# data cleaning and prep
df1 <- df  |>
    filter( 
        race == 2,
        female == 1, # US black woman
        region == 2 # Midwest
    ) |> 
    mutate( marital = ifelse( marital %in% c(1,2,3), 1,0 ))

df1 |> skimr::skim()


# Model 
model <- glm( data = df1, marital ~ age + I(age^2) + education, family = binomial(link = "probit"))

summary(model)


# 3. Explain the bootstrap standard error

# 1) comute the B
# 2) drow the bootstrap sample with replacement 
# 3) comutes the B*r on simulated sample
# 4) Repeats R times

R <- 1000
bootstrap_samples <- map(1:R, \(x) df1[ sample(x = 1:nrow(df1), size = nrow(df1), replace = T), ])

fn_modeling <- function(data) {
    glm( data = data, marital ~ age + I(age^2) + education, family = binomial(link = "probit"))} 

# Models 
bootstrap_models <- map(bootstrap_samples,\(x) fn_modeling(data = x))

# Get the coefs
bootstrap_coefs <- map_dbl(bootstrap_models, \(x) coef(x)[4])

# Non paramteric bootstrap mean(bootstrap_coefs)
bootsd <- sd(bootstrap_coefs)
bootsd2 <- ((1/(R-1))*sum( (bootstrap_coefs-mean(bootstrap_coefs))^2 ) )^0.5


# 4) t-test with ML and bootstrap

coef_edu <- summary(model)$coef[4,1]
ml_st <- summary(model)$coef[4,2]

# i) ML
coef_edu/ml_st

# ii) bootstrap
coef_edu/sd(bootstrap_coefs)

# b) CI method

# percentile method
quantile( bootstrap_coefs, c(0.025, .975) )

# Percentile t-method
boot_t <- map_dbl( bootstrap_coefs, \(x) (x-coef_edu)/sd(bootstrap_coefs) )

t_s <- quantile( boot_t, c(0.025, 0.975))

# Percentile t-method
c( coef_edu + t_s[1]*ml_st, coef_edu + t_s[2]*ml_st )

# c) What is the difference between the methods?

# Percentil
# is very simple to implement.
# works well if the underlying distribution can be transformed to a normal.
# no theoretical evidence on whether it will be better than the standard CI from asymptotics.

# Percentile t-method
# Provide asymptotic refinement (are second order accurate) regardless of the true distribution

# 5) What is the bias corrected estimate?

2*coef_edu -mean(bootstrap_coefs)

# At what age is it most likely to be married?

b_age <- summary(model)$coef[2,1]
b_age2 <- summary(model)$coef[3,1]

# The age
(b_age)/((-1)*2*b_age2)



# Data
R <- 1000
df1$age2 <- df1$age^2
bootstrap_samples <- map(1:R, \(x) df1[ sample(x = 1:nrow(df1), size = nrow(df1), replace = T), ])

# models
fn_modeling <- function(data) {
    glm( data = data, marital ~ age + age2 + education, family = binomial(link = "probit"))} 

boot_models <- map(bootstrap_samples, \(x) fn_modeling(x))


# Boot estimate
boot_estimate <- map_dbl(boot_models, \(m){
        age  <- summary(m)$coef[2,1]
        age2 <- summary(m)$coef[3,1]

        age/((-1)*2*age2)
    }
)

quantile( boot_estimate, probs = c(0.025, 0.975) )

# Graphical representation
qplot(boot_estimate, geom = "density") + xlim( c(30,60))


