
library(tidyverse)

# Boostrap seminar
# Marriage behavior US Black woman
# Model: P(married = 1|x) = normal(xB)

df <- haven::read_dta("data/cps09mar.dta")
tibble( var = names(df), label = map_chr(df,\(x) attr(x, "label") ) ) # Labels

# data cleaning and prep
df1 <- df  |>
    filter( 
        race == 2,
        female == 1, # US black woman
        region == 2 # Midwest
    ) |> 
    mutate( marital = ifelse( marital %in% c(1,2,3), 1,0 ))


# Model 
model <- glm( data = df1, marital ~ age + I(age^2) + education, family = binomial(link = "probit"))

summary(model)
beta_educ <- coef(model)[4]

# 2) How calculate the bootstrap standard errors

# 1) r-th random sample with replacement 
# 2) calculate Beta hat (r*)
# 3) Repeat 1) and 2)

R <- 100
bootstrap_samples <- map(1:R, \(x) df1[ sample(x = 1:nrow(df1), size = nrow(df1), replace = T), ])

fn_modeling <- function(data) {
    glm( data = data, marital ~ age + I(age^2) + education, family = binomial(link = "probit"))
} 

# Models 
bootstrap_models <- map(bootstrap_samples,\(x) fn_modeling(data = x))

# Get the coefs
bootstrap_coefs <- map_dbl(bootstrap_models, \(x) coef(x)[4])

# Non paramteric bootstrap
bootsd <- sd(bootstrap_coefs)
bootsd2 <- ((1/(R-1))*sum( (bootstrap_coefs-mean(bootstrap_coefs))^2 ) )^0.5

# CI
c( beta_educ-1.96*bootsd, beta_educ+1.96*bootsd )
summary(model) # Close to the ml-sd.

# 4) t-test using ML and Bootstrapped standard errors

# a) ML and the bootstrap standard errors
ml_st <- summary(model)$coef["education",2]
0.0898547/ml_st

# Bootstrap standard errors
0.0898547/bootsd2

# 4 b) 

# i) percentilie method
quantile( bootstrap_coefs, probs = c(0.025, 0.975) )

# ML ceck method
lmtest::coefci(model)[3,]


# percentile t-method using bootstrap standard errors # https://st47s.com/Math154/Notes/boot.html

# Look at the slide number 26

# 5) Bias corrected estimate 

# Bias = E[B]-B
# BIAS = hat(mean(B*)) - B
# B -Bias = 2B - mean(B*) 

# Age
c <- bootstrap_models[[1]] |> coef()
c[2]
bootstrap_coefs_age <- map_dbl(bootstrap_models, \(x) coef(x)[2])

Bage <- coef(model)[2]
meanBage <- mean(bootstrap_coefs_age)

# The bias corrected estimate
2*Bage - meanBage


# 6. Which age is the woman must likely to marry?

# Model 
model <- glm( data = df1, marital ~ age + age2 + education, family = binomial(link = "probit"))


# Want to know at which age the propability for getting marriage is highest.

# Link: linear prediction.
tbl <- tibble( predicted =  predict(model, type = "link"), dnorm = dnorm(predicted) )


# d) Estimator for this purpose 
# Derive the probit mtp age
# pdf()*b1age + 2*b2age2 = 0

# Get the pdf

# Average
df1_average <- df1 |>
    select(age, age2, education) |> 
    summarise_all(
        function(x) mean(x)
    )

pdf_avg <- predict( model, type = "link", newdata =  df1_average  ) |> dnorm()

summary(model)

b_age <- summary(model)$coef[2,1]
b_age2 <- summary(model)$coef[3,1]

# The age
(pdf_avg*b_age)/((-1)*2*b_age2)

# Bootstrap st.

# Data
R <- 1000
bootstrap_samples <- map(1:R, \(x) df1[ sample(x = 1:nrow(df1), size = nrow(df1), replace = T), ])

# models
fn_models <- function(x) {glm( data = df1, marital ~ age + age2 + education, family = binomial(link = "probit"))}
boot_models <- map(bootstrap_samples, \(x) fn_models(x))

# Average data
boot_avg_df <- map( bootstrap_samples, \(x) x |>
    select(age, age2, education) |> 
    summarise_all(
        function(x) mean(x)
        ) 
    )

# pdf_avg 
boot_pdf <- pmap_dbl( 
    list(
        m = boot_models,
        avg_data = boot_avg_df
    ),
    \(m, avg_data){
        predict( m, type = "link", newdata =  avg_data  ) |> dnorm()
    }  
)

# Boot estimate
boot_estimate <- pmap_dbl(
    list( 
    m = boot_models,
    pdf = boot_pdf
    ),
    \(m, pdf){
        age  <- summary(m)$coef[2,1]
        age2 <- summary(m)$coef[3,1]

        pdf*age/((-1)*2*age2)
    }
)

quantile( boot_estimate, probs = c(0.025, 0.975) )

# Graphical representation
qplot(boot_estimate, geom = "histogram")


