
library(fixest)

# Example IV-regression, homogenous treatment effect.
# ECON 4137 Seminar 3

# prepart) Data: Sample of mothers and labor supply
angev <- haven::read_dta("data/angev98.dta")

unique( angev$kidcount)

# Question of interest: What is the causal effect of fertility on labor supply?

# SF: workedm = a + bmorekids + v

# 1) Estimate model with OLS
model1 <- feols( data = angev, workedm ~ morekids)
model2 <- feols( data = angev, workedm ~ morekids, vcov  = "HC1")

# Which variable to control for?
vars <- c( "agem", "agefstm", "ageqk" ,"ageq2nd",  "boy2nd", "marital", "blackm", "hispd")
X <- paste( vars, collapse =   " + " )
model_as_string <- as.formula( "workedm ~morekids + " |> paste0(X)  )
model3 <- feols( data = angev, model_as_string  , vcov  = "HC1")

# Print the result
fixest::esttable(list(model1, model2, model3))

#  Check for correlation between morekids and X
cor( angev |> subset( select = vars ) )
cov( angev |> subset( select = c( "workedm","morekids", vars[1:4]) ))

# Calculate the biased
df_cov <- cov( angev |> subset( select = c( "morekids", vars[1:4]) ) )   # Covariat matrix 
df_cov <-  as.data.frame(df_cov) |>  transform( var = colnames(df_cov) ) # With colnames

# Calcualte the OVB direction from more kids.
df_cov |> 
    subset( select = c( "var", "morekids") ) |> 
    transform( ovb = morekids/(morekids[var == "morekids"])^2 )

# 2) Mixed gender suggests parents with two girls/boys are more likely to have a third
angev[c("morekids", "kidcount")]

# OLS check data
model4 <- feols( data = angev, morekids ~ samesex)

# With control
model_string2 <- as.formula( paste0( "morekids ~ samesex + " , paste( vars[c(1,6:8)], collapse = "+")  ))
model5 <- feols( data = angev, model_string2 )
model6 <- feols( data = angev, kidcount ~samesex )

fixest::esttable( list(model4, model5, model6)) # Print the result

# Parents with kids (1 and 2) with same sex, tent to have 0.07  more kids.
# or 5.9 percent increased chance of haveing more than 2 kids

# 3) Use samesex as instrument

# Necessary assumption

# Exclution restriction: Uncorrelated with the error term. 
    # work on morekids, only through that parents want to have a third (when 1 and 2 has the same gender)

# Relevance
model5 # morekids, highly correlated with samesex

# Covariats in SF need to be in the first stage
fixest::esttable(list(model5))


# 4) B_IV balancing: The instrument should work as a randomizer

lm( data = angev, morekids ~hispm)

# Logic for perform a balance test
df        <- data.frame(xvar = c("hispm", "blackd", "educm", "hsgrad"), stringsAsFactors = FALSE)
reg_list  <- lapply( df$xvar, function(var) lm(as.formula(paste0(var, "~ samesex")), data = angev))
names(reg_list) <- df$xvar 

# Prepering the result
reg_list1 <- lapply( reg_list, \(x) broom::tidy(x)  |> filter( term == "samesex"))
dplyr::bind_rows(reg_list1, .id = "var")

# 5) Wald estimate. Effect of more kids on workem

# i) by hand
y1_y0 <- mean(angev$workedm[angev$samesex == 1] ) - mean(angev$workedm[angev$samesex == 0] )
x1_x0 <- mean(angev$morekids[angev$samesex == 1]) - mean(angev$morekids[angev$samesex == 0])

b_wald <- y1_y0/x1_x0

# ii) By inbuilt function IV-regression: doing canned 2SLS
#  Fixest model
modelw <- feols( data = angev, workedm ~ 1|morekids~samesex )
fixest::esttable( list( model1, modelw)) # Printing the result

# iii) by 2-stage least squre

# First stage
model1_fs <- feols( data = angev, morekids~samesex)
string_stage1_x <- as.formula( paste0( "morekids ~", paste0( c(xvar[-1], "samesex"), collapse = " + ") ) )
model1_fs_c <- feols( data = angev, string_stage1_x )

# Second stage
angev$x_mk_iv_samesex <- predict(model1_fs)
model1_ss <-   feols(  data = angev, workedm ~ x_mk_iv_samesex)
model1_ss_c <- feols(  data = angev, workedm ~ hispm + blackm + agem + x_mk_iv_samesex)

# Print the result
esttable( list( model1_ss, model1_ss_c))


# 6 Hausman test of endogenity: Test if the X1 is a endogeonus variable 
# logic: 1) Keep the residual from the first stage. This part should contain the endogenous part.
# 2) Add residual to SF model. Calcualte the t-value.  

# First stage
residuals <- angev$morekids - predict(model_fs)

df <- data.frame( y = angev$workedm,  morekids = angev$morekids, e = residuals)

model_hausman <- feols( data = df, y ~  morekids + e, vcov = "HC1")

# Under the assumption the instrument is valid
esttable( list(model1, model_hausman))
# Since the coefficient on the residual is not significant, the test indicates that the regressor morekids is exogenous

# 7 Adding a control variable W
# First stage
model1_iv_fs1 <- feols( data = angev, morekids ~ samesex + hispm + blackm + agem)
residuals <- angev$morekids - predict(model1_iv_fs1)

xvar <- c( "morekids", "hispm", "blackm", "agem")

df <- angev |>
    subset( select = c("workedm", xvar)) |> 
    transform( e = residuals)

model_hausman2 <- feols( data = df, as.formula( paste0( "workedm ~", paste0( c(xvar, "e"), collapse = " + ") ) ) , vcov = "HC1")

# Still not significant
esttable(model_hausman2)

# canned way:
feols( data = angev, workedm ~ hispm + blackm + 1|morekids~samesex ) |> esttable()

# Alternativ iv-regresion 
# library(estimatr)
# iv_robust( data = angev, workedm ~ hispm + blackm + morekids|samesex +hispm + blackm )

# 8) Alternative instrument: twins2
modela2_iv_fs1 <- feols( data = angev, morekids ~ twins2) 
angev$x_twins <- predict(modela2_iv_fs2)

modela2_iv_fs2_c <- feols( data = angev, morekids ~ twins2 + hispm + blackm + agem ) # With controls
angev$x_twins_c <- predict( modela2_iv_fs2_c)

esttable( list( modela2_iv_fs1, modela2_iv_fs2_c)) # First stage print
# Interpret: Having twins increase the prop. of having morekids (kids > 2) with 0.6 = 60 percent. 

# Stage 2 IV-regression.
modela2_iv_ss   <- feols( data = angev, workedm ~ x_twins )
modela2_iv_ss_c <- feols( data = angev, workedm ~ x_twins_c + hispm + blackm + agem )

# Printing second stage
esttable( list( model1, model1_ss,  modela2_iv_ss)) 
# Interpret: 
# The OLS model says having more kids are assosisated with -0.12 less workedm (percentage less participation in the workforce)
# The IV1 says the causal effect of having more than two kids are -0.13
# The IV2 says the casual effect is less. Only -0.08  

# Sargan test.
# When M (instruments) > L (endogenous X) we can test whether a subset of the exclution restriction is valid (under some assumption) 
# Can test cov(z1, u) != 0, or cov(z2, u) != 0, but not both

# How:
df <- angev |> subset( select = c("workedm" ,"morekids", "twins2", "samesex")) # Data

# First stage 
fs_sargan <- feols( data = df, workedm ~morekids + twins2 + samesex )
df$v <- residuals(fs_sargan)

# Test
sargan <- lm( data = df, v ~ morekids + twins2 + samesex)

sum_v_sargan <- summary(sargan)
chi_test <- sum_v_sargan$r.squared*nrow(df) 

# 10. Using both instruments
model3_fs <- feols( data = angev, morekids ~ samesex +  twins2 + hispm + blackm + agem ) # First stage
angev$x_iv3 <- predict(model3_fs)                                                        # Predict X
model3_ss <- feols( data = angev, workedm ~ x_iv3 + hispm + blackm + agem )              # Second stage

# Print the result
esttable( list( model1_ss_c, modela2_iv_ss_c, model3_ss ), keep = "^x", digits = 2)
# The result is closer to twins -> because the result first stage twins are much stronger! look at t-value.
esttable( list( model1_fs_c, modela2_iv_fs2_c )  ,digits = 3)
summary(model1_fs_c)
summary(modela2_iv_fs2_c)



