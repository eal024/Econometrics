
library(tidyverse)
options(scipen = 9)
# Exercise 1 - Prefered spesification -------------------------------------

sem1_ex1 <- haven::read_dta("Seminar/RawData/econ4137sem1ex1.dta")

sem1_ex1 %>% distinct(z)

#sem1_ex1 %>% GGally::ggpairs( axisLabels = "hide")

sem1_ex1 %>% plot()

sem1_ex1 %>% ggplot( aes(x = x , y = y , fill = factor(z), )) + geom_point() + facet_wrap(~z)

# What is the prefered spesification?

sem1_ex1 %>% lm( y ~ x  + z, data  =. ) %>% summary()

sem1_ex1 %>% lm( y ~ x + I(x^2)  + I(x*z), data  =. ) %>% summary()

sem1_ex1 %>% lm( I(log(y)) ~ I(log(x)) + z, data  =. ) %>% summary()

# Should z be included?

en <- sem1_ex1 %>% lm(y ~x, data = .) %>% broom::augment() %>% mutate( index = row_number()) %>% select(index,y ,.resid)
to <- sem1_ex1 %>% lm(x ~z, data = .) %>% broom::augment() %>% mutate( index = row_number()) %>% select(index ,.resid)

en %>% left_join(to, by = "index") %>% 
  ggplot( aes(.resid.x, .resid.y)) + geom_point() + geom_smooth(method = "lm", se = F) +
  labs( title = "Residuals from y~x, regressed on residuals from x ~z is the new info z brings to table",
        subtitle = "Plotting the first set of residuals against the second gives\nyou the relationship between X0 and Y after controlling for the other X’s")

sem1_ex1 %>% ggplot( aes(z, y)) + geom_point() + geom_smooth(method = "lm", se  = F) + labs(title = "y ~ z. Seems like no trend between theme")



# 2. Clothing -------------------------------------------------------------

# fs::dir_ls("Verbeek Guide to Modern Econ/data_verbeek")

library(broom);library(Ecdat);library(estimatr)

clothing <- Ecdat::Clothing %>% as_tibble()

clothing %>% skimr::skim()

clothing %>% plot()

# Model 1
model1_sale <-  clothing %>% lm_robust(sales ~ ssize + hoursw, data = .) 

summary(model1_sale)
glance(model1_lm)

# lm
model1_lm <- clothing %>% lm(sales ~ ssize + hoursw, data = .)

summary(model1_lm)
# RESET test. Q  = 2
model1_sale %>%
  helper::help_augment(data = clothing) %>%
  mutate(fitted_2 = fitted ^ 2) %>% 
  lm_robust( sales ~ ssize + hoursw  + fitted_2, data = .) %>% summary()

  
model1_lm %>% augment() %>% mutate( .fitted_2 = .fitted^2) %>% 
  lm(sales ~ssize + hoursw + .fitted_2, data = .)  %>% summary()

# No indication that f-wrong or important v. exluded.

# c) owner 
model2_lm <- clothing %>% lm(sales ~ ssize + hoursw + nown, data = .)
model3_lm <- clothing %>% lm(sales ~ ssize + hoursw + nown + npart, data = .)

summary(model2_lm)
summary(model3_lm)

# d)
model_b <- clothing %>% lm(sales ~ nown + nfull+ npart + ssize , data = .)

summary(model_b)
glance(model_b)

## R2 derived
augment(model1_lm) %>%
  mutate( mean_sale = mean(sales)) %>% 
  mutate( sst = (sales - mean(sales) )^2,
          se_2 = .resid^2) %>% summarise( sst = sum(sst), sse = sum(se_2), r = 1-sse/sst)

glance(model1_lm)

model1_lm %>% summary()

# Test non-nested models
# F-test
 
## Validity of model B

model_a <- clothing %>% lm(sales ~ ssize + hoursw, data = .)
model_b <- clothing %>% lm(sales ~ nown + nfull+ npart + ssize , data = .)


un_restricted <- clothing %>% lm(sales ~ ssize + nown + nfull+ npart + (hoursw) , data  =.)
restricted <- model_b

r_sq <- function(model) { broom::glance(model) %>% pull(r.squared)}

f_test <- function(model_un, model_r, J, N , K) {
   
  f <-  ( (r_sq(model_un)-r_sq(model_r) )/J ) /( (1-r_sq(model_un))/(N-K) )
  f
}

f_test( un_restricted, model_b, J = 1, N = 400, K = 6  )

r_sq(model_a)
r_sq(un_restricted)


un_restricted <- clothing %>% lm(sales ~ ssize + hoursw + (nown + nfull+ npart) , data  =.)

f_test( un_restricted, model_a, J = 3, N = 400, K = 6  )

r_sq(un_restricted)
r_sq(model_b)

## J-test
# Validiy of model a
unr <- model_a %>% augment() %>% mutate( index = row_number()) %>% select(index, .fitted) %>% 
  right_join(clothing %>% mutate(index = row_number()), by  = "index" ) %>% 
  lm(sales ~ nown + nfull+ npart + ssize + .fitted , data = .) %>% summary()

restriced <- model_b

# Model a cant be rejected
f_test(unr, restriced, J = 1, N = 400, K = 5)

unr <-
  model_b %>% augment() %>% mutate(index = row_number()) %>% select(index, .fitted) %>%
  right_join(clothing %>% mutate(index = row_number()), by  = "index") %>%
  lm(sales ~  ssize + hoursw  + .fitted , data = .) %>%
  summary()


f_test(unr, model_a, J = 1, N = 400, K = 3)
# Cant reject model B

## i) Model C
model_c <- clothing %>%  lm(sales ~ hoursw + ssize + nfull + npart , data = .)

# Positiv and significant
summary(model_c)

model_c$fitted.values

clothing %>%
  mutate(fitted = model_c$fitted.values) %>%
  lm(sales ~ I(fitted ^ 2) + hoursw + ssize + nfull + npart, data = .) %>%
  summary()

# y(Q=2) rejeceted. No indeces that omitted variables are missing or wrong functional form 

# Model D
model_d <- clothing %>% lm(I(log(sales)) ~ I(log(ssize)) + I(log(hoursw)) , data = .)

summary(model_d)

## Compare model A with model D

# Cant compare R2 vs. R2 ¨-- when different y
# But corr2(y, mhat), were mhat = exp(log(y) + MSE/2) and correct for the stand. error og logY. MSE(s2)

# 1) 
rmse <- model_d %>% 
  augment() %>% janitor::clean_names() %>% 
  mutate_if( is.numeric, as.numeric) %>% 
  dplyr::select(fitted, e = (resid) ) %>% summarise( rmse = sum(e^2)) %>% pull()
  
# Calculate R2 -> corr(sales, mhat)
augment(model_d) %>%
  mutate(mhat = exp(.fitted + (rmse / 2)), sales = clothing$sales) %>%
  summarise(
    corr_mhat_sales = cor(mhat, sales),
    sigma_mhat = sd(mhat),
    sigma_s = sd(sales),
    sigma_m_s = cov(mhat, sales),
    corr = (sigma_m_s / (sigma_mhat * sigma_s))
  )

# R2 adjusted for diffenrent functional form gives 65% . Model D are prefered.

# Normality ~residuals

tibble(x = model_a$residuals) %>% 
  ggplot( aes( x = x ))  + 
  geom_histogram( aes(y = ..density.. ), alpha = 0.5, color = "gray", bins = 50) +
  geom_density( color = "red", size = 1.5)
  


# 3 Lalonde ---------------------------------------------------------------

#fs::dir_ls("Seminar/RawData")

lalonda <-  haven::read_dta("Seminar/RawData/lalonde.dta")



lalonda_random <- lalonda %>% 
  filter( sample %in% c(4,5))


# a) # Does it look like the data is randomized? 
lalonda_random %>% 
  select( age, married, black, education, earnings75, treatment) %>% 
  pivot_longer( names_to = "var", values_to =  "value", age:earnings75) -> lalonda_random_long

lalonda_random_long %>% 
  group_by(var) %>% 
  nest() %>% 
  mutate( balance_model = map(data, function(x){lm(x$value ~ x$treatment, data = .) %>% tidy()})) %>% 
  unnest(balance_model) %>% 
  arrange(term, var) %>% 
  select( var, term, estimate, statistic)
  

# Alternativ
lalonda_random_long %>%
  mutate(type = ifelse(var %in% c("married", "black"), "factor", "numeric")) %>%
  ggplot(aes(
    x = var,
    y = value,
    fill = factor(treatment)
  )) + geom_boxplot() +
  coord_flip() +
  facet_wrap( ~ var, scales = "free_x")


# t-test 
lalonda %>% 
  filter( sample %in% c(4,5)) %>% 
  t.test( age ~ treatment, alternative = c("two.sided") , data = .)

t_test <- lalonda_random_long %>% 
  group_by(var) %>% 
  nest() %>% 
  mutate( t_test = map(data, function(x) {t.test( x$value ~ x$treatment , alternative = c("two.sided"), data = x) } ))

walk2(t_test$t_test, t_test$var ,function(x,y) {print(str_c("The variable in that is tested is: ", y));print(x) })

# Graphical check
lalonda_random %>% ggplot( aes(y = age, x = factor(treatment) )) + geom_boxplot()




# Data from the NSW - National Supported Work Demonstration
# T: Temporary employment
# Out_Y - earnings78
# Pre-experiment char: age married black edu earning75

lalonda %>% head(n = 10)

skimr::skim(lalonda)

map_dbl(lalonda, function(x) { length(unique(x))} )

lalonda %>%
  filter(sample %in% c(4, 5)) %>%
  group_by(treatment) %>%
  summarise(mean_age =  mean(age),
            n = n(),
            st_dev = sd(age),
            )

sigma <-  (  ( (425-1)*6.59^2 +(297-1)*6.69)/(425+297-2)   )^0.5


se_ <- 11.19*(((1/425)+(1/297))^0.5)

se_^0.5

se_weight <- (sigma)*(((1/425)+(1/297))^0.5)

#
(24.626-24.44)/se_

# 
(24.626-24.44)/se_weight


lalonda %>% 
  filter( sample %in% c(4,5)) %>% 
  t.test( age ~ sample, alternative = c("two.sided") , data = .)




df_filted <- lalonda %>% 
  filter( sample %in% c(4,5))

lsr::independentSamplesTTest( 
  formula = df_filted$age ~ df_filted$treatment ,  # formula specifying outcome and group variables
  data = df_filted,             # data frame that contains the variables
  var.equal = TRUE          # assume that the two groups have the same variance
)  


