

# Seimar excercise RDD ECON 4136 2014

# Library and data --------------------------------------------------------


library(tidyverse)

# Data from Angris and Lavy (1999) 
grade_5_class_size <- haven::read_dta("RDD/5th_class_size_al_99.dta")

# Clean data
grade_5_class_size <-
  grade_5_class_size %>%  mutate(avgmath = ifelse(between(avgmath, 170, 190), 81.5, avgmath)) %>%
  filter(avgmath > 0)
 

# First look
grade_5_class_size %>% keep(~ is.numeric(.x) ) %>% map_df(function(x) {mean(x,is.na = T)}) %>% keep(~!is.na(.x))

grade_5_class_size %>% skimr::skim() 


# Graphical illustration. Classize function and residuals from rea --------
theme_set(theme_minimal())

grade_5_class_size_new %>%   
  ggplot(aes( y = f, x  = c_size) ) + 
  geom_line( color = "red", size = 1.0) +
  # The mean class size, grouped by enrollment. Red line in plot
  geom_line( data = grade_5_class_size %>% 
               group_by(c_size) %>% 
               summarise( mean_size = mean(classize) ),
             aes(y = mean_size, x  = c_size), color = "black", linetype = 1, size = 0.8, alpha = 0.5) +
  geom_line(data = grade_5_class_size_new %>% 
              group_by(c_size) %>% 
              mutate( avg_math = mean(avgmath)/1.4) , aes( x = c_size, y = avg_math), 
            size = 0.5, linetype = 1) +
  geom_vline( xintercept = c(40,80,120)) +
  scale_y_continuous( sec.axis = sec_axis(~. , name = "avgmath", breaks = c(40,80)))


# Reading test score and classize 
grade_5_class_size_new %>% 
  lm( avgverb ~ c_size + tipuach, data = .) %>% broom::augment() %>% 
  group_by(c_size) %>% 
  summarise( resid = mean(.resid) ) %>% 
  left_join( grade_5_class_size_new  %>%  select(c_size, f) , by = "c_size")  %>%
  group_by( c_size) %>% 
  summarise( resid = mean(resid),
             f = mean(f)) %>% 
  pivot_longer( names_to =  "var", values_to = "value", resid:f ) %>% 
  select(var, c_size, value) %>% 
  ggplot() +
  aes( x = c_size, y = value, fill = var) + geom_line() +
  geom_vline( xintercept = c(40,80,120 ), color = "red" )



#  Modeling - OLS ---------------------------------------------------------

# Result from modeling of mathavg. score.

# 1) OLS math and verbal test score ~ classize
grade_5_class_size %>%
  #filter(!is.na(avgmath),!is.na(avgverb)) %>%
  lm(avgmath ~ classize  , data = .) %>% summary()

# 1.2) With controls
grade_5_class_size %>%
  lm(avgmath ~ classize + tipuach + c_size , data = .) %>% summary()

# mean avgmath score:
# Method 1
grade_5_class_size %>% summarise( mean = mean(avgmath), se = sd(avgmath))

# Method 2
grade_5_class_size %>%
  lm(avgmath ~ classize + tipuach + c_size , data = .) %>% 
  broom::augment( newdata = grade_5_class_size %>% select(classize, tipuach, c_size) %>% map_df(function(x) {mean(x)}) )
#    %>% 
#   mutate( ci_low = .fitted - (1.96*.se.fit) , ci_high = (1.96*.se.fit )+ .fitted)

# Limit sample 20 - 60 student - RDD  -------------------------------------

# 2 Limiting the sample,:
  # School with enrollemnt between 20 and 60 students.
  # first discountinuity: 40 - dummy


grade_5_small_sample <-  
  grade_5_class_size %>% select( classize, contains("avg"), tipuach, c_size) %>% 
  # size enrollemnt
  filter(between(c_size, 20,60 )) %>% mutate( D = ifelse( c_size < 40, 0, 1))  %>% 
  # RD-models do often convert the running variables to x-c(cutoff - point)
  mutate( x_c = c_size - 40 ) 
  
# OLS 
## Check if this is correct
grade_5_small_sample %>% lm( avgmath ~ c_size, data = .) %>% summary()
grade_5_small_sample %>% lm( avgmath ~ x_c, data = .) %>% summary()

avgmath_ols_model <- grade_5_small_sample %>% lm( avgmath ~ x_c + tipuach + classize, data = .) 

summary(avgmath_ols_model)

# 2 RDD -sharp
grade_5_small_sample %>% 
  lm( avgmath ~  D  + x_c + I(x_c*D) + tipuach, data  = .) %>% summary()

# With controls
grade_5_small_sample %>% 
  lm( avgmath ~ x_c + D + I(D*x_c) + tipuach , data  = .) %>% summary()


# 4 Fuzzy
# find out how strict is the rule: Easiest way is by graph

grade_5_small_sample %>% count(classize > 40, c_size > 40 )

grade_5_small_sample %>% 
  mutate( c_d = ifelse(classize > 40, 1, 0)) %>% 
  ggplot(aes( x = c_size, y = c_d, color = as.factor(c_d) ) ) +
  geom_point( size = 1.5 ,alpha = 0.5) +
  geom_vline( xintercept = c(40)) +
  theme( legend.position = "down")

grade_5_small_sample %>% 
  mutate( c_d = ifelse(classize > 40, 1, 0)) %>% filter( c_d == 1)

# Transformed RDD-equation
grade_5_class_size %>% lm( avgmath ~ classize, data  = .) %>% summary()

# Gives the same as above
m1_srr <- grade_5_small_sample  %>% lm( avgmath ~ x_c + D + I(D*x_c) , data = .) 

summary(m1_srr)

m1_c_srr <- grade_5_small_sample %>%  lm( avgmath ~ x_c + D + I(D*x_c)  + tipuach , data = .) 

summary(m1_c_srr)

# First stage:
# Classize
rf_stage1_model <- grade_5_small_sample %>%
  lm(classize ~  tipuach + c_size + D, data = .) 

rf_stage1_model_MATH <- grade_5_small_sample %>%
  lm(avgmath ~  tipuach + c_size + D, data = .) 

summary(rf_stage1_model)
summary(rf_stage1_model_MATH)

# Second stage
model_small_sample_2sls <- rf_stage1_model %>% broom::augment() %>% 
  mutate( index = row_number(), .fitted) %>% 
  select( index, .fitted) %>% 
  right_join( grade_5_small_sample %>% mutate(index = row_number()), by = "index") %>% 
  lm( avgmath ~ tipuach + c_size + .fitted, data = .)
  

model_small_sample_2sls %>% summary

# Compare to OLS
summary(avgmath_ols_model )

# IV 2sls correct sd:
grade_5_small_sample %>% estimatr::iv_robust(avgmath ~tipuach + c_size + classize | D + tipuach + c_size , data = . ) %>% summary()


# The complet sample ------------------------------------------------------
grade_5_class_size_new %>% 
  lm( avgmath ~  classize + tipuach + c_size, data = .) %>% summary

# create new var
grade_5_class_size_new <- grade_5_class_size %>% 
  select( classize, contains("avg"), tipuach, c_size) %>% 
  # create the f_sc
  mutate( integer = (as.integer( (c_size -1 )/40)) +1 ) %>% 
  mutate( f = c_size/integer ) 


# First stage a class and math
model_first_stage <- grade_5_class_size_new %>% 
  lm( classize ~  f +   tipuach + c_size, data = .)  

model_first_math <- grade_5_class_size_new %>% 
  lm( avgmath  ~ f + tipuach + c_size, data = .) 

summary(model_first_stage)
summary(model_first_math)

# Second stage LS. First prepare data. Fitted values from stage 1
data_for_secound_stage <-
  model_first_stage %>% 
  broom::augment() %>% mutate( index = row_number()) %>%
  select(index, .fitted) %>% 
  right_join( grade_5_class_size_new %>% mutate(index = row_number()  ), by = "index" )

model_1 <- data_for_secound_stage  %>% 
  lm( avgmath ~ .fitted + c_size + tipuach , data = .) 
%>% 
  lm( classize  ~ c_size, data =. ) %>% 
  broom::augment() %>% 
  mutate( index = row_number()) %>% 
  select(index, d_prop = .fitted) %>% 
  right_join( grade_5_small_sample %>% mutate(index = row_number() ), by = "index" ) 

#
# grade_5_small_sample_fuzzy_df %>%  
#     lm( avgmath ~ D + x_c + I(x_c*d_prop) , data = .) %>% summary()

estimatr::iv_robust(avgmath ~ x_c  + tipuach + D | z + tipuach + x_c , data = grade_5_small_sample %>%   mutate( z = ifelse( x_c == 1, 1,0))) %>% summary()






rdd::RDestimate( avgmath ~ c_size + D)

estimatr::iv_robust(avgmath ~ x_c  + tipuach + D | z + tipuach + x_c , data = grade_5_class_size_new) %>% summary()









