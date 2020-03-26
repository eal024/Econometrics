

# Seimar excercise RDD ECON 4136 2014

# Library and data --------------------------------------------------------


library(tidyverse)

#dir_ls_rdd <- fs::dir_ls("/RDD")

#?DOS::angristlavy

# grade_4_class_size <- haven::read_dta("RDD/4th_class_size_al_99.dta")

# Data from Angris and Lavy (1999) 
grade_5_class_size <- haven::read_dta("RDD/5th_class_size_al_99.dta")

# Clean data
grade_5_class_size <-
  grade_5_class_size %>%  mutate(avgmath = ifelse(between(avgmath, 170, 190), 81.5, avgmath)) %>%
  filter(avgmath > 0)
 

# First look
grade_5_class_size %>% keep(~ is.numeric(.x) ) %>% map_df(function(x) {mean(x,is.na = T)}) %>% keep(~!is.na(.x))

grade_5_class_size %>% skimr::skim() 

#grade_5_class_size %>%  filter( !is.na(avgverb) | !is.na(avgmath))  %>%   select(classize,tipuach, avgverb,avgmath)  %>% skimr::skim()

# 1) OLS math and verbal test score ~ classize
grade_5_class_size %>% 
  filter( !is.na(avgmath ), !is.na(avgverb)) %>% 
  lm( avgmath ~ classize  , data = .) %>% summary()

grade_5_class_size %>% 
  filter( !is.na(avgmath ), !is.na(avgverb)) %>% 
  lm( avgverb ~ classize , data = .) %>% summary()

# With controls
grade_5_class_size %>% 
  filter( !is.na(avgmath ), !is.na(avgverb)) %>% 
  lm( avgmath ~ classize + tipuach + c_size , data = .) %>% summary()




# 2 Limiting the sample,:
  # School with enrollemnt between 20 and 60 students.
  # first discountinuity: 40 - dummy

grade_5_small_sample <-  
  grade_5_class_size %>% 
  select( classize, contains("avg"), tipuach, c_size) %>% 
  # size enrollemnt
  filter(between(c_size, 20,60 )) %>% 
  mutate( D = ifelse( c_size < 40, 0, 1))  %>% 
  # create RD-equation:
  mutate( x_c = c_size - 40 ) 
  
# OLS
grade_5_small_sample %>% lm( avgmath ~ classize, data = .) %>% summary()
grade_5_small_sample %>% lm( avgmath ~ x_c, data = .) %>% summary()
grade_5_small_sample %>% lm( avgmath ~ x_c + tipuach, data = .) %>% summary()

# 2 RDD -sharp
grade_5_small_sample %>% 
  lm( avgmath ~  D  + x_c + I(x_c*D) + tipuach, data  = .) %>% summary()

# With controls
grade_5_small_sample %>% 
  lm( avgmath ~ x_c + D + I(D*x_c) + tipuach , data  = .) %>% summary()


# 4 Fuzzy
# find out how strict is the rule: Easiest way is by graph
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


# Determine if the design is fuzzy or sharp -------------------------------

# find out how strict is the rule: Easiest way is by graph
grade_5_small_sample %>% 
  mutate( D = ifelse(classize > 40, 1, 0)) %>% 
  filter( between(c_size, 20,130)) %>% 
  ggplot( ) +
  aes( x = c_size, y = D, color = as.factor(D) ) +
  geom_point( size = 1.5 ,alpha = 0.5) +
  geom_vline( xintercept = c(40,80,120)) +
  theme( legend.position = "down")


# 4. fuzzy RDD:
library(estimatr)

min(grade_5_small_sample$classize)

grade_5_small_sample %>% 
  mutate( z = ifelse( x_c == 1, 1,0)) %>% 
  iv_robust( avgmath  ~  D + x_c + tipuach  | x_c + z + tipuach, data = .) %>% summary()



grade_5_small_sample_fuzzy_df <-
grade_5_small_sample %>% 
  lm( classize  ~ c_size, data =. ) %>% 
  broom::augment() %>% 
  mutate( index = row_number()) %>% 
  select(index, d_prop = .fitted) %>% 
  right_join( grade_5_small_sample %>% mutate(index = row_number() ), by = "index" ) 

#
# grade_5_small_sample_fuzzy_df %>%  
#     lm( avgmath ~ D + x_c + I(x_c*d_prop) , data = .) %>% summary()
  
estimatr::iv_robust(avgmath ~ x_c  + tipuach + D | z + tipuach + x_c , data = grade_5_small_sample %>%   mutate( z = ifelse( x_c == 1, 1,0))) %>% summary()

  





# The complet sample ------------------------------------------------------
grade_5_class_size_new %>% 
  lm( avgmath ~  classize + tipuach + c_size, data = .) %>% summary

# create new var
grade_5_class_size_new <- grade_5_class_size %>% 
  select( classize, contains("avg"), tipuach, c_size) %>% 
  mutate( integer = (as.integer( (c_size -1 )/40)) +1 ) %>% 
  mutate( f = c_size/integer ) 


# First stage a class and math
model_first_stage <- grade_5_class_size_new %>% 
  lm( classize ~  f +   tipuach + c_size, data = .)  

model_first_math <- grade_5_class_size_new %>% 
  lm( avgmath  ~f + tipuach + c_size, data = .) 

summary(model_first_stage)
summary(model_first_math)

# Secound stage
data_for_secound_stage <-
  model_first_stage %>% 
  broom::augment() %>% mutate( index = row_number()) %>%
  select(index, .fitted) %>% 
  right_join( grade_5_class_size_new %>% mutate(index = row_number()  ), by = "index" )

model_1 <- data_for_secound_stage  %>% 
  lm( avgmath ~ .fitted + c_size + tipuach , data = .) 

summary(model_1)

model_2 <-   data_for_secound_stage %>% lm( avgmath ~ .fitted + c_size  + tipuach , data = .)


options(scipen = 999)
summary(model_2)
summary(model_2_1)  



# With IV -----------------------------------------------------------------
options(scipen = 9)
grade_5_class_size_new %>%  iv_robust( avgmath ~ classize + c_size + tipuach | f + c_size + tipuach, data = .) %>% 
  summary()




# Nice RDD plot::: --------------------------------------------------------
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


rdd::RDestimate( avgmath ~ c_size + D)

estimatr::iv_robust(avgmath ~ x_c  + tipuach + D | z + tipuach + x_c , data = grade_5_class_size_new) %>% summary()
  










grade_5_class_size %>% 
  lm(classize ~ c_size, data = .) %>% 
  broom::augment() %>% 
  mutate( index = row_number()) %>% 
  select( index,  .fitted) %>% 
  right_join( grade_5_class_size %>% mutate(index = row_number() ), by = "index") %>% 
  select(index, classize, pred_classize = .fitted ,c_size, tipuach, avgmath) %>% 
  lm( avgmath ~ pred_classize + tipuach , data = .) %>% summary( )


grade_5_class_size %>% 
  filter(between(classize, 20, 60)) %>% 
  #group_by(c_size) %>%
  mutate( over_40 =  ifelse( classize > 40, 1, 0)) %>% 
  group_by( over_40) %>%  




grade_5_class_size_new <- grade_5_class_size %>% 
  select( classize, contains("avg"), tipuach, c_size) %>% 
  mutate( integer = (as.integer( (c_size -1 )/40)) +1 ) %>% 
  mutate( pred_class_size = c_size/integer ) 

grade_5_class_size_new %>%   
ggplot( ) + 
  aes( y = pred_class_size, x  = c_size) +
  geom_line( color = "red", size = 1.0) +
  # The mean class size, grouped by enrollment. Red line in plot
  geom_line( data = grade_5_class_size %>% 
               group_by(c_size) %>% 
               summarise( mean_size = mean(classize) ),
             aes(y = mean_size, x  = c_size), color = "black", linetype = 1, size = 0.8, alpha = 0.5)


# Regression
grade_5_class_size_new %>%  
  lm( avgmath ~ 







