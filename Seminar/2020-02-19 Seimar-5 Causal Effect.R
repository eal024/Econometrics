



# Packages and data -------------------------------------------------------

library(tidyverse);library(foreign);library(broom)

lalonda <- foreign::read.dta("Seminar/RawData/lalonde.dta")

lalonda <- as_tibble(lalonda)


# 2 lalonda ---------------------------------------------------------------

# a) The randomazed sub-group:

lalonda_sub_4_5 <- filter(lalonda, as.integer(sample) %in% c(4,5))

lalonda_sub_4_5 %>% distinct(sample)

# b) Estimate the ATT. 

# Since RCT, E(Y,1, D = 0) - E(Y,0, D = 0) = 0. And E(Y,1, D = 1) - E(Y,0, D = 1) = ATT = ATE = E(Y, D = 1)  - E(Y, D = 0)

table_treatment <- 
  lalonda_sub_4_5 %>% 
  group_by( treatment) %>% 
  summarise( n  = n(),
             wage_78 =   mean(earnings78)) 

att <-  table_treatment %>% summarise( t_effect = wage_78[treatment == 1] - wage_78[treatment == 0])


lalonda_sub_4_5 %>% lm(earnings78 ~ treatment, data = .) %>% summary


lalonda_sub_4_5 %>% skimr::skim()





# c) pre-experimet characteristics


lalonda_sub_3_5 <- 
  lalonda %>% filter( as.integer(sample) %in% c(3,5))


lalonda_sub_3_5 %>% 
  count(sample, treatment)


lalonda_sub_3_5 %>% 
  select( -outcome) %>% 
  pivot_longer(names_to = "var", values_to = "value", age:earnings78) %>% 
  mutate( deler = ifelse( var %in% c("earnings78", "earnings75") , "storre", "mindre")) %>% 
  ggplot( ) +
  aes( x = var, y =  value , fill = as.factor(treatment )) +
  geom_boxplot() +
  coord_flip() +
  facet_grid( ~deler, scales = "free_x") 


# Check for balance
lalonda_sub_3_5 %>% 
  group_by(treatment) %>% 
  summarise( n = n(), 
             earnings_75 = mean(earnings75, na.rm = T), earnings_78 = mean(earnings78, na.rm = T),
             age = mean(age),
             edu = mean(education),
             black = mean(black) )

# Regression 
options( scipen = 999) 

lalonda_sub_3_5 %>% lm( earnings78  ~ treatment, data = .) %>% summary()

lalonda_sub_3_5 %>% select(-sample, -outcome) %>% lm( earnings78  ~ . , data = .) %>% summary()


# e)

# Recipe:
# 1. calculate the t== 0 submodel (save the coeff.)
model_beta_null <- lalonda_sub_3_5 %>% filter( treatment == 0) %>%  select(-sample, -outcome, -treatment) %>% 
  lm( earnings78 ~ ., data = .) 



# (look at the model)
model_beta_null %>% summary()

# Get Y0, given X = (T==1), by 1) get mean(x), than add this to prediced values:
# 1
x_mean <- lalonda_sub_3_5 %>% select(-sample, -outcome) %>%   filter( treatment == 1) %>% 
  map_df( function(x) {mean(x)}) 
# 2
model_beta_null  %>%  broom::augment( newdata = x_mean )

# the y = 
y0 <- model_beta_null  %>%  broom::augment( newdata = x_mean ) %>% pull(.fitted)
y0

# 3) get y, given T == 1:
model_beta_one <- lalonda_sub_3_5 %>%  filter( treatment == 1) %>%  select(-sample, -outcome, -treatment) %>% 
  lm( earnings78 ~., data = . )


model_beta_one %>% summary()

# the y1 , given T==1;
model_beta_one %>% augment( newdata = x_mean  ) 

y1 <- model_beta_one %>% augment( newdata = x_mean  ) %>% pull(.fitted)

y_att <- (y1-y0)
y_att

## Method 2:

model_beta_null %>%
  augment(newdata = lalonda_sub_3_5 %>% filter(treatment == 1) %>% select(-sample, -outcome,-treatment)) %>%
  mutate(y_xBc = earnings78 - .fitted) %>%
  #add_count( n()) %>%
  summarise(
    u_hat = sum(y_xBc) * (1 / 297),
    mean = mean(y_xBc),
    sd = sd(y_xBc),
    n = n()
  )



# SE - ATT ----------------------------------------------------------------

xbar <-  lalonda_sub_3_5 %>%
  filter(treatment == 1) %>%
  select(-c(sample, treatment, outcome, earnings78)) %>%
  map_df(function(x) {
    mean(x)
  }) %>%
  mutate(const = 1)

xbar <- xbar %>% select(const, age, education, black,hispanic,married, nodegree, earnings75)

xbar <- xbar %>% Matrix::as.matrix()

# get var(b)
model <- lalonda_sub_3_5 %>%  filter(treatment == 0) %>% select(-sample, -outcome, -treatment)   %>% lm( earnings78 ~., data = .)


v <- vcov(model) 

v_m <-  v %>% tidy() %>%
  rename( const = X.Intercept.) %>%
  mutate( .rownames = ifelse(str_detect(.rownames, "Intercept"), "const", .rownames)) %>% 
  column_to_rownames( ".rownames") %>% 
  Matrix::as.matrix()
  
v_m
  
    
sqrt(xbar%*%v_m%*%t(xbar)) 







# Bootstrap the SD-of mean ------------------------------------------------

model <- function(df) {lm( df$earnings78 ~ ., data = df) }

df <-
  crossing( index = seq(1:10)) %>% 
  mutate( lalonda = list(lalonda_sub_3_5 %>% select(-outcome, -sample))) %>% 
  mutate( random = map(lalonda, function(x) {sample_n(x ,size = nrow(lalonda_sub_3_5))} )) %>% 
  select(-lalonda) %>% 
  unnest( random)


a <- df %>% 
  group_by(treatment) %>% 
  nest() %>% 
  mutate( model = map(data, function(df) {model(df)}) ) %>% 
  group_split( treatment) %>% 
  bind_cols() %>% 
  mutate( avg_y = map_df(data, function(x) {mean(x, na.rm =T) }))


# df %>% 
#   group_by(treatment) %>% 
#   nest() %>%  
#   mutate( avg_y = map_if(data, function(x) {mean(x)  })  )
  
a$data[[1]] %>% map_df(mean)


mtcars %>% 
  as_tibble() %>% 
  group_by( vs) %>% 
  nest() %>% 
  mutate( test = map(data, function(x) {mean(x)}))



lalonda_sub_3_5 %>% 
  cro


create_samples <- 
  lalonda_sub_3_5 %>% 
  select( -c(outcome,sample)  )%>% 
  group_by( treatment) %>% 
  nest() %>% 
  crossing( index = seq(1:10)) %>% 
  mutate( random_data = map(data, function(x) {sample_n(x, size = nrow(x)  )})) %>% 
  mutate( model_null = map( data, function(df) {model(df)} ) ) %>%  
  group_split( treatment) %>% 
  bind_cols()


a <- create_samples %>% 
  mutate( df = map( random_data1, function(x) {x}) ) %>% 
  mutate( df_mean = map_df(df, function(x) {mean(x, na.rm = T)}) )


a$df_mean




# Appendix - calculating the se and mean ----------------------------------

sd_y <-  lalonda_sub_4_5 %>% 
  select(y = earnings78) %>% 
  mutate( y_hat_y_p2 = (y-mean(y))^2 ) %>% 
  summarise( n = n(), 
             sd_s_y = sqrt( (1/(n-1) )*sum(y_hat_y_p2) ) ) %>% pull(sd_s_y)
sd_y

lalonda_sub_4_5 %>%
  select(y = earnings78, t = treatment) %>%
  group_by( t) %>% 
  summarise( n = n()) %>% 
  summarise( n_n = (1/n[t==1]) +(1/n[t==0]) ) %>% pull(n_n) -> n_n


se_y_y <- sd_y*sqrt( n_n)


(5976-5090)/se_y_y

lalonda_sub_4_5 %>% lm( formula = earnings78 ~ treatment, data = .) %>% summary()













