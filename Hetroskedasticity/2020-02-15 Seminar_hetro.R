
options( scipen = 999)

# Packages and data -------------------------------------------------------

library(tidyverse)
library(broom)

theme_set(theme_light())

fs::dir_ls("Seminar/RawData")

 %>% %>% %>% %>% %>% %>% 

lalonde <- lalonde %>% as_tibble()



# Seminar 4 ---------------------------------------------------------------

# Look at the data
lalonde %>% 
  skimr::skim()


lalonde %>% distinct(sample)

# Subsample of interest: CPS - non-experimental CPS controls
lalonde_controls <- lalonde %>% filter( sample == "cps_controls")


lalonde_controls %>% skimr::skim()

dep_var <- "earnings78"

vars <- c("age", "married", "black", "education" , "earnings75", "hispanic", "nodegree")


# Model 1 
model1 <- lalonde_controls %>% lm(str_c(dep_var,
                                        str_c(vars, collapse = " + "),
                                        sep = "~"), data = .)


model2 <- 
  lalonde_controls %>%
  filter( earnings78  > 0) %>% 
  mutate( log_earnings78 = ifelse(earnings78 != 0, log(earnings78), 0)) %>% 
  lm( str_c("log_earnings78","~", str_c(vars, collapse = "+")),
      data = .)



# Explaining the coeffisient

model1 %>% summary

model2 %>% summary

explained <-  model1 %>% anova() %>% tidy %>% mutate( expl =  sumsq/sum(sumsq))


model1 %>% summary %>% tidy( ) %>% 
  filter(! str_detect(term, "Intercept")) %>% 
  mutate( kategori = ifelse( term %in% c("age", "earnings75"), "double", "factor")) %>% 
  mutate( xmin = estimate - 1.96*std.error, xmax = estimate + 1.96*std.error) %>%
  left_join(explained, by = "term") %>% 
  ggplot( ) +
  aes( y = estimate , x = fct_reorder(term, estimate)) + geom_point( aes(size  = expl  )) +
  geom_errorbar(  aes(ymin = xmin, ymax = xmax)  ) +
  coord_flip() +
  geom_hline( yintercept =   0, size = 2, linetype = 2, color = "gray") +
  facet_wrap(~kategori,  scales = "free_x") + 
  theme( legend.position = "none") +
  labs( y = "Term", x = "Koeffisientes størrelse", title = glue::glue("Graf som viser estimatene i linear-linear modelen earnings78"),
        subtitle = glue::glue("Strørrelsen på kulepunktene indikerer hvor\n stor andel estimatet har av modellens forklaringsgrad."))

 


# C) Breusch-Pagan test: --------------------------------------------------


# The UiO-solution drops all variables were earnings78 are equal 0. This cant be the right way to do it. 

lalonde_controls %>%  filter( earnings78 != 0) %>%  dim()


axilliar_regression_model2 <- 
  augment(model2) %>%
  # earnings78 is filted in model2
  mutate(u2 = .resid^2) %>% 
  lm( u2 ~  age + married + black + education + earnings75 + hispanic +nodegree, data = . ) 


axilliar_regression_model2 %>% summary

BP <- function(model) {
  
  dim <- augment(model) %>% dim()  
  n <- dim[1]
  
  b_n <- length(model$coefficients) 
  
  r_squared <- summary(model)$r.squared
    
  f_test <- (r_squared/(1-r_squared) )*(n/b_n)

  bp_lm <- n*r_squared
  
  list_samlet <- list(f_test = f_test, bp_lm = bp_lm)
  return(list_samlet)
}


# Return F-test of BP-test
BP(axilliar_regression_model2)

0.0687*14244

lmtest::bptest(model2)


# model1 log-model

axilliar_regression_model1 <-
  augment(model1) %>% 
  mutate(log_u2 = log(.resid^2)) %>%
  mutate(index = row_number()) %>% 
  select(index, everything()) %>% 
  lm( str_c("log_u2", "~ ", str_c(vars, collapse = "+")), data = .)


# c 2: Model 2 - Use log2 - and check for the PB-test

log_auxiliary_reg_model2 <-
  augment(model2) %>% 
  mutate(log_u2 = log(.resid^2)) %>% 
  lm( str_c("log_u2", "~" , 
            str_c(vars, collapse = "+") 
            ),
      data = .)


log_auxiliary_reg_model2 %>% summary


BP(log_auxiliary_reg_model2)


# d) FGLS - As Verbeek  ------------------------------------------------------


# d) FGLS
# calculate the h(xi)
fgls_h_model1 <-
  augment(model1) %>%
  mutate(log_u2 = log(.resid ^ 2)) %>%
  lm(str_c("log_u2" ,"~" ,
           str_c(vars, collapse =  "+")),
     data = .) %>%
  augment() %>%
  mutate(index = row_number(), w_i = 1/exp( .fitted^0.5) )

w <- fgls_h_model1 %>% pull(w_i)


lalonde_controls %>% 
  select(dep_var, vars) %>% 
  #filter( earnings78 > 0 ) %>% 
  mutate_all( function(x) {x*w}) %>%
  mutate( w = w) %>% 
  lm( str_c("earnings78", "~", "0 + w +" , str_c(vars, collapse = "+") ) , data = .) %>% 
  summary()

lalonde_controls %>% 
  select(dep_var, vars) %>% 
  lm( str_c("earnings78", "~", str_c(vars, collapse = "+") ) , weights = 1/w , data = .) %>% 
  summary()




augment(model1) %>%
  mutate(index = row_number()) %>%
  left_join(fgls_h_model1 %>% select(index, w_i), by = "index") %>%
  select(w_i, earnings78:nodegree) %>%
  transmute(
    earnings78_h = earnings78 * w_i,
    age_h = age * w_i,
    married_h = w_i * married,
    black_h = w_i * black,
    education_h = earnings75 * w_i,
    hispanic_h = hispanic * w_i,
    nodegree_h = nodegree * w_i
  ) %>% 
  lm( earnings78_h ~., data = .) %>% 
  summary
  


# e) Hetro-robust st.errors

lalonde_controls %>% 
  estimatr::lm_robust(earnings78 ~ age + black + hispanic + married + nodegree + education + earnings75,
                      se_type = "HC1",
                      data = .) %>% 
  summary







# bootstrap_tibble <-  map(seq(from = 1, to = 300 ,by = 5) , function(x) {the_bootstrap(size = x, model = model1_earnings78)} ) %>% 
#   bind_rows()


# bootstrap_tibble %>% 
#   filter(mc_size != 0) %>% 
#   ggplot( ) + 
#   aes( y = z, x = mc_size) + 
#   geom_line() +
#   geom_smooth( se = F) +
#   facet_wrap( ~term, scales = "free_y")
# 


