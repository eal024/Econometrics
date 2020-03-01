


# library and data --------------------------------------------------------

library(tidyverse);library(broom)

labour2 <- read.table("Verbeek Guide to Modern Econ/data_verbeek/labour2.prn", header = T)


labour2 <- as_tibble(labour2)

labour <-  "LABOUR"

labor_dep_var <- c("CAPITAL", "OUTPUT", "WAGE")


# Model 1
model1 <- labour2 %>%  lm( str_c(labour, "~", str_c(labor_dep_var, collapse = "+")), data = .)

summary(model1)

# Breusch Pagan test:
auxillariary_model <-  
  augment(model1) %>% 
  mutate( e2 = (.resid)^2) %>% 
  lm( str_c("e2", "~", str_c(labor_dep_var, collapse = "+")), data = .) 


auxillariary_model %>% summary()


glance(auxillariary_model)

dim(augment(auxillariary_model))


# Model2 

log_labor_dep <- map_chr(labor_dep_var, function(x) {str_c("log(", x, ")")})

model2 <- labour2 %>% lm(str_c("log(LABOUR)", "~", str_c(log_labor_dep, collapse = "+")), data = .)


# Summary
summary(model2)

auxill_model2 <- 
  augment(model2) %>%  
  mutate( e2 = (.resid)^2) %>% 
  lm( e2 ~ log.CAPITAL. + log.OUTPUT. + log.WAGE., data = .)

BP(auxill_model2)



# White data
white_data <- augment(model2) %>% 
  janitor::clean_names() %>%
  mutate( e2 = resid^2, index = row_number()) %>% 
  mutate_at( vars(log_output, log_capital,log_wage), function(x) {x^2}) %>% 
  select( index, e2, log_labour:log_wage) %>% 
  left_join(augment(model2) %>% 
              janitor::clean_names() %>%  mutate( index = row_number()) %>%  select(contains("log"), index), by = "index") %>% 
  rename_at( vars(contains(".x")), function(x) {str_replace(x, pattern = ".x", replacement =   "^2")}) %>% janitor::clean_names()



white_auxillary <-
  white_data %>% 
  select(str_subset(names(white_data), pattern = "labour|index", negate = T)) %>% 
  mutate( log_wage_y*log_capital_y, log_wage_y*log_output_y, log_output_y*log_capital_y) %>% 
  janitor::clean_names() %>% 
  lm(e2 ~ ., data = . ) 


summary(white_auxillary)



# Robust st.error ---------------------------------------------------------

se_robust_model2 <-   
  labour2 %>%
  estimatr::lm_robust( log(LABOUR) ~log(CAPITAL) + log(OUTPUT) + log(WAGE), se_type =  "HC1", data = .) %>% summary() 

se_robust_model2 %>% attributes()  




# FGLS --------------------------------------------------------------------

# 1 Compute the e^2
w <- augment(model2) %>% 
  mutate( e2 = .resid^2) %>% 
  janitor::clean_names() %>% 
  lm( log(e2) ~ log_capital + log_output + log_wage, data =  .) %>% 
  augment(  ) %>%
  mutate( exp_e2 = 1/exp(.fitted)^0.5 ) %>% 
  pull()



# 2 transform the model of interest:
weighted_model2 <-
  labour2 %>% 
  mutate_all( function(x) {log(x)} ) %>% 
  mutate_all( function(x) {x*w}) %>% 
  rename_all( function(x) { str_c("log_", x)}) %>% 
  mutate( w = w) %>% 
  janitor::clean_names() %>% 
  lm( log_labour ~ 0 + w + log_capital  + log_output + log_wage , data = .) 

# compare to mode2 -- look at the st.err

weighted_model2 %>% summary()

model2 %>% summary()


log_labour <- labour2 %>% mutate( log_labour = log(LABOUR)) %>% pull(log_labour)









