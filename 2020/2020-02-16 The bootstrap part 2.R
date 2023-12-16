# Bootstrap ---------------------------------------------------------------



bootstrap_df <- df %>% 
  mutate( data = map( index, function(x) {  sample_n(labour2, size = length(labour2$CAPITAL), replace = T ) } ) ) %>% 
  mutate( model = map( data  , function(x) { data %>% lm( log(x$LABOUR) ~ log(x$WAGE), data  = .) %>% summary() %>% tidy() } )) %>% 
  unnest( model) %>% 
  filter( ! str_detect(term, "(Intercept)")) %>% 
  select( term , estimate) 


bootstrap_df <- function(data, data_size, boot_rep, lm_formula ) {
  
  bootstrap_df <- tibble( index = seq(1:boot_rep)) %>% 
    mutate( data = map( index, function(x) {  sample_n(data, size = data_size, replace = T ) } ) ) %>% 
    mutate( model = map( data  , function(x) { data %>% lm( formula = log(x$LABOUR) ~ log(x$WAGE), data  = .) %>% summary() %>% tidy() } )) %>% 
    unnest( model) %>% 
    select( index, term , estimate) 
  
  bootstrap_df
}


df <- bootstrap_df( data = labour2, data_size = 560, boot_rep = 200) 

df %>% ggplot( ) +
  aes( x = estimate ) + geom_histogram(alpha = 0.9) + facet_wrap(~term, scale = "free_x")


df %>% 
  pivot_wider( values_from = estimate, names_from = term) %>% janitor::clean_names() %>% 
  skimr::skim()

percentiler <- 
  df %>% 
  group_by(term) %>% 
  nest( ) %>% 
  mutate( vector_tall = map(data, function(x) {x %>% pull(estimate)})) %>% 
  mutate( percentil = map(vector_tall, function(x) {quantile(x, probs = seq(0.05, by = 0.05))})) %>%  
  mutate( percentil_tidy = map(percentil, broom::tidy)) %>% 
  unnest( percentil_tidy)



percentiler %>%
  filter( names %in% c("25%", "75%"))



# Inference Numeric data-------------------------------------------------------------------

rent_data <- read_csv("https://assets.datacamp.com/production/repositories/846/datasets/bd62fb71666052ffe398d85e628eae9d0339c9c4/manhattan.csv")

# right skiwed  
rent_data %>% ggplot(  ) + aes( x = rent) + geom_histogram()

# Few datapoint - bootstrap with the infer-packages
library(infer)

# Each replica is one calculation o 
replica_median <- rent_data %>% specify( response = rent) %>% generate( reps = 300, 
                                                                        type = "bootstrap") %>% 
  calculate( stat = "median")


replica_median %>% 
  summarise( se = sd(stat))


replica_median$stat %>% quantile(0.1, probs = 0.50)


# Calculate bootstrap CI as lower and upper quantiles
replica_median %>%
  summarize(
    l = quantile(stat, probs =  0.025),
    u = quantile(stat, probs =  0.975)
  ) 

# From previous steps
rent_med_obs <- manhattan %>%
  summarize(median_rent = median(rent)) %>%
  pull()
degrees_of_freedom <- nrow(manhattan) - 1 
t_star <- qt(0.975, df = degrees_of_freedom)

# Calculate the CI using the std error method
rent_med_ci %>%
  # Calculate the std error of the statistic
  summarize(boot_se = sd(stat)) %>%
  # Calculate the lower and upper limits of the CI
  summarize(
    l = rent_med_obs - boot_se  *t_star,
    u = rent_med_obs + boot_se * t_star
  )

# Recall the CI using the percentile method from step 1
rent_med_ci %>%
  summarize(
    l = quantile(stat, 0.025),
    u = quantile(stat, 0.975)
  )



# -------------------------------------------------------------------------

# the data:
ncbirths_complete_visits <- tibble( visits = c(12,14,12,10,11,9,8,14,20))


# From previous steps

ncbirths_complete_visits %>%
  filter(!is.na(visits)) %>% 
  specify( respons = visits) %>% 
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "mean") -> visit_mean_ci

# Calculate the 90% CI via percentile method
visit_mean_ci %>%
  summarize(
    l = quantile(stat, 0.05 ),
    u = quantile(stat, 0.95)
  )



# Calculate the 90% CI via percentile method
visit_sd_ci <- ncbirths_complete_visits %>%
  specify(response = visits) %>%
  generate(reps = 15000, type = "bootstrap") %>%
  calculate(stat = "sd")

# Calculate the 90% CI via percentile method
visit_sd_ci %>%
  summarise(
    l = quantile(stat, 0.05),
    u = quantile(stat, 0.95)
  )






# H-test ------------------------------------------------------------------

# From previous step
n_replicates <- 15000
rent_med_ht <- manhattan %>%
  specify(response = rent) %>%
  hypothesize(null = "point", med = 2500) %>% 
  generate(reps = n_replicates, type = "bootstrap") %>% 
  calculate(stat = "median")
rent_med_obs <- manhattan %>%
  summarize(median_rent = median(rent)) %>%
  pull()

rent_med_ht %>%
  # Filter for bootstrap stat greater than or equal to observed stat
  filter(stat >= rent_med_obs) %>%
  # Calculate the p-value
  summarize(p_val = n() /n_replicates)
















# -------------------------------------------------------------------------




model1_earnings78 <- function(x){lm(str_c(dep_var, "~", str_c(vars, collapse = "+")), data = x ) }

the_bootstrap <- function(size = 4, model, n_size = length(lalonde_controls$age)) {
  
  se_bootstrap <-
    tibble( index = 1:size) %>% 
    group_by(index) %>% 
    mutate( data = map(index, function(x){ sample_n(lalonde_controls, replace = T, size = n_size)} )) %>% 
    mutate( model = map( data, model )) %>% 
    mutate( tidy_model = map(model, summary) ) %>% 
    mutate( tidy_model = map(tidy_model, broom::tidy)) %>% 
    unnest( tidy_model) %>% 
    ungroup() %>% 
    group_by(term) %>% 
    mutate( var_estimate = (estimate - mean(estimate))^2) %>% 
    summarise(bootstrap_std =  sqrt( (1/(size-1))*sum(var_estimate) ) )   %>% 
    mutate( mc_size =  size)
  
  coef_obs <- lalonde_controls %>% lm(lm(str_c(dep_var, "~", str_c(vars, collapse = "+")), data = . ) %>% broom::tidy() %>% 
                                        select( term, estimate) %>% 
                                        left_join( se_bootstrap, by = "term")
                                      
                                      coef_obs %>% 
                                        mutate( z = estimate/bootstrap_std)
                                      
}
the_bootstrap(size = 500, model = model1_earnings78 , n_size = 22828/1)


the_bootstrap(size = 40, model = model1_earnings78)
the_bootstrap(size = 100, model = model1_earnings78)
the_bootstrap(size = 200, model = model1_earnings78)
the_bootstrap(size = 300, model = model1_earnings78)






# Bootstrapping simple ----------------------------------------------------





