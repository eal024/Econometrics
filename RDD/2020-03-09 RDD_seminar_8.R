

# 1. rdrobust -------------------------------------------------------------

# Test of functionalities in rdroboust
library(rdrobust)

data("rdrobust_RDsenate")

rdrobust_RDsenate <- as_tibble(rdrobust_RDsenate)

rdrobust_RDsenate %>% ggplot() +
  aes(x = margin, y = vote) +
  geom_point(alpha  = 0.5)

rdrobust(y = rdrobust_RDsenate$vote, x = rdrobust_RDsenate$margin, c = 0)

rdplot(y = rdrobust_RDsenate$vote, x = rdrobust_RDsenate$margin)

rdrobust_RDsenate

rdrobust_RDsenate %>% ggplot( aes(x = margin, y = vote)) +
  geom_point( alhpa = 0.1) + 
  geom_smooth( data = rdrobust_RDsenate %>% filter( between(margin, -99,0)), aes(margin, vote), method = "lm", se = F, color = "red") +
  geom_smooth( data = rdrobust_RDsenate %>% filter( between(margin, 0,Inf)), aes(margin, vote), method = "lm", se = F, color = "red") +
  geom_smooth( se = F, color = "green", linetype = 2)


plot_RDsenate_low <- rdrobust_RDsenate %>% 
  filter(between(margin, -Inf, 0)) %>% 
  lm( vote ~margin + I(margin^2) + I(margin^3)  , data = .) %>% 
  broom::augment()

plot_RDsenate_high <- rdrobust_RDsenate %>% 
  filter(between(margin, 0, Inf)) %>% 
  lm( vote ~margin + I(margin^2) + I(margin^3)  , data = .) %>% 
  broom::augment()

rdrobust_RDsenate

rdrobust_RDsenate %>% 
  ggplot( ) +
  aes( x = margin, y = vote) +
  geom_line(data = plot_RDsenate_low, aes(margin, .fitted), color = "red" , size = 1.2) +
  geom_point( alpha = 0.1) +
  geom_vline( xintercept = 0, color = "blue", linetype = 2) +
  geom_line(data = plot_RDsenate_high, aes(margin, .fitted), color = "red" , size = 1.2) +
  # Line without model cut-off
  geom_smooth( method = "loess", se = F , color = "black", linetype = 2)



# card and Lavy 1999----------------------------------------------------------

final5 <- haven::read_dta("RDD/final5.dta")

final5_small <- final5 %>% filter(between(c_size, 20,60))

rdrobust(y = final5_small$avgmath, x = final5_small$c_size, c = 40)

p <- rdplot(y = final5_small$avgmath, x = final5_small$c_size, c = 40) 

final5_small %>% mutate(D = ifelse(c_size > 40, 1, 0)) %>% lm( avgmath ~ c_size +  D, data =  .) %>% summary()

# Exercise Two: -----------------------------------------------------------

# Booij, 2016

library(tidyverse)
theme_set(theme_minimal())


# i) Desricptive data
fs::dir_ls("Seminar/RawData")

talent <- haven::read_dta("Seminar/RawData/econ4137sem06rd.dta")

talent %>% skimr::skim()

talent %>% ggplot( aes(ist_raw)) + geom_histogram(alpha = .5) + geom_vline( xintercept = c(94.8, 94.8+15.6, 94.8-15.6), color ="red", linetype =2)

# To print
as.data.frame( talent %>% filter(!is.na(reswis_raw) ) %>% select(-cohort)  ) %>% stargazer::stargazer(type = "text")

# a,c,n
talent %>% 
  count( ist_raw >= x0, treat) %>% 
  pivot_wider(names_from = treat, values_from = n)

talent %>% ggplot( aes(avgmath)) + geom_histogram( aes(y = ..density..), bins = 50)

# Are treated different than non-treaded?
# My opinion is that it should be opposite: male ~treat -> the t-value shows the same (anyway)
talent %>% drop_na() %>% lm(treat ~  male, data = .) %>% summary()

talent %>% drop_na() %>% lm(treat ~  age, data = .) %>% summary()

# Alternativ
talent %>%
  select(treat, age, male, cito_raw, ist_raw) %>%
  pivot_longer(names_to   = "var", values_to = "value", age:ist_raw) %>%
  mutate(type = ifelse(var %in% c("age", "male"), "factor", "number")) %>%
  ggplot(aes(x = var, y = value , fill = factor(treat))) +
  geom_boxplot() +
  facet_wrap(. ~ factor(type), scales = "free_y")


# b) Estimate Treat-effect
talent %>% 
  drop_na() %>% 
  lm( avgmath ~  treat , data = . ) %>% summary

# Whole sample
talent %>% 
  drop_na() %>% 
  lm( avgmath ~  treat + age + male + cohort , data = . ) %>% summary

# b.2) Data below threshold
# subsample

talent %>% count( ist_raw < x0  & treat == 1)

talent %>% 
  drop_na() %>% 
  filter( ist_raw < x0 ) %>% 
  lm( avgmath ~ treat , data = . ) %>% summary


talent %>% 
  drop_na() %>% 
  filter( ist_raw < x0 ) %>%
  lm_robust( avgmath ~   treat + male + age + cohort, data = . ) %>% summary

# Validity of desing
# i) Balancing of predetermined variables:
talent %>% 
  lm(treat ~ male , data = .) %>% summary()

#Alternativ:    talent %>% lm(male ~ treat, data = .) %>% 

talent %>% 
  lm(age ~ treat , data = .) %>% summary()


# Validity test -----------------------------------------------------------

# test z ~ variable
talent %>%   mutate( z = D) -> talent

talent %>% lm(male ~ x_c + treat , data = .) %>% summary()
talent %>% lm(age ~ x_c + treat , data = .) %>% summary()
# conflicted to use may see a sudoeffect because it is correlated with talent and abilitiy
talent %>% lm(cito_raw ~ x_c + treat , data = .) %>% summary()

talent_long <- talent %>%
  select(cohort, x_c, male, age, cito_raw, treat) %>%
  group_by(treat, x_c) %>%
  summarise_at(vars(male, age, cito_raw), mean) %>%
  pivot_longer(names_to = "var", values_to = "value", male:cito_raw)

talent_long %>% 
  ggplot(aes(x = x_c, y = value) )+
  geom_point(alpha = 0.2) +
  geom_smooth( data =filter(talent_long, x_c < 0), 
               aes(x = x_c, y = value), method = "lm", se = F, color = "blue", linetype = 2, inherit.aes = F) +
  geom_smooth( data =filter(talent_long, x_c > 0), 
               aes(x = x_c, y = value), method = "lm", se = F, color = "red", linetype = 2, inherit.aes = F) +
  facet_wrap(~var, scales = "free")


# Density-check:
talent %>% 
  ggplot( aes(x = ist_raw )) + geom_histogram( aes(y = ..density..) , bins = 50) +
  geom_density( aes( y = ..density..), color = "steelblue", linetype = 2, bins = 200, size = 1) +
  geom_vline( data = talent, aes(xintercept = x0), color = "red" ) +
  facet_wrap( ~cohort, scales = "free")

talent %>% 
  ggplot( aes(x = ist_raw )) + geom_histogram( bins = 100) +
  geom_vline( data = talent, aes(xintercept = x0), color = "red" ) +
  facet_grid( cohort ~male, scales = "free")




# First stage -------------------------------------------------------------

# First stage
model_first_stage <- talent %>% lm_robust(treat ~ z + x_c + factor(male) + age + factor(cohort) + cito_raw, data = .  )

# Reduced form
model_reduce_form <- talent %>% lm_robust(avgmath ~ z + x_c + factor(male) + age + factor(cohort) + cito_raw, data = .  ) 

# Wald
first_stage <- model_first_stage$coefficients[["z"]]
reduced_form <- model_reduce_form$coefficients[["z"]]

wald <- reduced_form/first_stage

wald

# Alternativ:
iv_wald_estimate <- talent %>%  iv_robust(avgmath ~ treat + x_c + factor(male) + age + factor(cohort) + cito_raw|z + x_c + factor(male) + age + factor(cohort) + cito_raw, data = .   )

iv_wald_estimate %>% summary()

ols <-  talent %>% lm_robust(avgmath ~ treat + x_c + factor(male) + age + factor(cohort) + cito_raw, data = .)

ols




# IV - secound stage -----------------------------------------------------

# Counting cohort as factor or numeric makes a different! 
<iv_robust <- talent %>%
  iv_robust(avgmath ~ treat + x_c + (male) + age + (cohort) + cito_raw|z + x_c + (male) + age + (cohort) + cito_raw, data = .   )


iv_robust %>% summary



# Graph -------------------------------------------------------------------

rdplot(y = talent$avgmath, x = talent$x_c, c = 0)

talent %>%
  group_by( x_c ) %>%
  summarise( avgmath = mean(avgmath, na.rm = T))  %>%
  ggplot( aes(y = avgmath, x = x_c)) +
  geom_point(position = "jitter") +
  geom_vline( xintercept = 0, color = "red") + 
  geom_smooth(data = filter(talent, x_c < 0 ), aes(y = avgmath, x = x_c), method = "lm" ,inherit.aes = F, se = F, color = "red") +
  geom_smooth(data = filter(talent, x_c < 0 ), aes(y = avgmath, x = x_c), method = "loess" ,inherit.aes = F, se = F, color = "red", linetype = 2) +
  geom_smooth(data = filter(talent, x_c > 0 ), aes(y = avgmath, x = x_c), method = "lm" ,inherit.aes = F, se = F, color = "black") +
  geom_smooth(data = filter(talent, x_c > 0 ), aes(y = avgmath, x = x_c), method = "loess" ,inherit.aes = F, se = F, color = "black", linetype = 2) +
  theme_bw()


# Spesification test and sensitivitie -------------------------------------

# Is the IV-model sensitive to distance from x_c = 0?
df_interval <- tibble(interval = c(4, 6, 10,15, 20, 25, 30,40, 50),
                      data = list(talent, talent, talent, talent, talent,talent,talent,talent,talent)) %>%
  mutate(sub_samples = map2(data, interval , function(x, y) {
    filter(x, abs(x_c) < y)
  })) %>%
  mutate(model = map(sub_samples, function(x) {
    estimatr::iv_robust(
      x$avgmath ~ x$treat + x$x_c + x$male + x$age + x$cohort + x$cito_raw |
        x$z + x$x_c + x$male + x$age + x$cohort + x$cito_raw,
      data = x
    )
  }))


df_interval %>% 
  mutate( b_iv_treat = map(model, function(x) {broom::tidy(x)})) %>% 
  unnest(b_iv_treat) %>% 
  filter(str_detect(term, "treat")) %>% 
  select(interval, term, estimate, contains("conf")) %>% 
  ggplot(aes(x = interval, y = estimate )) + geom_point( size = 2, color = "red") +
  geom_errorbar( aes(ymin = conf.low, ymax = conf.high)) +
  geom_hline( yintercept = c(0,0.62), linetype = c(2,3), color = c("red", "blue"), size = c(1,2))



# Controling for the running variable in a more flex way? -----------------


talent_power <- talent %>%
  mutate( z = ifelse(ist_raw > x0, 1, 0),
          x_c = ist_raw - x0,
          avgmath = reswis_raw )%>%
  mutate( p_2 = x_c^2, p_3 = x_c^3) %>%
  tidyr::drop_na()


iv_robust_more_flex <- talent_power %>%
  estimatr::iv_robust(avgmath ~ treat + x_c + p_2 + p_3  + (male) + age + (cohort) + cito_raw|z + x_c + p_2 + p_3  + (male) + age + (cohort) + cito_raw, data = .   )


augment <-  iv_robust_more_flex %>% 
  helper::help_augment(., data = talent_power )


talent_power %>% 
  group_by(x_c) %>%
  summarise( avgmath = mean(avgmath, na.rm = T)) %>%
  ggplot( aes(y = avgmath, x = x_c)) + geom_point() +
  geom_line( data = augment %>% select(avgmath, fitted, x_c) %>%
               group_by(x_c) %>%
               summarise( fitted = mean(fitted, na.rm = T) ) %>% filter( x_c < 0), aes(y = fitted, x = x_c),
             color = "red", inherit.aes = F,
             size = 1.2) +
  geom_line( data = augment %>% select(avgmath, fitted, x_c) %>%
               group_by(x_c) %>%
               summarise( fitted = mean(fitted, na.rm = T) ) %>% filter( x_c > 0), aes(y = fitted, x = x_c),
             color = "blue", inherit.aes = F,
             size = 1.2, linetype = 1) +
  geom_vline( xintercept =  0) +
  # Add linear and local linear regression line form the smooth
  theme_bw()




# Kernel! -----------------------------------------------------------------
















# Appendix::: Delete/keep? Balance test of treated -------------------------------------------------

talent %>% 
  group_by(cohort) %>% 
  nest() %>% 
  mutate( model_diff = map(data, function(x) {lm(x$treat ~x$male, data =.) %>% broom::tidy() })) %>% 
  mutate( diff = map(data, function(x) {x %>% summarise( mean_t = mean(male[treat == 1]),
                                                         mean_non_t = mean(male[treat == 0]),
                                                         mean_t - mean_non_t,
                                                         sd = sd(male),
                                                         t = ((mean_t - mean_non_t)/sd)) } ) ) %>%
  unnest(diff) %>% 
  ungroup() %>% 
  summarise_all( mean )

talent %>% 
  group_by(treat) %>% 
  nest() %>% 
  #mutate( model_diff = map(data, function(x) {lm(x$treat ~x$male, data =.) %>% broom::tidy() })) %>% 
  mutate( diff = map(data, function(x) {x %>% summarise( mean_t = mean(male),
                                                         sd = sd(male))} ) ) %>% unnest(diff)


talent %>% 
  mutate( qualified = ifelse(ist_raw > x0, 1,0) ) %>% 
  group_by(cohort  ) %>%
  nest() %>% 
  mutate( balance_male = map(data, function(data) { lm( male  ~ qualified , data = data) %>% broom::tidy()} ),
          balance_age  = map(data, function(data) { lm( age  ~ qualified , data = data) %>% broom::tidy()}) ) %>% 
  pivot_longer( names_to = "balance", values_to =  "list", balance_male:balance_age) %>% 
  unnest(list) %>% 
  filter(! str_detect(term,"(Intercept)")) %>% 
  mutate( var = str_c(cohort, " ", balance)) %>% 
  separate( balance, into = c("cohort_text", "var")) %>% select(-cohort_text) %>% 
  ggplot( aes( x = var, y = (abs(statistic)), fill = ifelse(estimate > 0, "red", "blue")  )) +
  geom_col() +
  coord_flip() +
  geom_hline( yintercept = 1.9, linetype = 2, color = "red", size = 1.2) +
  theme_minimal() + theme(legend.position = "none") +
  labs( title = "Balance check. Is age or sex correlated with treatment?", 
        subtitle = "The bars shows t-stat from model var ~ qualified to treatment program\n
        red color indicate negative correlation, blue indicate positive",
        y = "t-stat in absolute value", x =  "var") + 
  facet_grid(cohort~.)


# Density for the running variable
talent %>% ggplot( ) + aes(x = ist_raw ) + geom_histogram( bins = 150) + facet_wrap(~cohort)


# Balance check -> Histogram
talent %>% 
  ggplot( aes(x = ist_raw )) + geom_histogram( aes(y = ..density..) , bins = 50) +
  geom_density( aes( y = ..density..), color = "steelblue", linetype = 2, bins = 200, size = 1) +
  geom_vline( data = talent, aes(xintercept = x0), color = "red" ) +
  facet_wrap( ~cohort, scales = "free")

talent %>% 
  ggplot( aes(x = ist_raw )) + geom_histogram( bins = 100) +
  geom_vline( data = talent, aes(xintercept = x0), color = "red" ) +
  facet_grid( cohort ~male, scales = "free")

talent %>% 
  mutate( x_c = ist_raw - x0) %>% 
  select(cohort,treat , x_c ,D,x0, ist_raw  ,avgmath, male, age,cito_raw) %>% 
  pivot_longer( names_to = "var", values_to = "value", male:cito_raw) %>% 
  ggplot() + 
  aes( x = x_c, y = value ) + 
  geom_point( alpha =.1) +
  geom_smooth( se = F) +
  geom_vline( xintercept = 0) +
  facet_wrap(var~., scales = "free") 



# d)

# First stage
talent %>%  lm(treat ~ ist_raw, data = .) %>% summary()

talent %>% 
  mutate( D = ifelse(ist_raw >= x0, 1  , 0 )) %>% 
  lm( treat ~ D, .) %>% summary()

# First stage
# D = 1  if qualified, otherwise 0.

talent %>% mutate( D = ifelse(ist_raw >= x0, 1  , 0 ), x_c = ist_raw-x0) -> talent 

# correct  
talent %>% estimatr::lm_robust( treat ~ D + x_c + as.factor(cohort) + male + age + cito_raw , .) %>% summary( )

# talent %>% estimatr::lm_robust( treat ~ ist_raw + cohort + male + age + cito_raw , .) %>% summary( )

# Graph
talent %>% ggplot( ) + aes( x = ist_raw, y = treat, color = factor(D)) + geom_point( alpha = 0.7) +
  labs( y = "Recived Treatment", x = "Score from cognitive test (ist_raw)", title = "Graph showing student being a part of the\nGifted and talenented program or not.",
        subtitle = "Treatet equal 1 indicate student being a part of the program. Blue-colored dots indicate that the student is qualifed") +
  scale_y_continuous( breaks = c(0,1)) +
   theme( legend.position = "none") 
  # +
  # geom_smooth(data = talent, aes(y = treat, x = ist_raw), method = "lm", se = F, inherit.aes = F)

library(estimatr)
# 

talent <- talent %>% mutate(avgmath = reswis_raw )

talent %>% 
  lm_robust(avgmath ~ D + x_c + male + age + factor(cohort) + cito_raw, data = .) 

talent %>%
  ggplot( ) +
  aes( x = ist_raw, y = avgmath, fill = factor(D), color = factor(D) ) +
  geom_point(alpha = 0.5) + geom_smooth(data = talent, aes(x = ist_raw, y = avgmath), se =F, inherit.aes = F, method= "lm")



#

# IV-regression f: The effect of T on math --------------------------------

options(scipen = 9)

ols <- talent %>% lm_robust(avgmath ~ x_c + treat + age + male + factor(cohort), data = .)


iv_model_math <- talent %>% iv_robust( avgmath ~ ist_raw + treat + age + male + factor(cohort) | ist_raw + D + age + male + factor(cohort), data = .)

iv_model_math_2 <- talent %>% iv_robust( avgmath ~ ist_raw + treat + age + male + cohort | D + ist_raw + age + male + cohort, data = .)


summary(iv_model_math)
summary(iv_model_math_2)
summary(ols)



# g) Restricting the sample -----------------------------------------------

df_near_cut <- tibble(
  sample = c("10", "20", "30", "60"), data = list(filter(talent, abs(ist_raw - x0) < 10),
                                      filter(talent, abs(ist_raw - x0) < 20),
                                      filter(talent, abs(ist_raw - x0) < 30),
                                      filter(talent, abs(ist_raw - x0) < 60))
  )


df_near_cut %>% 
  mutate( model = map(data, function(x) { iv_robust(x$avgmath ~ x$ist_raw + x$treat + x$age + x$cohort| x$ist_raw + x$D + x$age + x$cohort, data = .) })) %>% 
  mutate( tidy_model = map(model, broom::tidy)) %>% 
  unnest( tidy_model) %>% 
  filter( str_detect(term, "treat" ))
  
geom_vline(data = df2, mapping = aes(xintercept = Mean.SL))

talent %>%
  rename( cut_off = x0)  %>% 
  ggplot( ) +
  aes( ist_raw, y = avgmath, fill = factor(D), color = factor(D) ) + 
  geom_point( alpha = 0.5) + 
  geom_vline(data = talent, mapping = aes(xintercept = x0), color = "red") +
  geom_smooth( method = "lm", se = F) +
  facet_wrap(~cohort , scales = "free") + 
  theme( legend.position =  "none")


## h)
df_near_cut %>% 
  mutate( model = map(data, function(x) { iv_robust(x$avgmath ~ x$ist_raw + I((x$ist_raw)^2) + I((x$ist_raw)^3) + x$treat + x$age + x$cohort| x$ist_raw + x$D + x$age + x$cohort, data = .) })) %>% 
  mutate( tidy_model = map(model, broom::tidy)) %>% 
  unnest( tidy_model) %>% 
  filter( str_detect(term, "treat" ))


talent %>%
  rename( cut_off = x0)  %>% 
  ggplot( ) +
  aes( ist_raw, y = avgmath, fill = factor(D), color = factor(D) ) + 
  geom_point( alpha = 0.5) + 
  geom_vline(data = talent, mapping = aes(xintercept = x0), color = "red") +
  geom_smooth( method = "loess", se = F) +
  facet_wrap(~cohort , scales = "free") + 
  theme( legend.position =  "none")


