library(tidyverse)


# Example 2.3.3 -----------------------------------------------------------
fs::dir_ls()
ind_wages <- read.delim("Verbeek Guide to Modern Econ/data_verbeek/bwages.dat")

ind_wages <- as.tibble(ind_wages)

ind_wages

ind_wages %>% lm( WAGE ~MALE , data = .) %>% summary()

model1 <- ind_wages %>% lm( WAGE ~MALE , data = .)

# Hourly wage rate is given $10.26 + 1.3 (If male == 1).

ind_wages %>% 
  ggplot( ) +
  aes( y = WAGE, x = MALE) +
  geom_point() +
  geom_smooth( method = "lm"  , se = F)


model1_augment <- model1 %>% broom::augment()


R2 <-model1_augment %>% 
  mutate( .resid_2 = .resid^2, 
          y_haty_2 = (WAGE-mean(WAGE))^2) %>% 
  summarise( SSR = sum(.resid_2 ),
             SST = sum(y_haty_2),
             R2 = 1-(SSR/SST))

# R2 <-
str_c("R2 i modellen er: ", R2 %>% pull(R2),". Som er kun 2% forklaring av variasjonen i WAGE") 
# FOrklarer at Gender ikke har stor betyning i forklaring av lønn.

# Hypotesetesting:
# Model 2
model1  %>% tidy( conf.int = T) %>% 
  mutate( c_95_low = estimate - std.error*1.96,
          c_95_high = estimate - std.error*1.96) 

model1  %>% tidy( conf.int = 0.95) %>% 
  mutate( c_95_low = estimate - std.error*1.96,
          c_95_high = estimate - std.error*1.96) %>% 
  ggplot( ) + aes( y= estimate, term  ) +
  geom_point() +
  geom_errorbar( aes(ymin = conf.low, ymax = conf.high) ) +
  coord_flip() +
  geom_hline( yintercept =  0) +
  ylim(-2, 12)



model1  %>% tidy( conf.int = 0.95) %>% 
  mutate( c_95_low = estimate - std.error*1.96,
          c_95_high = estimate - std.error*1.96)

## Test if b1 - b2 = 0
ind_wages %>% 
  lm( WAGE ~ EDUC + MALE, data = .) %>% summary

ind_wages %>% 
  mutate( EDUC_MALE = EDUC + MALE) %>% 
  lm( WAGE ~ EDUC + EDUC_MALE, data = .) %>% summary

# Education variable minus male. Cant reject the H0, that be > bM.



# Example 2.5.5 -----------------------------------------------------------

<- ind_wages %>% lm(WAGE ~ MALE + EDUC + EXPER , data = .)

GGally::ggpairs(ind_wages)

glue_info <- model2 %>% augment() %>% 
  mutate( .resid_2 = .resid^2, 
          y_haty_2 = (WAGE-mean(WAGE))^2) %>% 
  summarise( SSR = sum(.resid_2 ),
             SST = sum(y_haty_2),
             R2 = 1-(SSR/SST))


model2 %>% tidy() %>% mutate( ci_low = estimate-std.error*1.96, ci_high = estimate+std.error*1.96  ) %>% 
  filter( !str_detect(term, "(Intercept)" )) %>% 
  ggplot( aes( y = estimate, x = fct_reorder(term, estimate, )  )) +
  geom_point() +
  geom_errorbar( aes(ymin = ci_low, ymax = ci_high)) + 
  geom_hline( yintercept = 0.0) +
  coord_flip( ) +
  labs( title = glue::glue("Estimate from the model wage ~ expriance + educ + Male\n Where R2 is: ", 
                           format(glue_info %>% pull(R2), digits = 2 ) ),
        x = "Term", y = "Estimate value")



model2 %>% summary
model1 %>% summary



# Example Exercises -------------------------------------------------------

ind_wages %>% as_tibble()

wages <- read.delim("Verbeek Guide to Modern Econ/data_verbeek/wages1.dat") %>% as_tibble()


wages %>%
  sample_n( size = 545) %>% 
  lm( WAGE ~ MALE , data = .) %>% summary














