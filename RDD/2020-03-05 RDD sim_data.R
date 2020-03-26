# RDD sharp simulated data ------------------------------------------------

set.seed(123)

N <- 1000


df <- tibble(x = rnorm(n = N , mean = 50, sd = 25 ), D = ifelse(between(x,0,49.9),0,1) ) %>% 
  mutate( x = ifelse(x < 0, 0, x),
          y = ifelse(D == 0, 25 + 0*D + 1.5*x + rnorm(N, sd = 20),  (25 + 40*D + 1.5*x + rnorm(N, sd = 20)) ) ) 
df %>%   
  ggplot( ) + 
  aes( y = y, x = x , group = D) + 
  geom_point( alpha = 0.5 )  +
  geom_smooth( data = df %>% filter( D < 1  ) ,method = "lm", color = "red", se = F) +
  geom_smooth( data = df %>% filter(between(x, 50,Inf))  ,method = "lm", color = "red", se = F, size = 1.1) +
  geom_smooth( data = df %>% filter( D <1  ) , se = F, color ="black",linetype = 2) +
  geom_smooth( data = df %>% filter(between(x, 50,Inf))  , color = "black", se = F, size = 1.1, linetype = 2)



df %>% 
  mutate( x_c = x - 50) -> df


df %>% 
  lm( y ~ D + x_c + I(x_c*D), data  = .) %>% summary()


df %>% 
  mutate( in_treat = ifelse( x > 50, 1, 0)) %>% 
  ggplot( ) + 
  aes( y  = in_treat, x = x ) +
  geom_point() + geom_vline( xintercept = 50)


# DRR fuzzy

df %>% 
  mutate( d_f = case_when( between(x,0,30) ~ 0,
                           between(x,30,50) ~ sample(c(0,1) , replace = T, prob = c(2,1) )[1],
                           between(x,50,60) ~sample(c(0,1) , replace = T, prob = c(1,2))[1],
                           T ~ sample(c(0,1) , replace = T, prob = c(0,1))[1]
                           ) )


sample(c(0,1), size = 100, replace = T , prob = c(1,3)) %>% sum()


# rdd - packages ----------------------------------------------------------
library(rdd)

reg1 <- rdd::RDestimate(y ~  x,data = df, cutpoint = 50)


plot(reg1)



# simulating data - non-linear data ---------------------------------------
# Replica Mixtape page 

df <- tibble(x = rnorm(n = N , mean = 50, sd = 25 ),
             x_p2 = 0.05*x^2,
             x_p3 = x^3,
             D = ifelse(between(x,0,49.9),0,1), 
             y = ifelse(D == 0, 25 + 0*D + 1.5*x + x_p2 + rnorm(N, sd = 20),  25 + 40*D + 1.5*x + x_p2 + rnorm(N, sd = 20) ) ) 

#
qplot(x = x, y = y, data = df, alpha = 0.1) + geom_vline(xintercept = 50, color = "red")












