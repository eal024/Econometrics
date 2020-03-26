
library(tidyverse)
# log likelihood function

log_p <- function(N,N1,p) {
  N1*log(p) + (N-N1)*log(1-p)
}

log_p(N = 100, N1 = 44, p = 0.1)


map_dbl(seq(from = 0.1, to = 0.8, by = 0.1), function(x) {log_p(100, 44, x)} ) %>% 
  set_names(seq(from = 0.1, to = 0.8, by = 0.1)) %>% 
  enframe() %>% 
  ggplot( ) +
  aes( x = as.numeric(name), y = value ) + 
  geom_smooth( se = F) + 
  geom_point( aes( x = 44/100, y = log_p(100, 44, 44/100)), size = 2, color = "red")



    

  
  
  
