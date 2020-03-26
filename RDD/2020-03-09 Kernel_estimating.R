

library(tidyverse)
library(dslabs)

data("polls_2008")


polls_2008 %>% ggplot( aes(x = day, y = margin)) + geom_point(size = 1.9)
qplot(day,margin, data = polls_2008)


# create smooth
span <- 100
fit <- with(polls_2008, ksmooth(day,margin, kernel = "box", bandwidth = span ))


polls_2008 %>% mutate(smooth = fit$y) %>% 
  ggplot( aes(day, margin)) +
  geom_point( size = 3, alpha = 0.5, color = "gray") + geom_line( aes(day, smooth), color = "red")
