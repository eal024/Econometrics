

library(tidyverse)
# Waterfall

balance <- tibble(
  desc = c(
    "Starting Cash",
    "Sales",
    "Refunds",
    "Payouts",
    "Court Losses",
    "Court Wins",
    "Contracts",
    "End Cash"
  ),
  amount = c(2000, 3400,-1100,-100,-6600, 3800, 1400, 2800)
)

balance <- balance %>% mutate( start = 0) %>% 
  mutate( end = cumsum(amount) , start = lag(end), start = ifelse(is.na(start), 0, start), end = ifelse( str_detect(desc, "End"), 0, end)) %>% 
  mutate( type = case_when( str_detect(desc, "End") | str_detect(desc, "Start") ~ "Net",
                            amount > 0  ~"Income",
                            T ~ "Cost"
                            ),
          id = row_number()
          ) 


ggplot( balance) + 
  aes( desc, fill = type ) +
  geom_rect( aes(x = desc, xmin = id - 0.45, xmax = id + 0.45, ymin = end, ymax = start)) + theme(axis.text.x = element_text(angle = 90), legend.position = "none")

