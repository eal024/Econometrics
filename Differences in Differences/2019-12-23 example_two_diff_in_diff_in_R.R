library(tidyverse);
theme_set(theme_bw())

eitc <- haven::read_dta(here::here("differences in Differences/eitc.dta"))


# The effect of eitc: "..The EITC thus benefits low-income Americans in both rural and urban counties. The impact on poverty is far greater 
  #   on workers with children than on those without."

eitc <- eitc %>% as_tibble()

skimr::skim(eitc, work, year, children)

# EITC takes place 1994. 
# Treatment if ind./familiy has/have more than 1 child

eitc_w_treat <-
  eitc %>% mutate( post93 = ifelse(year >= 1994, 1,0),
                 anykids = ifelse( children > 1, 1, 0),
                 did = post93*anykids)


eitc_w_treat %>% 
  sample_n( 15) %>% 
  head( n = 15)


# The model: work = β0+ δ0post93 + β1anykids + δ1(anykids×post93) +ε.
# The model: work = β0+ δ0post93 + β1anykids + δ1did +ε.

# DID: Estimate have the EITC have work, after treatment


eitc_w_treat %>% lm( work ~ post93 + anykids + did, data = .) %>% summary

# The EITC has in an positive effect on the probability for the ind. to work. 5 %


eitc_w_treat %>% 
  group_by(anykids, year) %>% 
  summarise( mean_work = mean(work)) %>% 
  ggplot( ) +
  aes( y = mean_work, x = year, fill = factor(anykids), color = factor(anykids) ) +
  geom_line( linetype = "twodash") +
  geom_smooth( se = F) +
  geom_vline( xintercept = 1994) +
  theme( legend.position = "bottom")




