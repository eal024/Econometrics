


# library and data-------------------------------------------------------------
library(tidyverse)

indekser <- readxl::read_excel("Timeseries/2020-01-03 indekser.xlsx") %>% mutate(date = lubridate::ymd(date))

intdef <- wooldridge::intdef %>% as_tibble()

# Example 10.2 Wooldridge
intdef %>% 
  lm( i3 ~ inf + def, data = .) %>% summary
  
# i3: 3 mnd T-bill
# price i3 increase with inflation 0.6 and with 0.5 def (fed. deficit, % of GDP)
# Int. rate icrease as inflation increase


# Indeks-examplet ---------------------------------------------------------
library(lubridate)
library(xts)
library(zoo)

df_regression <- 
  indekser %>% 
  mutate( kvartal = case_when(
    between(month(date), 1,3) ~ "Q1",
    between(month(date), 4,6) ~ "Q2",
    between(month(date), 7,9) ~ "Q3",
    T ~"Q4"
  ) 
  )  %>% 
  group_by( navn_index, year = year(date), kvartal = factor(kvartal, levels = c("Q1", "Q2", "Q3","Q4")) )%>% 
  summarise_at( vars(value), mean ) %>% 
  arrange(navn_index, desc(year), desc(kvartal)) %>% 
  ungroup()



# Sjekk xts-object --------------------------------------------------------

df_xts_obj <- 
  df_regression %>%  
  mutate( date = str_c(year,":0", str_sub(kvartal, start = 2)  )) %>% 
  mutate( date = as.yearqtr(date,format = "%Y:0%q") ) %>% 
  dplyr::select(- year, -kvartal ) %>% 
  as.data.frame()
  
df_xts_obj %>% head()

df_xts_obj_gp24 <- filter(df_xts_obj, navn_index == "gp2452")

xts_gp24_clean <- xts(df_xts_obj_gp24$value, df_xts_obj_gp24$date )


quants <- function(df_xts_index) {
  s <- df_xts_index
  return(
    data.frame("Level" = s,
               "Logarithm" = log(s),
               "AnnualGrowthRate" = 400 * log(s / lag(s)),
               "1stLagAnnualGrowthRate" = lag(400 * log(s / lag(s))))
  )
}

xts_gp24_to_reg <- quants(xts_gp24_clean)

xts_gp24_to_reg$time <- 0:(nrow(xts_gp24_to_reg)-1)

model <- lm( Logarithm ~ time , data = xts_gp24_to_reg)


summary(model)



# tibble - model ----------------------------------------------------------

df_regression %>% 
  filter( navn_index == "gp2452") %>% 
  lm( I(log(value)) ~ year + factor(kvartal), data = .) %>% summary



# -------------------------------------------------------------------------

df_regression %>% 
  pivot_wider( names_from = kvartal, values_from = kvartal) %>% 
  mutate_at( vars(contains("Q")), ~ifelse(is.na(.x), 0, 1)) %>% 
  filter(navn_index == "gp2452") %>% 
  dplyr::select(-navn_index) %>% 
  lm( I(log(value))  ~ . , data = .) %>% summary


df_regression %>%  
  mutate( kvartal = fct_relevel(kvartal,  "Q4")) %>% 
  filter(navn_index == "gp2452") %>% 
  lm( I(log(value)) ~ year + I(year^2) + kvartal*year, data = .) %>% summary


df_regression %>% 
  filter(! str_detect(navn_index, "samlet")) %>%
  mutate( kvartal = fct_relevel(kvartal,  "Q4")) %>% 
  group_by(navn_index) %>% 
  nest() %>% 
  mutate( model_poly = map(data, function(x) { lm( log(x$value) ~ x$year + I(x$year^2) + x$kvartal) })) %>% 
  mutate( model_poly_tidy = map(model_poly, function(x) {tidy(x)})) %>% 
  unnest(model_poly_tidy ) %>% 
  filter( str_detect(term, "Q")) %>% 
  head( n = 20)


# Graph -------------------------------------------------------------------

# linje for å 
snitt_per_kvartal <- 
  df_regression %>% 
  filter( navn_index %in% c("gp2452", "gp2651", "pg28")) %>% 
  mutate( time = str_c(year,"-0", str_sub(kvartal, start = 2))) %>%
  mutate( endring = ( (lag(value) - value )/ value) ) %>%
  group_by(navn_index, kvartal ) %>% 
  summarise( mean = mean(endring, na.rm =T))


theme_set(theme_light())

df_regression %>% 
  filter( navn_index %in% c("gp2452", "gp2651", "pg28")) %>% 
  mutate( time = str_c(year,"-0", str_sub(kvartal, start = 2))) %>%
  mutate( endring = ( (lag(value) - value )/ value) ) %>% 
  filter( !is.na(endring)) %>% 
  filter( between(endring,  -0.05, +0.05 ) ) %>% 
  #left_join( select(snitt_per_kvartal c(navn_index, mean)), by = "navn_index") %>% 
  mutate( time = str_sub(time,end = 4)) %>% 
  ggplot( ) +
  aes( time, y =  endring ) +
  geom_col( alpha = 0.7, color = "gray") +
  theme( panel.grid.minor.x = element_blank() , panel.grid.major.x = element_blank() ,axis.title.x = element_blank()) +
  geom_smooth( aes(group = kvartal), method = "lm", se = F , color = "steelblue", size = 1.2) +
  geom_hline( yintercept = mean  ) +
  facet_grid( kvartal ~ navn_index, scales = "free_y") +
  scale_y_continuous( labels = scales::percent_format()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    
  

  

