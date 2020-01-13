
tidy_indekser_til_reg <-
  indekser %>% 
  dplyr::select(navn_index, date, value) %>% 
  mutate( kvartal = case_when(
    between(month(date), 1,3) ~ "Q1",
    between(month(date), 4,6) ~ "Q2",
    between(month(date), 7,9) ~ "Q3",
    T ~"Q4"
  ) 
  ) 




nested_index <-
  tidy_indekser_til_reg %>% 
  group_by(navn_index, year = year(date), kvartal) %>% 
  summarise( value = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate( date = str_c(year,":0", str_sub(kvartal, start =2))) %>% 
  dplyr::select(navn_index, date, value, kvartal) %>% 
  drop_na() %>% 
  filter( navn_index != "eg5_lønn") %>% 
  mutate( date = as.yearqtr(date,format = "%Y:0%q")  ) %>% 
  as.data.frame() %>% 
  group_by(navn_index) %>% 
  nest()

nested_index %>% 
  unnest(data)


model <- function(df) { lm(log(df$value) ~ df$Time, data = df)}
model2 <- function(df) { lm(log(df$value) ~ df$Time + (df$kvartal), data = df)}
 

model_index <-
  nested_index %>% 
  mutate( df = map(data, function(x) { xts::xts(x$value, x$date) } )) %>% 
  mutate( df2 = map(df , function(x) {data.frame(value = x , Time = index(x), kvartal = str_sub(index(x), 6,7) ) } ) ) %>% 
  mutate( model1 = map(df2 , function(x) {model(x) } ),
          model2 = map(df2, function(x) {model2(x)} ) ) %>% 
  mutate( model11 = map(model1, function(x) {lmtest::coeftest(x, vcov= sandwich::NeweyWest(x))} %>% broom::tidy() ),
          model22 = map(model2, function(x) {lmtest::coeftest(x, vcov= sandwich::NeweyWest(x))} %>% broom::tidy() ) ) 


#model_index$model1

test <- 
  model_index  %>%
  unnest(model11) %>% 
  filter( term  != "(Intercept)")

model_index %>% 
  unnest(model2) %>% 
  filter( term  != "(Intercept)")

model_index %>% unnest(model11) 
 

model_index$model1


# stargazer ---------------------------------------------------------------

streng_navn <-  model_index %>% ungroup() %>% mutate( navn_index =  ifelse( str_detect(navn_index, "_samlet"), "matr_ger", navn_index)) %>% pull(navn_index)

streng_navn <- c("en", "to")

stargazer::stargazer(model_index$model1[1], model_index$model1[2], type = "text") 

stargazer::stargazer(model_index$model1, 
                     type = "text"    , 
                     dep.var.caption = "Avhengig variabel: Log(Indeks-verdi)",
                     covariate.labels = "År",
                     dep.var.labels.include = F,
                     column.labels =map_chr(streng_navn, function(x) {str_replace(x,pattern = "_", replacement = "-")}),
                     omit = c("Constant"),
                     out = "model1_time.tex") 





stargazer::stargazer(model_index$model1, 
                     type = "text"    , 
                     dep.var.caption = "Avhengig variabel: Log(Indeks-verdi)",
                     covariate.labels = "År",
                     dep.var.labels.include = F,
                     column.labels =map_chr(streng_navn, function(x) {str_replace(x,pattern = "_", replacement = "-")}),
                     omit = c("Constant"),
                     out = "model1_time.tex") 



stargazer::stargazer(model_index$model2, 
                     type = "text"    , 
                     dep.var.caption = "Avhengig variabel: Log(Indeks-verdi)",
                     covariate.labels = c("År", "Kvartal Q2", "Kvartal Q3", "Kvartal Q3", "Konstant"),
                     dep.var.labels.include = F,
                     column.labels =map_chr(streng_navn, function(x) {str_replace(x,pattern = "_", replacement = "-")}),
                     #omit = c("Constant"),
                     align = T,
                     out = "model2_time_kvartal.tex")



# Figur -------------------------------------------------------------------





