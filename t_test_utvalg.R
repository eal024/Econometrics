
# Data : lalonda - randomisert 
# Ønsker å se om  de som mottar treatment lik de som ikke mottar.
lalonda <- read_csv("lalonda.csv") %>% 
  filter( sample %in% c(4,5))

lalonda_random_long <- 
  lalonda %>%
  select( age, married, black, education, earnings75, treatment) %>% 
  pivot_longer( names_to = "var", values_to =  "value", age:earnings75)

balance_test <- 
  lalonda_random_long %>% 
  group_by(var) %>% 
  nest() %>% 
  # Gjennomfører t-test:
  mutate( balance_t_test = map(data, function(x){t.test(x$value ~ x$treatment, data = .)}))

# Printer ut test-resultatene:
map(balance_test$balance_t_test, ~print(.x))
