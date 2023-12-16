# map_chr(mus06, function(x) {haven::print_labels(x)})
# attr(mus06$ssiratio, "label")

fs::dir_ls("Seminar/RawData")

mus06 <-  haven::read_dta("Seminar/RawData/mus06data.dta") 


exp_drog <- mus06 %>% filter(`_est_ivreg2_0` == 1)

exp_drog <- janitor::clean_names(exp_drog)


exp_drog %>% 
  skimr::skim()

# Wald

# 1. stage
rf_w_1 <- exp_drog %>%  lm_robust( hi_empunion ~ multlc, data = . ) 

rf_w_1 %>% summary()

rf_w_aug <- help_augment(model = rf_w_1, data = exp_drog)

rf_w_aug %>% View()

rf_w_aug %>% select(index, fitted) %>% 
  right_join(exp_drog %>% 
               mutate(index = row_number()),
             by = "index") %>%  
  lm(ldrugexp ~ fitted, data = . ) %>% 
  summary()



exp_drog %>% estimatr::iv_robust( I(log(drugexp)) ~ hi_empunion  + totchr + age + female + blhisp + linc| multlc +  totchr + age + female + blhisp + linc, data = .)

exp_drog %>% estimatr::lm_robust( hi_empunion ~ multlc + totchr + age + female + blhisp + linc , data = .)

