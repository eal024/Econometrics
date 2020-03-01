



# 3 Lalonde ---------------------------------------------------------------

fs::dir_ls("Seminar/RawData")

lalonda <-  haven::read_dta("Seminar/RawData/lalonde.dta")

# Data from the NSW - National Supported Work Demonstration
# T: Temporary employment
# Out_Y - earnings78
# Pre-experiment char: age married black edu earning75

lalonda %>% head(n = 10)

skimr::skim(lalonda)

map_dbl(lalonda, function(x) { length(unique(x))} )

lalonda %>%
  filter(sample %in% c(4, 5)) %>%
  group_by(treatment) %>%
  summarise(mean_age =  mean(age),
            n = n(),
            st_dev = sd(age),
            )

sigma <-  (  ( (425-1)*6.59^2 +(297-1)*6.69)/(425+297-2)   )^0.5


se_ <- 11.19*(((1/425)+(1/297))^0.5)

se_^0.5

se_weight <- (sigma)*(((1/425)+(1/297))^0.5)

#
(24.626-24.44)/se_

# 
(24.626-24.44)/se_weight


lalonda %>% 
  filter( sample %in% c(4,5)) %>% 
  t.test( age ~ sample, alternative = c("two.sided") , data = .)


df_filted <- lalonda %>% 
  filter( sample %in% c(4,5))

lsr::independentSamplesTTest( 
  formula = df_filted$age ~ df_filted$treatment ,  # formula specifying outcome and group variables
  data = df_filted,             # data frame that contains the variables
  var.equal = TRUE          # assume that the two groups have the same variance
)  


