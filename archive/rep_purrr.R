

list_a <- list( a = "Dette er tekst", b = c(1,2,3), c = as_tibble(mtcars), d = NULL)


pluck(list_a, 2)

map(list_a, 1)

keep(list_a, function(x) {is.character(x)})

discard(list_a, function(x) {is.character(x)})

# Drop NULL
compact( list_a)

# Reshape list
flatten(keep(list_a, function(x) {!is.tibble(x)}))

flatten_chr(keep(list_a, function(x) {!is.tibble(x)}))
flatten(keep(list_a, function(x) {!is.tibble(x)}))


list_a[[3]]
keep(list_a, is.numeric )[[1]]
transpose(keep(list_a, is.numeric ))[[3]]


test_red <- seq(1:10)

reduce(test_red, sum)
reduce(test_red, sum, .dir = "backward")

reduce(letters[1:3], str_c)

str_c_min <- function(x,y) {str_c(x, sep = " og ", y)}

reduce(letters[1:5], str_c_min)

accumulate(letters[1:3], str_c_min)

# accumilate
reduce(test_red, sum)
accumulate(test_red, sum)

reduce(test_red, function(x,y){y/x})
# How the steps goes:
accumulate(test_red, function(x,y){y/x})



# Many argunments ---------------------------------------------------------


test <- list(
  mean = list(10, 20, 30),
  sd = list(2,3,5),
  n = list(10, 20, 50)
)

pmap(test, function(mean, sd, n) {rnorm(n, mean, sd)})

head(mtcars %>% as_tibble())

test2 <- list(
  var_y = list("mpg", "cyl", "hp"),
  var_x = list("disp", "wt", "carb"),
  var_c = list("am", "am", "am"),
  data = list(mtcars,mtcars,mtcars)
)


pmap(test2, function(var_y, var_x, var_c, data) {lm(str_c(var_y,"~",var_x,"+", var_c), data = data) %>% tidy()})



# compose -----------------------------------------------------------------
library(broom)
library(tidyverse)

clean_aov <- compose( tidy, anova, stats::lm )

clean_aov( Sepal.Length ~ Sepal.Width, data = iris)

se <- partial( mean, na.rm  = T)

round_mean <- compose( 
  partial(round, digits = 2),
  partial( mean, na.rm = T)
  )

round_mean( c(c(1:4), c(0.5:9, NA) ))










