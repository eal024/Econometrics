
head(as_tibble(mtcars))

ggplot( mtcars, aes(mpg, hp)) + 
  geom_point(size = 2, shape = 3, color ="red") +
  theme_minimal()


usethis::edit_rstudio_snippets()