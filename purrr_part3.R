

library(tidyverse)
# 
temp <- tempfile()
dir.create(temp)

cyls <- split(mtcars, mtcars$cyl)
paths <- file.path( temp, paste0( "cyl-", names(cyls), ".csv") )

# The sideeffect
walk2( cyls, paths, function(x, y) x %>% write.csv(y) )

dir(temp)

# iterate over index
#  imap(x, f) is equivalent to map2(x, names(x), f) i
# f x has names, and map2(x, seq_along(x), f) if it does not.

# If it has names:
imap( iris, ~paste0("the first value of", .y, " is ", .x[[1]]))

# If no name
x <- map(1:6, ~sample(1000, 10))

imap_chr( x, ~paste0( "The highets value of ", .y," is ", max(.x) ) ) 

tibble( line = imap_chr( x, ~paste0( "The highets value of ", .y," is ", max(.x) ) ))

# 
pmap(  list(n = c(10,5,2), mean = c(5,2,2), sd = c(1,2,3)) , function(n,mean,sd) { rnorm(n = n, mean = mean, sd = sd)} )


pmap( tibble( n = c(10:15), mean = 4, sd = 1), function(n,mean, sd) rnorm(n, mean, sd) )




# Reduce ------------------------------------------------------------------

reduce( 1:4, sum )

reduce(c(10,20,30), sum)

reduce(c(10,20,30), mean)




































