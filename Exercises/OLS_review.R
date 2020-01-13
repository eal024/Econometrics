


# Seimar 1 (2015), Exr.6 --------------------------------------------------

# b)

wage <- function(year_schooling = 0) {
  
  log_wage <- 1.6 + 0.0528*year_schooling
  
  return(exp(log_wage))
}
wage(0)
wage(10)

# Calcucalte F - from use of R2 or SSE

f_1 <- ((0.2159-0.1259)/1)/((1-0.2159)/(31237-3))
f_2 <- ((4395.32-3942.8)/1)/(3942.8/(31237-3))

# t-stat for several variables in regression
t2 <- 59.87^2

f_2
t2

