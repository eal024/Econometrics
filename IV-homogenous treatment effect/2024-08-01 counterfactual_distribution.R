
library(data.table)

# the lottery data: Dutch medical schools are assigned through a lottery
dt <- haven::read_dta("data/lottery.dta")  |> as.data.table()
dt1 <- dt[ , .(y = lnw, d, z),]  # Simplify the noations.

# Identify the Pi
stat <- dt1[ ,.(.N), by = .(d,z)]  

pn <- stat[d == 0 & z == 1,N]/nrow(dt1[z == 1]) # Never takers:
pa <- stat[d == 1 & z == 0,N]/nrow(dt1[z == 0]) # Always takers:
pc <- 1-pa-pn                                   # compliers
lm( data =dt1, d ~ z) |> coef()                 # Verify the compliers share

# Counterfactual distributions: Identifying distributions
# fzd(y) = f(y| Z = z; D = d)
# The Always-takers
ya <- dt1[ d == 1 & z == 0,y]
yn <- dt1[ d == 0 & z == 1,y]

# Density A and N
gn <- density(yn)
ga <- density(ya)

plot(gn, ylim = c(0,1.2), xlim = c(1, 5))
lines(ga, col = "red")

# The compliers
f00 <- dt1[ d == 0 & z == 0 & !is.na(y),y]
f10 <- yn
yc0 <- f00*(pc+pn)/pc - f10*(pn/pc)

g00 <- density(f00)

xval <- seq(1,5, length.out = 1000)

g00_approx <- approx(g00$x, g00$y, xval)$y
gn_approx   <- approx(gn$x, gn$y, xval)$y
gc0 <- g00_approx*(pn+pc)/pc - gn_approx*(pn/pc) 


f11 <- dt1[ d == 1 & z == 1 & !is.na(y),y]
g11 <- density(f11)
g11_approx <- approx(g11$x, g11$y, xval)$y
ga_approx   <- approx(ga$x, ga$y, xval)$y
gc1 <- g11_approx*(pa+pc)/pc - ga_approx*(pa/pc)  |> na.omit()

# Ploting the gc0
plot(
    y = plot_data$g0c,
    x = plot_data$x,
    type = "ln",
    ylim = c(0, 1.6),
    xlim = c(1, 5),
    xlab = "Log(wage)"
    )

# Adding the gc1    
dtgc1 <- data.table( g = gc1, x = xval) |> na.omit()
lines(y = dtgc1$g, x = dtgc1$x, lw = 2, col = "black", lty = 2)
lines(ga, col = "red")
lines(gn, col = "blue", lty = 2)

# 
g00_approx <- approx(g00$x, g00$y, xval)$y
gn_approx   <- approx(gn$x, gn$y, xval)$y

gc0 <- g00_approx*(pn+pc)/pc - gn_approx*(pn/pc) 
plot_data <- data.table( g0c = gc0, x = xval) |> na.omit()
