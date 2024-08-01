
# Instrument variable regression with hetrogenous treatment effect.
# Topic also named LATE (Local Average treatment Effect) 

# Example 2 is based on seminar exercises ECON 4137 (UiO)

# Case:
# Seats in Dutch medical schools are assigned through a lottery. Applicants to medical 
# studies in the Netherlands are assigned to lottery categories based on their high
# school grades.

# data: 
# Result from first lottery outcome participants 1988 and 1989
# attended mical school
# earnings (from survey) 2007

# Goal: Estiate the return to attending medical school on earnings 2007

# Library
library(data.table)
library(fixest)
library(tinyplot)

# data
dt <- haven::read_dta("data/lottery.dta") |> as.data.table()
skimr::skim(dt) # Descriptive data

# a) Discuss IV
#) i) Exogeneity: Random assignment + exlution restriction

# Random assignment: IV independent of all potential outcomes
# z indepentent on y
# z indepentent on d
# Plausible since it was determined by lottery
# if z indep. y: We can calcualte the reduce form

# Exlusion restriction: P.O only depend on treatment (d), not on z
# P.O can be written as: y(d,z) = y(d)

# Y not affeceted by z
# But non eligibl may get motivated by the lottery to study harder?

# ii) Monotonicity: d(z = 1) >= d(z = 0)
# All those affected by z are affected same direction.
# Seems plausible.


# b) Assess IV relevance: The IV predict treatment
# First stage can verify
mean(dt$d[dt$z == 1]) - mean(dt$d[dt$z == 0])
model_fs   <- feols( data = dt, d ~ z)
model_fs_c <- feols( data = dt, d ~ z + year + I(factor(lotcateg)) + female + I(factor(lotcateg) |> as.numeric() *female) |> as.factor() )

esttable( list(model_fs, model_fs_c), keep = "z")

# c) Is your discussion above includes covariats?
# Not included
# If Z not random, covariats is needed
# But strait forward to include Xs
# The X should be predetermed, such as gender etc.
# Not something that seems to be related to y


model_rf <- feols( data = dt, d ~ z + female + year + lotcateg)
model_sf <- feols( data = dt, lnw ~ 1| d ~ z)
model_sf2<- feols( data = dt, lnw ~ 1| d  ~ z  + I(factor(lotcateg)) + female + I(factor(lotcateg) |> as.numeric() *female) |> as.factor()  )
esttable(list(model_rf, model_sf, model_sf2), keep =  "z|d")

# d) What is the increaed return to earnings?
# y is log(earnings)
exp(0.1850)-1 # Medical school seems to increase earnings by 20 percent in 2007.
# This is the LATE -- causal effect for compliers, of being drafted eligible for med-school, and attend the program.

# e) Count the numer of compliers and compare them to the population in terms of gender.

# Compliers:
# Always-takers:
# Never-takers: 
stat <- dt[ , .(.N), by = .(z,d)] |> dcast(z~d) |> setkey(NULL)
setnames(stat, c("z", "do", "d1"))

pa <- 187/(269+187) # Always takers
pn <- 71/(71+949)  # Never takers
pc <- 1-pa-pn # Compliers
# verify the numrs by:
feols( d ~ z, data = dt)

mean(dt$female)
feols(d~z, data = dt |> subset(female == 1))
feols(d~z, data = dt)
0.5/0.52
# P(female|compliers) = 0.5/0.52
pc_females <- 0.5/0.52 # Amoung Compliers, female are 3 percent underrepresented

# f) The LATE is not ATT, with out assumptions.
# If non-compliance is one-sided, LATE is equal to the ATT (or ATU)
# p( d = 1|z = 0) = 0 , or P(d = 0|z = 1) = 0

# g) Y[0] and Y[1] for the compliers

# Y[0]  = Y[0c]pc/(pc+pn) + Y[0n]pn/(pc+pn) 
# Y[1]  = Y[1c]pc/(pc+pa) + Y[0c]pa/(pc+pa) 
# Y[0c] = { Y[0]*(pc+pn) - Y[0n]*pn }*(1/pc)
y00  <- mean(dt$lnw[dt$d == 0 & dt$z == 0])
yn0  <- mean(dt$lnw[dt$d == 0 & dt$z == 1]) # Nevertakers
y0c  <- y00*(pc+pn)*(1/pc) - yn0*pn*(1/pc) 

# Y[1c] = { Y[1]*(pc+pa) - Y[1a]*pa }*(1/pc)
y11  <- mean(dt$lnw[dt$d == 1 & dt$z == 1]) 
ya1  <- mean(dt$lnw[dt$d == 1 & dt$z == 0]) # Always takers
y1c  <- y11*(pc+pa)*(1/pc) - ya1*pa*(1/pc)

LATE <- y1c - y0c
feols( data = dt, lnw ~ 1|d~z)

# g) Estimate the Compliers mean Y0 and Y1 using regression.

# Transform variable for regression.
dt[ ,let(
    `1-d` = (1-d),
    y_1_d = lnw*(1-d),
        yd = lnw*(d) 
    ),
    ]

# IV regression for estimation of:
#yc[0] = E[y*1-d | z == 1]- E[y*1-d | z == 0]/{ E[1-d | z == 1]- E[1-d | z == 0]}
#yc[1] = E[y*d | z == 1]- E[y*d | z == 0]/{ E[1-d | z == 1]- E[1-d | z == 0]}

modely0c <- feols( data = dt, y_1_d ~ 1| `1-d` ~ z)
modely1c <- feols( data = dt, yd ~ 1| d ~ z)
esttable( list(modely0c, modely1c)) # LATE is yc1- yc0
3.264-3.077

# h) What can we say about y0, y1 for always takers
dens_a <- density(dt$lnw[dt$d == 1 & dt$z == 0]) # Always takers
dens_n <- density(dt$lnw[dt$d == 0 & dt$z == 1]) # Never takers

plot( dens_a, col = "red", ylim = c(0,2) )
lines( dens_n, col = "blue")
legend( "topright", legend = c("Always takers", "NT") , col = c("red", "blue"), lwd = 2)


ya <- dt$lnw[dt$d == 1 & dt$z == 0]
yn <- dt$lnw[dt$d == 0 & dt$z == 1]

#
dt_plot <- data.table( 
    value = c( ya, yn),
    kat   = c( rep("A", times = length(ya)), rep("N", times = length(yn)) )
)

## The plot
tinyplot::tinyplot( type = "density", data = dt_plot, ~value|kat, main = "densiyt ya and yn" )


## Density plot for yc1 and yc0

# Generate densities for the relevant subgroups
density_d1_z1 <- density(dt$lnw[dt$d == 1 & dt$z == 1])  # Compliers + Always-takers
density_d1_z0 <- density(dt$lnw[dt$d == 1 & dt$z == 0])  # Always-takers
density_d0_z0 <- density(dt$lnw[dt$d == 0 & dt$z == 0])  # Never-takers

# Ensure the densities are evaluated at the same points
x_vals <- density_d1_z1$x
g1c <- approx(density_d1_z1$x, density_d1_z1$y, x_vals)$y
f1a <- approx(density_d1_z0$x, density_d1_z0$y, x_vals)$y

# Calculate the density for compliers (f1c)
f1c <- g1c* (pc + pa) / pc - f1a * pa / pc



# Adding the y0c
density_d0_z0 <- density(dt$lnw[dt$d == 0 & dt$z == 0])  # Never takers + compliers
density_d0_z1 <- density(dt$lnw[dt$d == 0 & dt$z == 1])  # Never takers
g0c <- approx(density_d0_z0$x, density_d0_z0$y, x_vals)$y
f0n <- approx(density_d0_z1$x, density_d0_z1$y, x_vals)$y

# Calculate the density for compliers (f1c)
f0c <- g0c * (pc + pn) / pc - f0n * pn / pc

# Plot the densities
# 1) the density for the yc1
plot(
    x_vals, 
    f1c,
    type = "l",
    col = "red",
    lwd = 2,
    ylim = c(0, max(f1a, na.rm = T) + 1), 
    ylab = "Density", xlab = "lnw", main = "Density Plot of Y1 for Compliers"
    )
# Adding the vertical line for mean 
abline( v = y1c, col = "black", lnw = 2, lty = 2) 
# 2) The yc0 in the same plot:
lines( x_vals, f0c, lnw = 2, col = "darkgreen")
abline( v = y0c, col = "darkgreen", lty = 1, lnw = 2)


fn_normalize <- function(d){ d/max(d,na.rm= T) }

n_f1c <- fn_normalize(d = f1c)
n_f0c <- fn_normalize(d = f0c)

plot(x_vals, n_f1c, type = "l", col = "red", lwd = 2)
lines(x_vals, n_f0c)
