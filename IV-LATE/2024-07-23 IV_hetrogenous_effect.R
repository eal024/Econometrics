
library(fixest)
library(data.table)

# IV and hetrogenous effect.
# Individuals differ in their gains of treatment.
# IV can be used to estimate LATE: Local average treatment effect.

# If there is hetrogenous effect of treatment: Beta differ for each


# Example of the LATE set up: 
# LATE: Average T (d) effect of those who are affected by the IV (z)

# Assumption needed for id LATE:
    # 1)
    # 2)
    # 3)
    # 4)

# Exampe from the Twins-data
angev <- haven::read_dta("data/angev98.dta")

angev$z1 <- angev$samesex  # 2 first child has the same sex 
angev$z2 <- angev$twins2   # Alternative IV; those gets twins 
angev$d  <- angev$morekids # d variable of intrest is more than 2 kids
angev$y  <- angev$workedm  # Labor market outcome, such as hour worked

# Vector form
z1 <- angev$samesex  # 2 first child has the same sex 
z2 <- angev$twins2   # Alternative IV; those gets twins 
d  <- angev$morekids # d variable of intrest is more than 2 kids
y  <- angev$workedm  # Labor market outcome, such as hour worked

# First stage:
(fs_same  <- mean(d[ z1==1 ])-mean(d[ z1==0 ]))
(fs_twins <- mean(d[ z2==1 ])-mean(d[ z2==0 ]))

# Reduced form: effect from z on y
(rf_same <- mean(y[ z1==1 ])-mean(y[ z1==0 ])  )
(rf_twins <- mean(y[ z2==1 ])-mean(y[ z2==0 ]) )

# IV
rf_same/fs_same
rf_twins/fs_twins

# Regression with SE
model_rf_same  <- feols( data = angev, d ~ z1)
model_rf_twins <- feols( data = angev, d ~z2)
model_iv_samesex <- feols( data = angev, y ~ 1|d~z1)
model_iv_twins <- feols( data = angev, y ~ 1|d~z2)
esttable( list( fs1 = model_rf_same, fs2 = model_rf_twins, iv_same =  model_iv_samesex, iv_twins = model_iv_twins), keep = "(z\\d|d)")

## We can identify the share of Complier, Never takers and Always takers-----
dt <- as.data.table(angev)
table <- dt[ , .( n = .N), by = .(d,z1)] |> dcast(d~z1)

Pa <- 72643/(122649 + 72643)  # P(d = 1| z = 0)
Pn <- 113440/(86108 + 113440) # P(d = 0| z =1)
Pc <- 1-Pa-Pn

# The size of the compliers, can also be calucalted from the first stage:
lm( data = dt, d ~ z1)

# Can also identify how much Y with or with out treatment
Ey00 <- mean(dt$y[dt$d == 0 & dt$z1 == 0 ]) # d = 0, z = 0
Ey01 <- mean(dt$y[dt$d == 0 & dt$z1 == 1 ]) # d = 0, z = 1: Never takers

Ey11 <- mean(dt$y[dt$d == 1 & dt$z1 == 1 ]) # d = 1, z = 1 # Compliers
Ey10 <- mean(dt$y[dt$d == 1 & dt$z1 == 0 ]) # d = 1, z = 0 # Always takers


# E[y0cc]
Ey0c <- Ey00*(Pc + Pn)/Pc - Ey01*(Pn/Pc)
# E[y1cc]
Ey1c <- Ey11*(Pc + Pa)/Pc - Ey10*(Pa/Pc)
esttable(model_iv_samesex)
LATE <- Ey1c - Ey0c


# Average potential outcomes from calculation.
# Can also calcualte the compliers estimated average outcomes.

dt1 <- dt[ , let( 
    y0c = y*(1-d), 
    d0c = (1-d),
    y1c = y*(d), 
    d1c = (d)
    ), ]

average_potential_out <- dt1[ , .(
    y0c = mean(y0c[z1 == 1]) - mean(y0c[z1 == 0]),
    d0c = mean(d0c[z1 == 1]) - mean(d0c[z1 == 0]),
    y1c = mean(y1c[z1 == 1]) - mean(y1c[z1 == 0]),
    d1c = mean(d1c[z1 == 1]) - mean(d1c[z1 == 0])
    ),]

yc0 <- -0.036/-0.0595
yc1 <- 0.02824/0.0595
LATE_alternative_calcualted <- yc1-yc0


## Example Instrument "Same sex" first and second born child.

firststage  <- feols( d ~ z1 , data = dt)      # First stage
dt$hatx <- predict(firststage)
secondstage <- feols( y ~ hatx , data = dt)    # 2SLS
ivreg <- feols( y ~ 1|d~samesex, data = dt)    # IV-reg
esttable(list(firststage, secondstage, ivreg)) # Result

# Can tell if the outcome is low if not treated, and high if treated
yc1  # Estiamted P.O if treated
yc0  # Estiamted P.O if not treated
LATE <- yc1 - yc0 

# Balancing the sample: How different is the treated vs. not treated
# Can tell us who the compliers are
vars <- c("agem", "boy1st", "boy2nd", "blackm", "hispm" )

dt[ , lapply(.SD, \(x) mean(x)), .SDcols = vars, by = z1]

