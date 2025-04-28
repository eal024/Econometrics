
# Packages
library(data.table)
library(fixest)

# Data. Also available in folder data/mus06data
# Medical Expenditure Panel Survey
# Individuals abouve age 65 years.
dt <- haven::read_dta("http://fmwww.bc.edu/ec-p/data/mus/mus06data.dta") 

dt <- as.data.table(dt) # Convert the data into a data.table

# We are interested in how health insurance affects out-of-pocket expenditure on drugs
# y: log(drug expenditure on prescribed medical drugs)
# d: supplemental health insurance (1) and 0 if no
# X: age, gender, linc (log household income), totchr (nr. children), blhisp (black or hispolic)

# Simplifying the nations.
dt1 <- dt[ , .( ldrugexp, hi_empunion, age, female, linc, totchr, blhisp, multlc), ]

dt1 <- na.omit(dt1) # Some na`s -- remove them.

# OLS
model0 <- feols( data =dt1, ldrugexp ~  hi_empunion + multlc + age + female + linc + totchr + blhisp )
model1 <- feols( data =dt1, ldrugexp ~  multlc + age + female + linc + totchr + blhisp )

esttable(
    list(
        model0,   # The endogeous HI: Positive: Those with HI used more on drugs out of pocket
        model1   # Expenditure regressed on controls.    
        )
        )

# a) Endogenity of supplement health insurance (HI):    
    # Endog. can come from several soruces.
    # e.g those who can afford HI, are rich -- can also buy more drugs.
    # Those who are sick, also get HI and need more drugs etc.

# b) IV: If the individual is employed in large operator, multiple locations.  
# Is the IV good or bad?
    # Random assignment:      Not random who works for large firm. Since large firm more often has health Ins., it is likely that large firms attract those who value this more.   
    # Exclution restriction:  Out-of-pocket expenditure affected by z only through d. Seems likely that it affects through other mechanism. 
    # Relevance:
    # Monotonicity: d(1) >= d(0) May hold

# Is the instrument strong: Seems like Z is relvant.
feols( data = dt1, multlc ~ hi_empunion)  t > 10

# c) 
model3 <- feols( data = dt1, hi_empunion ~ multlc + totchr + age + female + blhisp + linc, vcov = "HC1")

esttable( list( 
    model3         # First stage
    ))


# c) Derive the indirect least squares
# y = a + b*d + e
# d = f + t*z + v

# y = a + b(f + t*z + v) + e
# y = (a+b*f) + b*t*z + bv+e
# Y = A + B*z + e
# B/t = b

model_iv  <- feols( data = dt, ldrugexp ~  age + female + linc + totchr + blhisp + 1| hi_empunion ~ multlc) 
model_rfy <- feols( data = dt, ldrugexp ~   multlc + age + female + linc + totchr + blhisp) 
model_rfz <- feols( data = dt, hi_empunion ~   multlc + age + female + linc + totchr + blhisp)

esttable( list( model_iv, model_rfy, model_rfz))

# The indirect IV-estimate:
-0.20/0.1488

#  Interpretation:

# d) What is the share of female in the compliers group:
# P(X = x| d(1) > d(0)) = {E[d|z = 1, X = x] - E[d|z = 0, X = x]/ E[d|z=1] - E[d|z = 0]}*P(X = x)


modela      <- feols(data = dt[ female == 1], hi_empunion ~ multlc + totchr + age + blhisp + linc) 
first_stage <- feols(data = dt, hi_empunion ~ multlc + totchr + age + female + blhisp + linc, vocv = "HC1")

esttable(
    list( 
        condition_female = modela,
        first_stage =first_stage
        ),
    drop = "Constant")

pr_female <- nrow(dt[female ==1])/nrow(dt)
((0.1668)/0.1488)*pr_female # = P(Female|Compliers)
# Female share in the compliers group: 65%
# In sample in general: 58 %  

# P(Female|Compliers)/P(Female) = 0.16676/0.1487 = 112% 
# 12% higher than in the genral population.
# IV-estimate refects relative more the effect on women.


# e) Assume B = 0 (no covariats). Use the moments and calculate the IV
# Na.omit before -- to compare against the solution 
dt1 <- na.omit(dt)[ , .(y = ldrugexp, d = hi_empunion, z = multlc, female),]

# Using the momoments:
# b = cov(y,d)/vard = cov(y,z)/cov(d,z)
esttable(model_iv)
cov_matrix <- as.data.frame( na.omit(dt1) ) |> cov()
-0.016529/(0.014051)

# f) What is the share of females amoung C, A, N
stat <- dt1[ , .N, by = .(d,z)]

pa <- stat[  d == 1 & z == 0,,][[3]]/nrow(dt1[z == 0,]) # Alwaystakers
pn <- stat[  d == 0 & z == 1,,][[3]]/nrow(dt1[z == 1,])
pc <- 1-pa-pn
lm( data = dt1, d ~ z)

stat
stat2 <- dt1[ , .N, by = .(d,z, female)]

stat2[ d == 1 & z == 0,,]
pa_female <- 1792/(1792+3721)  # Females amoung A/(females with z = 0) 
pn_female <- 125/(125+184)     # Females amoung N/(females with z = 1) 

# Compliers
pc_female <- 1-pa_female - pn_female

(pa_female*pr_female)/pa  # Females amoung Always takers
(pn_female*pr_female)/pn
(pc_female*pr_female)/pc

# g) 




