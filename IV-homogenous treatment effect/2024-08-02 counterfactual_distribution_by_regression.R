
library(data.table)
library(fixest)

# The lottery data: Dutch medical schools are assigned through a lottery
dt <- haven::read_dta("data/lottery.dta")  |> as.data.table()

# E[y1|c] and E[y0|c] can be found by regression.

# 1) E[y1|c]
dt[ ,
    let( 
         y    = lnw,
         yd   = lnw*d,
        `1-d` = (1-d),
        y_1_d = lnw*(1-d)
    ),
    ]

model_late <- feols( data = dt, y ~ 1| d ~z)  
model_y1c  <- feols( data = dt, yd ~ 1| d ~z)
model_y0c  <- feols( data = dt, y_1_d ~ 1| `1-d` ~z)

# LATE should be equal y1c - y0c
esttable( list( model_late , model_y1c, model_y0c))
3.264-3.077

# Why it is usefull to know about y1c and y0c:
    # is the return to education due to: high y1c (high earning in case of education) or
    # from low earnings without edu (y0)
