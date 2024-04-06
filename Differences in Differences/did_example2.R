
#
library(data.table)
library(fixest)

# The effect of eitc: "..The EITC thus benefits low-income Americans in both rural and urban counties. The impact on poverty is far greater 
#   on workers with children than on those without."

dt <- haven::read_dta(here::here("differences in Differences/eitc.dta")) |>
    as.data.table()


# Prepering
dt[ , 
    ':='
    ( post    = ifelse( year > 1993, 1, 0),
      anykids = ifelse( children > 1, 1, 0) 
    ),
    ]

#
model <- lm( 
    data = dt,
    work ~ post + anykids + I(post*anykids) 
    )

summary(model)

modelf <- feols( data = dt, work ~ post + anykids + i(post*anykids), vcov = vcov_cluster("state"))

esttable(modelf )
iplot(modelf)
