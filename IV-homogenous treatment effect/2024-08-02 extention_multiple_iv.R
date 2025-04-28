
library(data.table)
library(fixest)

# The lottery data: Dutch medical schools are assigned through a lottery
dt <- haven::read_dta("data/lottery.dta")  |> as.data.table()
