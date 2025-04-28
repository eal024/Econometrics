
# Seminar exercises from the course econ 5100, UiO 

library(data.table)
library(fixest)

# The lottery data: Dutch medical schools are assigned through a lottery
dt <- haven::read_dta("data/lottery.dta")  |> as.data.table()

# The goal is to estimate the return to attending medical school on earnings 2007.

# a) Discuss the instrunent

# exogeneity: Random assignemnt and exclusion restriction.
    # Random assignment: Seems plassible, since the assignment is based on lottery.
    # Exlusion restriction: The causal effect of z on y ins only due to effect on d
        # The result of the lottery may motivate or demotivate the students.
        # This may give the z effect through other chanels than the medical school
    # Monotonicity: d(1) >= d(0): If some one goes to medschool if not assigned through the lottery. visa versa

# Important to only keep first time drafters. If it is posible to take part in the lottery a second time, this may inflict the estimate.


