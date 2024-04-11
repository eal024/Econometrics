
library(tidymodels)


# data
data <- vroom::vroom("data/lalonda_prepered.csv")

# 2) glm
model1 <- glm( 
    data = data, 
    treat ~ age + agesq + agecube + educ + educsq + marr + nodegree + re75 + re78 + u74 + u75 + edu_re74 + re74sq + re75sq + u74_hisp,
    family = binomial( link = "logit")
)


# Logit formula 
logit_model <- logistic_reg(
    mode = "classification", 
    engine = "glm"
)

# Recipe
rec <- recipe( 
    treat ~ age + agesq + agecube + educ + educsq + marr + nodegree + re75 + re78 + u74 + u75 + edu_re74 + re74sq + re75sq + u74_hisp, 
    data = data |> mutate( treat = factor(treat) )
    )


wf_logit <- workflow() |> 
    add_recipe(rec) |> 
    add_model( logit_model) 

model2 <- wf_logit |> fit(data = data |> mutate( treat = factor(treat) ))

modelsummary::modelsummary( list( model1, model2))


