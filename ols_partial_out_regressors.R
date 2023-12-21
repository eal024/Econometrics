

library(ggplot2)

# To partial out regressors is useful when checking your specification

auto <- causaldata::auto

# Want to find how much of the variation are explained by weight or headroom
model <- lm( price ~ weight + headroom, data = auto)

coef(model)

# First get the residuals from headroom (what is not explain of headroom)
model1 <- lm( price ~ headroom, data = auto)
e1 <- residuals(model1) # The left info in price

model2 <- lm( weight ~ headroom, data = auto) # How does weight correlate with headroom (what info does headroom allready give?)
w1 <- residuals(model2)

df <- data.frame( e1 = e1, w1 = w1, weight = auto$weight, headroom = auto$headroom)

# Explain the rest of the y, from the info in weigh, not given by headroom
model3 <- lm( e1 ~w1, data = df)

coef(model)  # The weight variable gives coef is 2.4. Telling each additional weight unit, increase price by 2.4
coef(model3) # The left variation in model1 (e1), where weight is left out, is correlated 2.4 with the part in weigth that is not corrrelated with headroom.

# The coef in model3 is equal the coef. in model1

df |>
    ggplot( 
        aes( x = w1, y = e1) 
    ) +
    geom_point(size = 6) +
    geom_point( aes( y = weight, x = w1), color= "red")

df |> 
    ggplot( aes( y = weight, x = headroom) ) +
    geom_point() +
    geom_smooth( method = "lm", se = F)



