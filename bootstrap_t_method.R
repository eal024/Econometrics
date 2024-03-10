

# data
df1

# Funksjon fit model

fn_coef <- function(x) { 
    
    coef <- coef(
    glm( data = x, marital ~ age + I(age^2) + education, family = binomial(link = "probit") )
        )

    
    }
    


fn_modeling <- function(data) {
    glm( data = data, marital ~ age + I(age^2) + education, family = binomial(link = "probit"))
} 
