library(stargazer)

# Explained (for example): https://online.stat.psu.edu/stat501/lesson/13/13.1

# OLS assume constant variance in the error (homoscedasticity)
# WLS can be used when homo is violated

# Eksempel:
# Tenk deg at du analyserer huspriser, men at du har mer usikkerhet (større varians) i prisdata for luksusboliger enn for gjennomsnittsboliger:
# Med OLS: Alle hus behandles likt, selv om prisdata for luksusboliger er mindre pålitelige.
# Med WLS: Prisdata for luksusboliger får lavere vekt, slik at analysen tar hensyn til usikkerheten.


# Example
data <- read.delim("https://online.stat.psu.edu/onlinecourses/sites/stat501/files/data/galton.txt")

# consisting of 7 measurements each of X = Parent (pea diameter in inches of parent plant) and
# Y = Progeny (average pea diameter in inches of up to 10 plants grown from seeds of the parent plant). 
# Also included in the dataset are standard deviations, SD, of the offspring peas grown from each parent.

#  we should downweight the observations with a large standard deviation and upweight the observations with a small standard deviation

model1 <- lm( data = data, Progeny ~ Parent)
model2 <- lm( data = data, Progeny ~ Parent, weights = 1/(data$SD)^2)

# Model
stargazer::stargazer( 
    list( 
        model1 = model1,
        model2 = model2
        ),
    type ="text",
    digits = 3
)

# Interpretation: 
#  The standard deviations tend to increase as the value of Parent increases, so the weights tend to decrease as the value of Parent increases.
# Thus, on the left of the graph where the observations are up-weighted the red fitted line is pulled slightly closer to the data points, whereas on the right of the graph where the observations are down-weighted the red fitted line is slightly further from the data points.

# In practice, for other types of datasets, the structure of W is usually unknown, so we have to perform an ordinary least squares (OLS) regression first.

# Example two created data:
N <- 100
x <- seq(0, 10, length.out = N) + rnorm( n = N, mean = 2, sd = 1)
error <- rnorm( n = length(x), mean = 0, sd = 4)
y <- 4 + 2*x + error

tbl <- tibble( y = y, x = x)        
summary( model <- lm( data = tbl, y ~ x ) )  # The model
plot( x, y, main = "scatter plot")  # plot
abline( model, col = "red", lwd = 2)


