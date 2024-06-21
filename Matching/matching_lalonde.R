

# Data
df <- haven::read_dta("data/lalonde2.dta")
distinct(df, sample)

# Subsample the randomized data
df1 <- df |> subset( sample == 1)

# t-test
tbl <- tibble( 
    y = names(df1),
    data = list(df1 )
    ) 

tbl1 <- tbl |> 
    mutate( t = map2( data, y, \(df,y){
        lm( data = df, formula = as.formula( paste( y, "~ treated")) ) |> broom::tidy()
        } )
        ) 

tbl1 |> 
    select(y,t) |> 
    unnest(t) 



## Estimate the effect using OLS
df1 <- df |> filter( treated == 1)
df2 <- df |> filter( sample == 2)

# NSW treated and controls from CPS
dat <- rbind(df1, df2) |>  mutate( d = ifelse( is.na(treated), 0, 1))

# Creating addition variable for regression
dat1 <- dat |> 
    mutate( 
        age2 = age^2,
        age3 = age^3,
        educ2 = educ^2,
        zero74 = re74 == 0,
        zero75 = re75 == 0
     ) 

vars <- c("d", "age", "age2", "age3", "educ", "educ2", "re74", "re75", "kids18", "zero74", "zero75", "kidmiss", "black", "married", "nodegree")
formula_string <- paste("re78 ~", paste(vars, collapse = " + "))
model_ols <- feols( data = dat1, fml = as.formula(formula_string) )

esttable(list(ols = model_ols))


## Balancing properites









