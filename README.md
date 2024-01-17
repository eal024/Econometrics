# Econometrics

Collection of scrips of econometrical topics and examples 

## OLS
- OLS in R: the Implementing with [`lm()`](https://github.com/eal024/Econometrics/blob/master/OLS/ols.R), with [matrix form]() 
- [Simulate OLS properties](https://github.com/eal024/Econometrics/blob/master/OLS/ols_properties.R)
- [Partial out regression OLS](https://github.com/eal024/Econometrics/blob/master/OLS/ols_partial_out_regressors.R)

## Standard errors  
- [Standard errors in OLS](https://github.com/eal024/Econometrics/blob/master/OLS/standarderrors.R)
- [Weighted least square](https://github.com/eal024/Econometrics/blob/master/OLS/weightedls.R)

## Standard errors
- [OLS standard errors, in matrix form]()

### Bootstrap 
-[example 1 (non paramteric)](https://github.com/eal024/Econometrics/blob/master/bootstrap_example1.R)
-[example 2(`lapply()` and the `boot()` library)]()


## Difference in difference 
- [Intro to DiD (2X2)](https://github.com/eal024/Econometrics/blob/master/intro_did.R) The basic 2x2 set up. Implementing DiD with `lm()` and with the `fixest` package, and the `felosl()` function. Also, a simple example of the event study setup, using `event()`.
- [Diff in diff in diff]() The intuition for when to use and the setup with base R and the `lm()`.
- [TWFE](https://github.com/eal024/Econometrics/blob/master/twfe.R)  Demonstrate the problem with TWFE (DID) -- staggered treatment. Treatment at different times and with different effects. 
- [The Goodman Bacon decomposition of the TWFE-estimate](https://github.com/eal024/Econometrics/blob/master/goodmanbacon_decomposition.R)
- [TWFE](https://github.com/eal024/Econometrics/blob/master/twfe.R)  Demonstrate the problem with TWFE (DID) -- staggered treatment. Treatment at different times and with different effects. 
