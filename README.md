# Econometrics

Collection of scrips of econometrical topics and examples 


## Toolbox

### Linear regression

#### OLS
- OLS: the Implementing with [`lm()`](https://github.com/eal024/Econometrics/blob/master/OLS/ols.R), and with [matrix form]() 
- [Simulate OLS properties](https://github.com/eal024/Econometrics/blob/master/OLS/ols_properties.R)
- [Partial out regression OLS](https://github.com/eal024/Econometrics/blob/master/OLS/ols_partial_out_regressors.R)
- [Weighted least seqare](https://github.com/eal024/Econometrics/blob/master/OLS/wls.R)

##### Spesification and interpretation  
- 

### Instrument variable regression (IV)
#### IV with homogenous treatment effect
- [The simple illustration](), [the Wald estimate](), and the [Structur and reduced form eqations]()
- [Example 1:]() Seminar exercise ECON 4137 UiO

#### [IV with heterogeneous treatment effect:]()
- [Example 1:]() How is the mother's labor supply affected by the number of children (IV: Two children of the same sex, and twins as IV)?
- [Example 2:]()
- [Example 3:]()
- [Example 4:]()
- The [Hausmans test of endogenous X]()

### Inference and more on Standard errors  

#### Standard errors  
- [Standard errors in OLS](https://github.com/eal024/Econometrics/blob/master/OLS/standarderrors.R)
- [Weighted least square](https://github.com/eal024/Econometrics/blob/master/OLS/weightedls.R)

### Bootstrap 
- [Bootstrap sample]()
- [The empirical CDF]()
- [example 1](https://github.com/eal024/Econometrics/blob/master/bootstrap_example1.R) non-paramteric and the `replicate()` function.
- [example 2]() the `lapply()` function and `boot()` packages.
- [example 3]() seminar case - sample, beta, percentile, and t-percentile method. compare with ML estimate. 

## Non-parametric methods
- [Manually calculated](https://github.com/eal024/Econometrics/blob/master/Nonparametric/histogram_density_manually_calculation.xlsx) Histogram and Nonparametric regression calculated in Excel.
- [The histogram](https://github.com/eal024/Econometrics/blob/master/Nonparametric/histogram.R): Implementing calculation of the histogram with Non param. Example based on a small dataset and Epanechnikov Kernel.
- [Local linear regression, Nadaraya Watson](https://github.com/eal024/Econometrics/blob/master/Nonparametric/local_linear_regression.R) The calucation in [Excel](https://github.com/eal024/Econometrics/blob/master/Nonparametric/histogram_density_manually_calculation.xlsx) translated in to general `function()` `s in R. 


## Difference in difference 
- [Intro to DiD (2X2)](https://github.com/eal024/Econometrics/blob/master/intro_did.R) The basic 2x2 set up. Implementing DiD with `lm()` and with the `fixest` package, and the `felosl()` function. Also, a simple example of the event study setup, using `event()`.
- [Diff in diff in diff]() The intuition for when to use and the setup with base R and the `lm()`.
- [TWFE](https://github.com/eal024/Econometrics/blob/master/twfe.R)  Demonstrate the problem with TWFE (DID) -- staggered treatment. Treatment at different times and with different effects. 
- [The Goodman Bacon decomposition of the TWFE-estimate](https://github.com/eal024/Econometrics/blob/master/goodmanbacon_decomposition.R)
- [TWFE](https://github.com/eal024/Econometrics/blob/master/twfe.R)  Demonstrate the problem with TWFE (DID) -- staggered treatment. Treatment at different times and with different effects. 


## Matching
- [Exact matching:]() 1:1 matching, based on covariats.
- []()
- [Constructing til Propensity Score](https://github.com/eal024/Econometrics/blob/master/Matching/2024-04-06%20example1_propensityscore.R) Example from LaLonda (1989), from [the Mixtape](https://mixtape.scunning.com/05-matching_and_subclassification). Using `glm()` and the maximum likehood estimator and testing the [`tidymodels packages`](https://github.com/eal024/Econometrics/blob/master/Matching/2024-04-06%20logistic_reg_glm_and_tidymodels.R)
- [Example 1: Lalonde from the Mixtape](https://github.com/eal024/Econometrics/blob/master/Matching/2024-04-06%20example1_propensityscore.R) Example from LaLonda (1989), from [the Mixtape](https://mixtape.scunning.com/05-matching_and_subclassification). Using `glm()` and the maximum likehood estimator and testing the [`tidymodels packages`]
- [Example 2: Lalonde from the Seminar (ECON 5100)](https://github.com/eal024/Econometrics/blob/master/Matching/2024-04-06%20example1_propensityscore.R) 
- [Example 3: Effect from union membership, from seminar ECON 5100)()
