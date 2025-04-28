# Eksempel double robust Lasso og Matching. 
# Med bruk av ChatGPT, 4o

# 📦 Laster inn nødvendige pakker
library(tidymodels)

# 🎲 Simulerer datasett med n observasjoner og p kovariater
set.seed(123)
n <- 500
p <- 20
X <- matrix(rnorm(n*p), ncol = p)
colnames(X) <- paste0("X", 1:p)  # Navngir kolonner som X1, X2, ..., X20

# 🧪 Lager utfallsvariabel og behandling (den sanne modellen)
# - Y påvirkes kun av X1 og X2
# - D (tiltak) påvirkes av X1 og X3 (via logistisk link)
Y <- 1 + 2*X[,1] - 3*X[,2] + rnorm(n)
D <- rbinom(n, 1, plogis(1.5*X[,1] - 1*X[,3]))

# 📋 Lager dataframe med Y, D og X1–X20
df <- as_tibble(X) |> 
    mutate(
        Y  = Y,
        D = as.factor(D)  # Gjør D til faktor for klassifikasjonsmodell
    ) |> 
    select(Y, D, everything())  # Flytter Y og D først

# 🔍 Tester to naive lineære modeller for illustrasjon
model1 <- lm(data = df, Y ~ 1 + X1 + X2)      # "sann" modell
model2 <- lm(data = df, Y ~ 1 + X1 + X2 + X3 + X4)  # med unødvendige kovariater
modelsummary::modelsummary(list(model1, model2), stars = T)

# 🧾 Recipe for behandling (treatment)
# - Fjerner utfallsvariabelen Y
# - Normaliserer kovariatene
rec_treat <- recipe(D ~ ., data = df) |> 
    step_rm(Y) |> 
    step_normalize(all_predictors())

# 🧾 Recipe for outcome (Y)
# - Fjerner behandling D
# - Normaliserer kovariatene
rec_outcome <- recipe(Y ~ ., data = df) |> 
    step_rm(D) |> 
    step_normalize(all_predictors())

# 🔧 Modellspesifikasjon: Lasso for behandling (logistisk regresjon)
lasso_spec <- logistic_reg(
    penalty = tune(),  # Skal tunes (lambda)
    mixture = 1        # Lasso (ikke ridge)
) |> 
    set_engine("glmnet")

# 🔧 Lasso for utfall (lineær regresjon)
linear_reg_lasso_spec <- linear_reg(
    penalty = tune(),
    mixture = 1
) |> 
    set_engine("glmnet")

# 🔁 Kryssvalidering: 5-fold CV
folds <- vfold_cv(df, v = 5)

# ⚙️ Workflow for behandling
wf_treat <- workflow() |> 
    add_model(lasso_spec) |> 
    add_recipe(rec_treat)

# 🔍 Kjører tuning av behandling (propensity score-modell)
treat_res <- tune_grid(wf_treat, resample = folds, grid = 20)

# 📌 Velger beste lambda basert på AUC (klassifikasjon)
best_lambda_treat <- select_best(treat_res, metric = "roc_auc")

# 🚀 Finaliserer workflow med valgt lambda og fitter på hele datasettet
final_treat_fit <- finalize_workflow(wf_treat, best_lambda_treat) |> 
    fit(data = df)

# ⚙️ Workflow for outcome
wf_outcome <- workflow() |> 
    add_model(linear_reg_lasso_spec) |> 
    add_recipe(rec_outcome)

# 🔍 Kjører tuning for outcome-modellen (prediksjon av Y)
outcome_res <- tune_grid(wf_outcome, resamples = folds, grid = 20)

# 📌 Velger beste lambda basert på RMSE (regresjon)
best_lamdba_outcome <- select_best(outcome_res, metric = "rmse")

# 🚀 Finaliserer og fitter outcome-workflow
final_outcome_fit <- finalize_workflow(wf_outcome, best_lamdba_outcome) |> 
    fit(data = df)

# 📊 Trekker ut variabler som ble valgt (ikke-null koeffisienter) i behandling
treat_vars <- tidy(final_treat_fit)  |>
    filter(term != "(Intercept)", estimate != 0) |> 
    pull(term)

# 📊 Trekker ut variabler som ble valgt i outcome-modellen
outcome_vars <- tidy(final_outcome_fit) |> 
    filter(term != "(Intercept)", estimate != 0) |> 
    pull(term)

# 🔗 Tar unionen av variabler fra begge modeller (double selection)
select_vars <- union(treat_vars, outcome_vars)

# 📋 Lager datasett med bare D, Y og utvalgte kovariater
df_selected <- df |> select(all_of(c("D", "Y", select_vars)))


# 1) Estimer PS-funksjonen
ps_recipe <- recipe( D  ~ ., data = df_selected) |> 
    step_rm(Y) |> 
    step_normalize( all_predictors())

# PS model
ps_model <- logistic_reg() |> set_engine("glm")

# Workflow for PS
ps_wf <- workflow() |> 
    add_recipe( ps_recipe) |> 
    add_model( ps_model)

# Estimer model
ps_fit <- fit(ps_wf, data = df_selected)

# Trekk ut PS
ps_score <- predict( ps_fit, df_selected, type = "prob") 


df_matched <- df_selected |> 
    mutate(
        ps = ps_score |> pull(.pred_1)
    )


# Vurder treffbarhet
df_matched |>
    select( ps, D) |> 
    mutate(
        ps_D = ifelse( ps >0.5, 1, 0)
    )

# Vurdere balansen





