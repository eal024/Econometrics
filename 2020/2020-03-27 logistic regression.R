


# Binary prediction -------------------------------------------------------

library(tidyverse)

donors <- read_csv("https://assets.datacamp.com/production/repositories/718/datasets/9055dac929e4515286728a2a5dae9f25f0e4eff6/donors.csv")

head(donors)

# Explore data descriptive - y var

table(donors$donated)

mean(donors$donated)

donation_model <- glm(donated ~ bad_address + interest_religion + interest_veterans, 
                      data = donors, family = "binomial")


donors$donation_prob <- predict(donation_model, type = "response")

donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donation_pred == donors$donated)

# ROC: 
library(pROC)
ROC <- roc(donors$donated, donors$donation_prob)
plot(ROC, col = "blue")

# Calculate the area under the curve (AUC)
auc(ROC)

# Correct! When AUC values are very close, it's important to know more about how the model will be used.


# Factors -----------------------------------------------------------------

# Convert the wealth rating to a factor
donors$wealth_rating <- factor(donors$wealth_rating, levels = c(0,1,2,3), labels = c("Unknown", "Low", "Medium", "High") )
# Use relevel() to change reference category
donors$wealth_rating <- relevel(as.factor(donors$wealth_rating), ref = "Medium")

# See how our factor coding impacts the model
summary(glm(donated ~ wealth_rating, data = donors , family = "binomial") )



# Handling NA -> replace with na ------------------------------------------

# Find the average age among non-missing values
summary(donors$age)

# Impute missing age values with the mean age
donors$imputed_age <- ifelse(is.na(donors$age), round( mean(donors$age, na.rm = T),digits = 2), donors$age)

# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age), 1,0)

# Build a recency, frequency, and money (RFM) model
rfm_model <- glm( donated ~money + recency*frequency, family = "binomial" ,data = donors)

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model, type = "response")

# Plot the ROC curve and find AUC for the new model
library(pROC)
ROC <- roc(donors$donated, rfm_prob)
plot(ROC, col = "red")
auc(ROC)



# Stepwise regression -----------------------------------------------------

# Removing predicters
# Backward
# Forward

# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Estimate the stepwise donation probability
step_prob <- predict(step_model)

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(donors$donated, step_prob)
plot(ROC, col = "red")
auc(ROC)




