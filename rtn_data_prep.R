# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)   # For Random Forest
library(car)  # For vif() function
library(pROC)  # For ROC curve
library(bigrquery)
library(ggplot2)
library(mgcv)

# Authenticate with Google BigQuery
bq_auth()  # This should prompt a browser window for authentication.

# Define BigQuery credentials and query
billing <- "himsdata-dev"
sql <- "Select * from `himsdata-dev.dev_cdowsett_adhoc.rtn_base_prod_12feb25`"
tb <- bq_project_query(billing, sql)
data <- bq_table_download(tb, n_max = 2000000)
dataProd <- data

# Center numerical predictors and create polynomial terms for non-linear relationships
dataProd$avg_age_centered <- scale(dataProd$avg_age, center = TRUE, scale = FALSE)
dataProd$avg_income_centered <- scale(dataProd$avg_income, center = TRUE, scale = FALSE)
dataProd$avg_txn_centered <- scale(dataProd$avg_txn, center = TRUE, scale = FALSE)
dataProd$avg_submit_response_centered <- scale(dataProd$avg_submit_response, center = TRUE, scale = FALSE)
dataProd$cnt_subs_centered <- scale(dataProd$cnt_subs, center = TRUE, scale = FALSE)
#dataProd$cohort_age_centered <- scale(dataProd$cohort_age, center = TRUE, scale = FALSE)
dataProd$cohort_weight_centered <- scale(dataProd$cohort_weight, center = TRUE, scale = FALSE)

# Create polynomial terms for selected numerical predictors
dataProd$avg_age_squared <- dataProd$avg_age_centered^2
dataProd$avg_income_squared <- dataProd$avg_income_centered^2
dataProd$avg_txn_squared <- dataProd$avg_txn_centered^2
dataProd$cnt_subs_squared <- dataProd$cnt_subs_centered^2

dataProd1 <- dataProd

