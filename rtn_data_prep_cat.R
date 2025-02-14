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
sql <- "Select * from `himsdata-dev.dev_cdowsett_adhoc.rtn_base_cat_12feb25`"
tb <- bq_project_query(billing, sql)
dataCat <- bq_table_download(tb, n_max = 2000000)
dataCat1 <- dataCat

# Center numerical predictors and create polynomial terms for non-linear relationships
dataCat1$avg_age_centered <- scale(dataCat1$avg_age, center = TRUE, scale = FALSE)
dataCat1$avg_income_centered <- scale(dataCat1$avg_income, center = TRUE, scale = FALSE)
dataCat1$avg_txn_centered <- scale(dataCat1$avg_txn, center = TRUE, scale = FALSE)
dataCat1$avg_submit_response_centered <- scale(dataCat1$avg_submit_response, center = TRUE, scale = FALSE)
dataCat1$cnt_subs_centered <- scale(dataCat1$cnt_subs, center = TRUE, scale = FALSE)
dataCat1$cohort_age_centered <- scale(dataCat1$cohort_age, center = TRUE, scale = FALSE)
dataCat1$cohort_weight_centered <- scale(dataCat1$cohort_weight, center = TRUE, scale = FALSE)

# Create polynomial terms for selected numerical predictors
dataCat1$avg_age_squared <- dataCat1$avg_age_centered^2
dataCat1$avg_income_squared <- dataCat1$avg_income_centered^2
dataCat1$avg_txn_squared <- dataCat1$avg_txn_centered^2
dataCat1$cnt_subs_squared <- dataCat1$cnt_subs_centered^2

