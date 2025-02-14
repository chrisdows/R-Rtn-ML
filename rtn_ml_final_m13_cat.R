## !! IMPORTANT !! ------- USE THE DATA PREPARATION SCRIPT FIRST ------------- 

# Remove rows with NA in rtn_13 and filter it
dataCat_rtn_13 <- filter(dataCat1, !is.na(rtn_13) & rtn_13 >= 0 & rtn_13 <= 1)

# Split the data into training and test sets (80% train, 20% test)
set.seed(42)  # Set seed for reproducibility
trainIndex <- createDataPartition(dataCat_rtn_13$rtn_13, p = 0.8, list = FALSE)
trainData_cat_rtn_13 <- dataCat_rtn_13[trainIndex,]
testData_cat_rtn_13 <- dataCat_rtn_13[-trainIndex,]

# Ensure 'prod_grp' factor levels match between training and test data
testData_cat_rtn_13$cat_grp <- factor(testData_cat_rtn_13$cat_grp, levels = levels(trainData_cat_rtn_13$cat_grp))

str(trainData_cat_rtn_13)
str(testData_cat_rtn_13)

# Random Forest Model (Predicting rtn_13)
rf_catmodel_rtn_13 <- randomForest(rtn_13 ~ 
                                     d7_canc +
                                     d7_snz +
                                     rtn_4 +
                                     slope_rtn_4 + 
                                     rtn_7 +
                                     slope_rtn_7 + 
                                     avg_age_centered +
                                     avg_age_squared +
                                     avg_income_centered +
                                     avg_income_squared +
                                     avg_txn_centered +
                                     avg_txn_squared +
                                     avg_submit_response_centered +
                                     cnt_subs_centered +
                                     cohort_weight_centered +
                                     #cohort_age_centered +
                                     cat +
                                     supply +
                                     epi +
                                     prop +
                                     site,
                                   data = trainData_cat_rtn_13, ntree = 100, na.action = na.exclude)

# Predictions for Random Forest Model (rtn_13)
rf_catpreds_rtn_13 <- predict(rf_catmodel_rtn_13, newdata = testData_cat_rtn_13)

# Calculate the R² equivalent for Random Forest Model (rtn_13)
sst_rtn_13 <- sum((na.omit(testData_cat_rtn_13$rtn_13) - mean(na.omit(testData_cat_rtn_13$rtn_13)))^2)

# Create a logical index where both rtn_13 and rf_catpreds_rtn_13 are not NA
complete_idx <- complete.cases(testData_cat_rtn_13$rtn_13, rf_catpreds_rtn_13)

# Use the index to subset both vectors
observed2 <- testData_cat_rtn_13$rtn_13[complete_idx]
predicted2 <- rf_catpreds_rtn_13[complete_idx]

# Now compute SSE using the aligned vectors
sse_rtn_13 <- sum((observed2 - predicted2)^2)

r_squared_rf_rtn_13 <- ifelse(var(na.omit(testData_cat_rtn_13$rtn_13)) == 0 | var(na.omit(rf_catpreds_rtn_13)) == 0, 
                              NA, 
                              1 - (sse_rtn_13 / sst_rtn_13))

# Print R² for Random Forest Model (rtn_13)
cat("R² for Random Forest Model (rtn_13): ", r_squared_rf_rtn_13, "\n")

# Extract feature importance for Random Forest Model (rtn_13)
rf_importance_rtn_13 <- importance(rf_catmodel_rtn_13)
rf_importance_rtn_13 <- data.frame(Variable = rownames(rf_importance_rtn_13),  
                                   Importance = rf_importance_rtn_13[, 1],  
                                   Model = "Random Forest Model")  
rf_importance_rtn_13$Date <- Sys.Date()

# Create a formatted date string (e.g., "7feb2025")
formatted_date <- format(Sys.Date(), "%d%b%Y")

# Define the table name with the date appended
table_name <- paste0("feature_importance_hims_cat_", formatted_date)

# Save the feature importance table
desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
file_path <- file.path(desktop_path, paste0(table_name, ".csv"))
write.csv(rf_importance_rtn_13, file_path, row.names = FALSE)

# Assign the table to a variable with the formatted name
assign(table_name, rf_importance_rtn_13)

# Model Evaluation (RMSE)
# Remove NA values from both actual and predicted values before calculating RMSE
complete_idx2 <- complete.cases(testData_cat_rtn_13$rtn_13, rf_catpreds_rtn_13)

# Subset the observed and predicted values using the same index
obs2 <- testData_cat_rtn_13$rtn_13[complete_idx2]
pred2 <- rf_catpreds_rtn_13[complete_idx2]

# Calculate RMSE for the second case
rmse_rf_rtn_13 <- sqrt(mean((obs2 - pred2)^2))

# Print RMSE result
cat("Random Forest Model RMSE (rtn_13): ", rmse_rf_rtn_13, "\n")

# Join the predictions back to the test data
testData_cat_rtn_13 <- testData_cat_rtn_13 %>%
  mutate(predicted_rtn_13_rf_model = rf_catpreds_rtn_13)

# Calculate the percent difference and actual difference for the model
testData_cat_rtn_13 <- testData_cat_rtn_13 %>%
  mutate(
    percent_diff_rf_model = (predicted_rtn_13_rf_model - rtn_13) / rtn_13,   
    actual_diff_rf_model = predicted_rtn_13_rf_model - rtn_13
  )

# Create plots for Actual vs Predicted for Random Forest model

# Remove rows with NA in 'rtn_13' or 'predicted_rtn_13_rf_model'
testData_cat_rtn_13_clean <- testData_cat_rtn_13 %>%
  filter(!is.na(rtn_13) & !is.na(predicted_rtn_13_rf_model))

# Plot for Random Forest Model with RMSE and R² annotations
ggplot(testData_cat_rtn_13_clean, aes(x = rtn_13, y = predicted_rtn_13_rf_model)) +
  geom_point(color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Act vs Pred: RF Cat Model v1.23 (rtn_13)",
       x = "Actual Retention",
       y = "Predicted Retention") +
  theme_minimal() + 
  # Annotate RMSE and R² on the plot
  annotate("text", x = 0.8, y = 0.2, label = paste("RMSE: ", round(rmse_rf_rtn_13, 4)), size = 5, color = "blue") +
  annotate("text", x = 0.8, y = 0.15, label = paste("R²: ", round(r_squared_rf_rtn_13, 4)), size = 5, color = "blue")


## JOINING THE PREDICTORS TO THE ORIGINAL DATA 

dataCat1rf <- left_join(dataCat1, testData_cat_rtn_13 %>%
                          select(create_mth, site, cat, supply, epi, prop, predicted_rtn_13_rf_model), 
                        by = c("create_mth", "site", "cat", "supply", "epi", "prop"))

# View the final dataset with predictions
head(dataCat1rf)
