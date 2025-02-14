## !! IMPORTANT !! ------- USE THE DATA PREPARATION SCRIPT FIRST ------------- 

# Remove rows with NA in rtn_25 and filter it
dataCat_rtn_25 <- filter(dataCat1, !is.na(rtn_25) & rtn_25 >= 0 & rtn_25 <= 1)

# Split the data into training and test sets (80% train, 20% test)
set.seed(42)  # Set seed for reproducibility
trainIndex <- createDataPartition(dataCat_rtn_25$rtn_25, p = 0.8, list = FALSE)
trainData_cat_rtn_25 <- dataCat_rtn_25[trainIndex,]
testData_cat_rtn_25 <- dataCat_rtn_25[-trainIndex,]

# Ensure 'prod_grp' factor levels match between training and test data
testData_cat_rtn_25$cat_grp <- factor(testData_cat_rtn_25$cat_grp, levels = levels(trainData_cat_rtn_25$cat_grp))

str(trainData_cat_rtn_25)
str(testData_cat_rtn_25)

# Random Forest Model (Predicting rtn_25)
rf_catmodel_rtn_25 <- randomForest(rtn_25 ~ 
                                     d7_canc +
                                     d7_snz +
                                     rtn_4 + 
                                     slope_rtn_4 + 
                                     rtn_7 + 
                                     slope_rtn_7+
                                     rtn_13 + 
                                     slope_rtn_13 + 
                                     rtn_19 + 
                                     slope_rtn_19 + 
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
                                   data = trainData_cat_rtn_25, ntree = 100, na.action = na.exclude)

# Predictions for Random Forest Model (rtn_25)
rf_catpreds_rtn_25 <- predict(rf_catmodel_rtn_25, newdata = testData_cat_rtn_25)

# Calculate the R² equivalent for Random Forest Model (rtn_25)
sst_rtn_25 <- sum((na.omit(testData_cat_rtn_25$rtn_25) - mean(na.omit(testData_cat_rtn_25$rtn_25)))^2)

# Create a logical index where both rtn_25 and rf_catpreds_rtn_25 are not NA
complete_idx <- complete.cases(testData_cat_rtn_25$rtn_25, rf_catpreds_rtn_25)

# Use the index to subset both vectors
observed2 <- testData_cat_rtn_25$rtn_25[complete_idx]
predicted2 <- rf_catpreds_rtn_25[complete_idx]

# Now compute SSE using the aligned vectors
sse_rtn_25 <- sum((observed2 - predicted2)^2)

r_squared_rf_rtn_25 <- ifelse(var(na.omit(testData_cat_rtn_25$rtn_25)) == 0 | var(na.omit(rf_catpreds_rtn_25)) == 0, 
                              NA, 
                              1 - (sse_rtn_25 / sst_rtn_25))

# Print R² for Random Forest Model (rtn_25)
cat("R² for Random Forest Model (rtn_25): ", r_squared_rf_rtn_25, "\n")

# Extract feature importance for Random Forest Model (rtn_25)
rf_importance_rtn_25 <- importance(rf_catmodel_rtn_25)
rf_importance_rtn_25 <- data.frame(Variable = rownames(rf_importance_rtn_25),  
                                   Importance = rf_importance_rtn_25[, 1],  
                                   Model = "Random Forest Model")  
rf_importance_rtn_25$Date <- Sys.Date()

# Create a formatted date string (e.g., "7feb2025")
formatted_date <- format(Sys.Date(), "%d%b%Y")

# Define the table name with the date appended
table_name <- paste0("feature_importance_hims_cat_", formatted_date)

# Save the feature importance table
desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
file_path <- file.path(desktop_path, paste0(table_name, ".csv"))
write.csv(rf_importance_rtn_25, file_path, row.names = FALSE)

# Assign the table to a variable with the formatted name
assign(table_name, rf_importance_rtn_25)

# Model Evaluation (RMSE)
# Remove NA values from both actual and predicted values before calculating RMSE
complete_idx2 <- complete.cases(testData_cat_rtn_25$rtn_25, rf_catpreds_rtn_25)

# Subset the observed and predicted values using the same index
obs2 <- testData_cat_rtn_25$rtn_25[complete_idx2]
pred2 <- rf_catpreds_rtn_25[complete_idx2]

# Calculate RMSE for the second case
rmse_rf_rtn_25 <- sqrt(mean((obs2 - pred2)^2))

# Print RMSE result
cat("Random Forest Model RMSE (rtn_25): ", rmse_rf_rtn_25, "\n")

# Join the predictions back to the test data
testData_cat_rtn_25 <- testData_cat_rtn_25 %>%
  mutate(predicted_rtn_25_rf_model = rf_catpreds_rtn_25)

# Calculate the percent difference and actual difference for the model
testData_cat_rtn_25 <- testData_cat_rtn_25 %>%
  mutate(
    percent_diff_rf_model = (predicted_rtn_25_rf_model - rtn_25) / rtn_25,   
    actual_diff_rf_model = predicted_rtn_25_rf_model - rtn_25
  )

# Create plots for Actual vs Predicted for Random Forest model

# Remove rows with NA in 'rtn_25' or 'predicted_rtn_25_rf_model'
testData_cat_rtn_25_clean <- testData_cat_rtn_25 %>%
  filter(!is.na(rtn_25) & !is.na(predicted_rtn_25_rf_model))

# Plot for Random Forest Model with RMSE and R² annotations
ggplot(testData_cat_rtn_25_clean, aes(x = rtn_25, y = predicted_rtn_25_rf_model)) +
  geom_point(color = "green") +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Act vs Pred: RF Cat Model v1.23 (rtn_25)",
       x = "Actual Retention",
       y = "Predicted Retention") +
  theme_minimal() + 
  # Annotate RMSE and R² on the plot
  annotate("text", x = 0.8, y = 0.2, label = paste("RMSE: ", round(rmse_rf_rtn_25, 4)), size = 5, color = "blue") +
  annotate("text", x = 0.8, y = 0.15, label = paste("R²: ", round(r_squared_rf_rtn_25, 4)), size = 5, color = "blue")


## JOINING THE PREDICTORS TO THE ORIGINAL DATA 

dataCat1rf <- left_join(dataCat1, testData_cat_rtn_25 %>%
                          select(create_mth, site, cat, supply, epi, prop, predicted_rtn_25_rf_model), 
                        by = c("create_mth", "site", "cat", "supply", "epi", "prop"))

# View the final dataset with predictions
head(dataCat1rf)
