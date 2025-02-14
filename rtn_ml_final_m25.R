## !! IMPORTANT !! ------- USE THE DATA PREPARATION SCRIPT FIRST -------------   

# Remove rows with NA in rtn_25 and filter it
dataProd_rtn_25 <- filter(dataProd1, !is.na(rtn_25) & rtn_25 >= 0 & rtn_25 <= 1 & cnt_subs > 20)

# List of all categorical variables
categorical_vars <- c("site", "product", "supply", "epi", "prop", "prod_grp")  # Include prod_grp if applicable

# Convert categorical variables to factors in the full dataset before splitting
dataProd_rtn_25[categorical_vars] <- lapply(dataProd_rtn_25[categorical_vars], as.factor)

# Ensure that factor levels are consistent across both training and test datasets
for (var in categorical_vars) {
  levels(dataProd_rtn_25[[var]]) <- union(levels(dataProd_rtn_25[[var]]), levels(dataProd_rtn_25[[var]]))  # Ensure the levels are consistent
}

# Group infrequent levels in 'product' as 'Other'
threshold <- 5  # For example, treat categories with fewer than 5 occurrences as "Other"
product_levels <- table(dataProd_rtn_25$product)
infrequent_levels <- names(product_levels[product_levels < threshold])
dataProd_rtn_25$product <- as.factor(ifelse(dataProd_rtn_25$product %in% infrequent_levels, "Other", as.character(dataProd_rtn_25$product)))

# Check the number of levels for each categorical variable
for (var in categorical_vars) {
  cat(paste(var, ": ", length(levels(dataProd_rtn_25[[var]])), " levels\n"))
}

# Split the data into training and test sets (80% train, 20% test)
set.seed(42)  # Set seed for reproducibility
trainIndex <- createDataPartition(dataProd_rtn_25$rtn_25, p = 0.8, list = FALSE)
trainData_prod_rtn_25 <- dataProd_rtn_25[trainIndex,]
testData_prod_rtn_25 <- dataProd_rtn_25[-trainIndex,]

# Ensure 'prod_grp' and other factor levels match between training and test data
for (var in categorical_vars) {
  # This ensures the levels in the test data match the training data
  testData_prod_rtn_25[[var]] <- factor(testData_prod_rtn_25[[var]], levels = levels(trainData_prod_rtn_25[[var]]))
}

# Verify the factor levels to make sure there are no NA values due to unmatched levels
summary(trainData_prod_rtn_25[categorical_vars])
summary(testData_prod_rtn_25[categorical_vars])

# Ensure that the columns in testData_prod_rtn_25 match the columns in trainData_prod_rtn_25
testData_prod_rtn_25 <- testData_prod_rtn_25[, names(trainData_prod_rtn_25)]


# Random Forest Model (Predicting rtn_25)
rf_prodmodel_rtn_25 <- randomForest(rtn_25 ~ 
                                      d7_canc +
                                      d7_snz +
                                      rtn_4 +
                                      slope_rtn_4 +
                                      rtn_7 +
                                      slope_rtn_7 +
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
                                      product + 
                                      supply + 
                                      epi + 
                                      prop + 
                                      cat + 
                                      site,
                                    data = trainData_prod_rtn_25, ntree = 100, na.action = na.exclude)

# Predictions for Random Forest Model (rtn_25)
rf_prodpreds_rtn_25 <- predict(rf_prodmodel_rtn_25, newdata = testData_prod_rtn_25)

# Calculate the R² equivalent for Random Forest Model (rtn_25)
sst_rtn_25 <- sum((na.omit(testData_prod_rtn_25$rtn_25) - mean(na.omit(testData_prod_rtn_25$rtn_25)))^2)

# Create a logical index where both rtn_25 and rf_prodpreds_rtn_25 are not NA
complete_idx <- complete.cases(testData_prod_rtn_25$rtn_25, rf_prodpreds_rtn_25)

# Use the index to subset both vectors
observed2 <- testData_prod_rtn_25$rtn_25[complete_idx]
predicted2 <- rf_prodpreds_rtn_25[complete_idx]

# Now compute SSE using the aligned vectors
sse_rtn_25 <- sum((observed2 - predicted2)^2)

r_squared_rf_rtn_25 <- ifelse(var(na.omit(testData_prod_rtn_25$rtn_25)) == 0 | var(na.omit(rf_prodpreds_rtn_25)) == 0, 
                              NA, 
                              1 - (sse_rtn_25 / sst_rtn_25))

# Print R² for Random Forest Model (rtn_25)
cat("R² for Random Forest Model (rtn_25): ", r_squared_rf_rtn_25, "\n")

# Extract feature importance for Random Forest Model (rtn_25)
rf_importance_rtn_25 <- importance(rf_prodmodel_rtn_25)
rf_importance_rtn_25 <- data.frame(Variable = rownames(rf_importance_rtn_25),  
                                   Importance = rf_importance_rtn_25[, 1],  
                                   Model = "Random Forest Model")  
rf_importance_rtn_25$Date <- Sys.Date()

# Create a formatted date string (e.g., "7feb2025")
formatted_date <- format(Sys.Date(), "%d%b%Y")

# Define the table name with the date appended
table_name <- paste0("feature_importance_hims_prod_", formatted_date)

# Save the feature importance table
desktop_path <- file.path(Sys.getenv("HOME"), "Desktop")
file_path <- file.path(desktop_path, paste0(table_name, ".csv"))
write.csv(rf_importance_rtn_25, file_path, row.names = FALSE)

# Assign the table to a variable with the formatted name
assign(table_name, rf_importance_rtn_25)

# Model Evaluation (RMSE)
# Remove NA values from both actual and predicted values before calculating RMSE
complete_idx2 <- complete.cases(testData_prod_rtn_25$rtn_25, rf_prodpreds_rtn_25)

# Subset the observed and predicted values using the same index
obs2 <- testData_prod_rtn_25$rtn_25[complete_idx2]
pred2 <- rf_prodpreds_rtn_25[complete_idx2]

# Calculate RMSE for the second case
rmse_rf_rtn_25 <- sqrt(mean((obs2 - pred2)^2))

# Print RMSE result
cat("Random Forest Model RMSE (rtn_25): ", rmse_rf_rtn_25, "\n")

# Join the predictions back to the test data
testData_prod_rtn_25 <- testData_prod_rtn_25 %>% 
  mutate(predicted_rtn_25_rf_model = rf_prodpreds_rtn_25)

# Calculate the percent difference and actual difference for the model
testData_prod_rtn_25 <- testData_prod_rtn_25 %>% 
  mutate(
    percent_diff_rf_model = (predicted_rtn_25_rf_model - rtn_25) / rtn_25,   
    actual_diff_rf_model = predicted_rtn_25_rf_model - rtn_25
  )

# Create plots for Actual vs Predicted for Random Forest model
testData_prod_rtn_25_clean <- testData_prod_rtn_25 %>% 
  filter(!is.na(rtn_25) & !is.na(predicted_rtn_25_rf_model))

ggplot(testData_prod_rtn_25_clean, aes(x = rtn_25, y = predicted_rtn_25_rf_model)) + 
  geom_point(color = "green") + 
  geom_abline(slope = 1, intercept = 0, color = "red") + 
  labs(title = "Act vs Pred: RF Prod Model v1.23 (rtn_25)",
       x = "Actual Retention",
       y = "Predicted Retention") + 
  theme_minimal() + 
  annotate("text", x = 0.8, y = 0.2, label = paste("RMSE: ", round(rmse_rf_rtn_25, 4)), size = 5, color = "blue") + 
  annotate("text", x = 0.8, y = 0.15, label = paste("R²: ", round(r_squared_rf_rtn_25, 4)), size = 5, color = "blue")

## JOINING THE PREDICTORS TO THE ORIGINAL DATA  

# List of all categorical variables
categorical_vars <- c("site", "product", "supply", "epi", "prop", "prod_grp")  # Add prod_grp if not already included

# Convert categorical variables to factors in both data frames
dataProd1[categorical_vars] <- lapply(dataProd1[categorical_vars], as.factor)
testData_prod_rtn_25[categorical_vars] <- lapply(testData_prod_rtn_25[categorical_vars], as.factor)

# Ensure that factor levels are consistent across both datasets
for (var in categorical_vars) {
  testData_prod_rtn_25[[var]] <- factor(testData_prod_rtn_25[[var]], levels = levels(dataProd1[[var]]))
}

# Perform the left join with correct column names
dataProd1rf <- left_join(dataProd1, testData_prod_rtn_25 %>% 
                           select(create_mth, site, prod_grp, product, supply, epi, prop, predicted_rtn_25_rf_model),
                         by = c("create_mth", "site", "prod_grp", "product", "supply", "epi", "prop"))

# View the final dataset with predictions
head(dataProd1rf)
