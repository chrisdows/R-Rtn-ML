## Bring in current mth counts, join predictions, join hist actuals, join cat actuals

# Define BigQuery credentials and query
billing <- "himsdata-dev"
sql <- "Select * from `himsdata-dev.dev_cdowsett_adhoc.rtn_cnts_12feb25`"
tb <- bq_project_query(billing, sql)
data_rtn_final_11Feb25_cat <- bq_table_download(tb, n_max = 2000000)

#create final cat table 
data_rtn_final_11Feb25_cat <- data_rtn_final_11Feb25_cat %>%
  filter(cat_cnt_subs > 10, create_mth == "2025-01", !is.na(cat_grp)) %>%  # Remove rows where cat_grp is NA
  select(create_mth, cat_grp, site, cat, supply, epi, prop, cat, site, cat_cnt_subs) %>%
  distinct()

## Join in data from the cat tables prior to adding a prediction
colnames(dataCat1)

data_rtn_final_11Feb25_cat <- data_rtn_final_11Feb25_cat %>%
  left_join(dataCat1 %>%
              select(create_mth
                     , cat_grp
                     , avg_age
                     , d7_canc
                     , d7_snz
                     , avg_submit_sign
                     , avg_txn
                     , avg_income
                     , avg_submit_response
              ), 
            by = c("cat_grp","create_mth"))

# join in historical averages from rtn_mostrct_final_cat.r <--- run the mostrct file first


data_rtn_final_11Feb25_cat <- data_rtn_final_11Feb25_cat %>%
  left_join(final_summary_cat %>%
              select(cat_grp
                     , min_cnt_subs
                     , max_cnt_subs
                     , max_weighted_avg_rtn_4
                     , max_weighted_avg_rtn_7
                     , max_weighted_avg_rtn_13
                     , max_weighted_avg_rtn_19
                     , max_weighted_avg_rtn_25
                     , rtn_7_flg 
                     , rtn_13_flg 
                     , rtn_19_flg 
                     , rtn_25_flg 
                     ), 
            by = c("cat_grp"))

data_rtn_final_11Feb25_cat <- data_rtn_final_11Feb25_cat %>%
  mutate(cohort_weight = 1)

data_rtn_final_11Feb25_cat <- data_rtn_final_11Feb25_cat %>%
  mutate(
      rtn_4 = max_weighted_avg_rtn_4,
      rtn_7 = max_weighted_avg_rtn_7,
      rtn_13 = max_weighted_avg_rtn_13,
      rtn_19 = max_weighted_avg_rtn_19,
      rtn_25 = max_weighted_avg_rtn_25
      )

data_rtn_final_11Feb25_cat <- data_rtn_final_11Feb25_cat %>%
  mutate(
    slope_rtn_4 = if_else(rtn_4 == 0, NA_real_, rtn_4 / 1),
    slope_rtn_7 = if_else(rtn_7 == 0, NA_real_, rtn_7 / rtn_4),
    slope_rtn_13 = if_else(rtn_13 == 0, NA_real_, rtn_13 / rtn_7),
    slope_rtn_19 = if_else(rtn_19 == 0, NA_real_, rtn_19 / rtn_13),
    slope_rtn_25 = if_else(rtn_25 == 0, NA_real_, rtn_25 / rtn_19)
    )


# Center numerical predictors and create polynomial terms for non-linear relationships
data_rtn_final_11Feb25_cat$avg_age_centered <- scale(data_rtn_final_11Feb25_cat$avg_age, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_cat$avg_income_centered <- scale(data_rtn_final_11Feb25_cat$avg_income, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_cat$avg_txn_centered <- scale(data_rtn_final_11Feb25_cat$avg_txn, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_cat$avg_submit_response_centered <- scale(data_rtn_final_11Feb25_cat$avg_submit_response, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_cat$cnt_subs_centered <- scale(data_rtn_final_11Feb25_cat$cat_cnt_subs, center = TRUE, scale = FALSE)
#data_rtn_final_11Feb25_cat$cohort_age_centered <- scale(data_rtn_final_11Feb25_cat$cohort_age, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_cat$cohort_weight_centered <- scale(data_rtn_final_11Feb25_cat$cohort_weight, center = TRUE, scale = FALSE)

# Create polynomial terms for selected numerical predictors
data_rtn_final_11Feb25_cat$avg_age_squared <- data_rtn_final_11Feb25_cat$avg_age_centered^2
data_rtn_final_11Feb25_cat$avg_income_squared <- data_rtn_final_11Feb25_cat$avg_income_centered^2
data_rtn_final_11Feb25_cat$avg_txn_squared <- data_rtn_final_11Feb25_cat$avg_txn_centered^2
data_rtn_final_11Feb25_cat$cnt_subs_squared <- data_rtn_final_11Feb25_cat$cnt_subs_centered^2


## Applying the current model to this dataset (should have run RF already)

# Make predictions for Random Forest Model RTN4 (rtn_4)
predictions_model_cat_rtn4 <- predict(rf_catmodel_rtn_4, newdata = data_rtn_final_11Feb25_cat)
predictions_model_cat_rtn7 <- predict(rf_catmodel_rtn_7, newdata = data_rtn_final_11Feb25_cat)
predictions_model_cat_rtn13 <- predict(rf_catmodel_rtn_13, newdata = data_rtn_final_11Feb25_cat)
predictions_model_cat_rtn19 <- predict(rf_catmodel_rtn_19, newdata = data_rtn_final_11Feb25_cat)
predictions_model_cat_rtn25 <- predict(rf_catmodel_rtn_25, newdata = data_rtn_final_11Feb25_cat)

# Add the predictions to the new dataset
data_rtn_final_11Feb25_cat <- data_rtn_final_11Feb25_cat %>%
  mutate(
    predictions_model_cat_rtn4 = predictions_model_cat_rtn4,
    predictions_model_cat_rtn7 = predictions_model_cat_rtn7,
    predictions_model_cat_rtn13 = predictions_model_cat_rtn13,
    predictions_model_cat_rtn19 = predictions_model_cat_rtn19,
    predictions_model_cat_rtn25 = predictions_model_cat_rtn25,
    rf_pred_date = Sys.Date()
         )

# View the updated dataset with predictions
head(data_rtn_final_11Feb25_cat)

#calculate differences 

data_rtn_final_11Feb25_cat <- data_rtn_final_11Feb25_cat %>%
  mutate(
    diff_rtn_4 = abs(predictions_model_cat_rtn4 - rtn_4),
    diff_rtn_7 = abs(predictions_model_cat_rtn7 - rtn_7),
    diff_rtn_13 = abs(predictions_model_cat_rtn13 - rtn_13),
    diff_rtn_19 = abs(predictions_model_cat_rtn19 - rtn_19),
    diff_rtn_25 = abs(predictions_model_cat_rtn25 - rtn_25)
  )

# FINAL SELECTION OF ACTUALS OR MODEL

data_rtn_final_11Feb25_cat <- data_rtn_final_11Feb25_cat %>%
  mutate(
    final_rtn_4  = if_else(diff_rtn_4  < 0.02, predictions_model_cat_rtn4,  rtn_4),
    final_rtn_7  = if_else(diff_rtn_7  < 0.02, predictions_model_cat_rtn7,  rtn_7),
    final_rtn_13 = if_else(diff_rtn_13 < 0.02, predictions_model_cat_rtn13, rtn_13),
    final_rtn_19 = if_else(diff_rtn_19 < 0.02, predictions_model_cat_rtn19, rtn_19),
    final_rtn_25 = if_else(diff_rtn_25 < 0.02, predictions_model_cat_rtn25, rtn_25)
  )


write.csv(data_rtn_final_11Feb25_cat, file = "/Users/cdowsett/Library/CloudStorage/GoogleDrive-cdowsett@forhims.com/My Drive/data_rtn_final_11Feb25_prod.csv", row.names = FALSE)

