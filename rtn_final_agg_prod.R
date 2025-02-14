## Bring in current mth counts, join predictions, join hist actuals, join prod actuals

# Define BigQuery credentials and query
billing <- "himsdata-dev"
sql <- "Select * from `himsdata-dev.dev_cdowsett_adhoc.rtn_cnts_12feb25`"
tb <- bq_project_query(billing, sql)
data_rtn_final_11Feb25_prod <- bq_table_download(tb, n_max = 2000000)

#create final prod table 
data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  filter(cnt_subs > 10, create_mth == "2025-01", !is.na(prod_grp)) %>%  # Remove rows where prod_grp is NA
  select(create_mth, site, cat, product, supply, epi, prop, cat_grp, prod_grp, cnt_subs) %>%
  distinct()

## Join in data from the prod tables prior to adding a prediction
colnames(dataProd1)
colnames(trainData_prod_rtn_4)

data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  left_join(dataProd1rf %>%
              select(create_mth
                     , prod_grp
                     , avg_age
                     , d7_canc
                     , d7_snz
                     , avg_submit_sign
                     , avg_txn
                     , avg_income
                     , avg_submit_response
              ), 
            by = c("prod_grp","create_mth"))

# join in historical averages from rtn_mostrct_final_prod.r <--- run the mostrct file first

data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  left_join(final_summary %>%
              select(prod_grp
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
            by = c("prod_grp"))

data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  mutate(cohort_weight = 1)

data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  mutate(
    rtn_4 = max_weighted_avg_rtn_4,
    rtn_7 = max_weighted_avg_rtn_7,
    rtn_13 = max_weighted_avg_rtn_13,
    rtn_19 = max_weighted_avg_rtn_19,
    rtn_25 = max_weighted_avg_rtn_25
  )

data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  mutate(
    slope_rtn_4 = if_else(rtn_4 == 0, NA_real_, rtn_4 / 1),
    slope_rtn_7 = if_else(rtn_7 == 0, NA_real_, rtn_7 / rtn_4),
    slope_rtn_13 = if_else(rtn_13 == 0, NA_real_, rtn_13 / rtn_7),
    slope_rtn_19 = if_else(rtn_19 == 0, NA_real_, rtn_19 / rtn_13),
    slope_rtn_25 = if_else(rtn_25 == 0, NA_real_, rtn_25 / rtn_19)
  )


# Center numerical predictors and create polynomial terms for non-linear relationships
data_rtn_final_11Feb25_prod$avg_age_centered <- scale(data_rtn_final_11Feb25_prod$avg_age, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_prod$avg_income_centered <- scale(data_rtn_final_11Feb25_prod$avg_income, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_prod$avg_txn_centered <- scale(data_rtn_final_11Feb25_prod$avg_txn, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_prod$avg_submit_response_centered <- scale(data_rtn_final_11Feb25_prod$avg_submit_response, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_prod$cnt_subs_centered <- scale(data_rtn_final_11Feb25_prod$cnt_subs, center = TRUE, scale = FALSE)
#data_rtn_final_11Feb25_prod$cohort_age_centered <- scale(data_rtn_final_11Feb25_prod$cohort_age, center = TRUE, scale = FALSE)
data_rtn_final_11Feb25_prod$cohort_weight_centered <- scale(data_rtn_final_11Feb25_prod$cohort_weight, center = TRUE, scale = FALSE)

# Create polynomial terms for selected numerical predictors
data_rtn_final_11Feb25_prod$avg_age_squared <- data_rtn_final_11Feb25_prod$avg_age_centered^2
data_rtn_final_11Feb25_prod$avg_income_squared <- data_rtn_final_11Feb25_prod$avg_income_centered^2
data_rtn_final_11Feb25_prod$avg_txn_squared <- data_rtn_final_11Feb25_prod$avg_txn_centered^2
data_rtn_final_11Feb25_prod$cnt_subs_squared <- data_rtn_final_11Feb25_prod$cnt_subs_centered^2


## Applying the current model to this dataset (should have run RF already)

## OPTIONAL -- FIXING FOR SOME OF THE LEVELS IN ONE RTN MODEL BUT NOT OTHERS

# Get classes (using only the primary class if multiple are returned)
train_classes <- sapply(trainData_prod_rtn_19, function(x) class(x)[1])
new_classes   <- sapply(data_rtn_final_11Feb25_prod, function(x) class(x)[1])

# Find the common variable names
common_vars <- intersect(names(train_classes), names(new_classes))

# Create the comparison data frame for the common variables
comparison <- data.frame(
  Variable = common_vars,
  TrainingDataType = train_classes[common_vars],
  NewDataType = new_classes[common_vars],
  row.names = NULL
)

print(comparison)

# For 'site' (training: factor, new: character)
data_rtn_final_11Feb25_prod$site <- factor(
  data_rtn_final_11Feb25_prod$site, 
  levels = levels(trainData_prod_rtn_25$site)
)

# For 'product' (training: factor, new: character)
data_rtn_final_11Feb25_prod$product <- factor(
  data_rtn_final_11Feb25_prod$product, 
  levels = levels(trainData_prod_rtn_25$product)
)

# For 'supply' (training: factor, new: numeric)
data_rtn_final_11Feb25_prod$supply <- factor(
  data_rtn_final_11Feb25_prod$supply, 
  levels = levels(trainData_prod_rtn_25$supply)
)

# For 'epi' (training: factor, new: logical)
data_rtn_final_11Feb25_prod$epi <- factor(
  data_rtn_final_11Feb25_prod$epi, 
  levels = levels(trainData_prod_rtn_25$epi)
)

# For 'prop' (training: factor, new: logical)
data_rtn_final_11Feb25_prod$prop <- factor(
  data_rtn_final_11Feb25_prod$prop, 
  levels = levels(trainData_prod_rtn_25$prop)
)

# For 'prod_grp' (training: factor, new: character)
data_rtn_final_11Feb25_prod$prod_grp <- factor(
  data_rtn_final_11Feb25_prod$prod_grp, 
  levels = levels(trainData_prod_rtn_25$prod_grp)
)


# Make predictions for Random Forest Model RTN4 (rtn_4)
predictions_model_prod_rtn4 <- predict(rf_prodmodel_rtn_4, newdata = data_rtn_final_11Feb25_prod)
predictions_model_prod_rtn7 <- predict(rf_prodmodel_rtn_7, newdata = data_rtn_final_11Feb25_prod)
predictions_model_prod_rtn13 <- predict(rf_prodmodel_rtn_13, newdata = data_rtn_final_11Feb25_prod)
predictions_model_prod_rtn19 <- predict(rf_prodmodel_rtn_19, newdata = data_rtn_final_11Feb25_prod)
predictions_model_prod_rtn25 <- predict(rf_prodmodel_rtn_25, newdata = data_rtn_final_11Feb25_prod)

# Add the predictions to the new dataset
data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  mutate(
    predictions_model_prod_rtn4 = predictions_model_prod_rtn4,
    predictions_model_prod_rtn7 = predictions_model_prod_rtn7,
    predictions_model_prod_rtn13 = predictions_model_prod_rtn13,
    predictions_model_prod_rtn19 = predictions_model_prod_rtn19,
    predictions_model_prod_rtn25 = predictions_model_prod_rtn25,
    rf_pred_date = Sys.Date()
  )

# View the updated dataset with predictions
head(data_rtn_final_11Feb25_prod)


#calculate differences 

data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  mutate(
    diff_rtn_4 = abs(predictions_model_prod_rtn4 - rtn_4),
    diff_rtn_7 = abs(predictions_model_prod_rtn7 - rtn_7),
    diff_rtn_13 = abs(predictions_model_prod_rtn13 - rtn_13),
    diff_rtn_19 = abs(predictions_model_prod_rtn19 - rtn_19),
    diff_rtn_25 = abs(predictions_model_prod_rtn25 - rtn_25)
  )

## ADD IN CAT_GRP RECENTS + MODEL

data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  left_join(
    data_rtn_final_11Feb25_cat %>%
      # Rename multiple columns here:
      rename(
        cat_avgwgt_rtn_4 = max_weighted_avg_rtn_4,
        cat_avgwgt_rtn_7 = max_weighted_avg_rtn_7,
        cat_avgwgt_rtn_13 = max_weighted_avg_rtn_13,
        cat_avgwgt_rtn_19 = max_weighted_avg_rtn_19,
        cat_avgwgt_rtn_25 = max_weighted_avg_rtn_25
      ) %>%
      # Select only the columns needed for the join and the new names
      select(create_mth, cat_grp, cat_avgwgt_rtn_4, cat_avgwgt_rtn_7, cat_avgwgt_rtn_13, cat_avgwgt_rtn_19, cat_avgwgt_rtn_25),
    by = c("cat_grp", "create_mth")
  )


# FINAL SELECTION OF ACTUALS OR MODEL <! ------ fix THISSSSSSSSSSS

data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  mutate(
    diff_rtn_4_cat = abs(predictions_model_prod_rtn4 - cat_avgwgt_rtn_4),
    diff_rtn_7_cat = abs(predictions_model_prod_rtn7 - cat_avgwgt_rtn_7),
    diff_rtn_13_cat = abs(predictions_model_prod_rtn13 - cat_avgwgt_rtn_13),
    diff_rtn_19_cat = abs(predictions_model_prod_rtn19 - cat_avgwgt_rtn_19),
    diff_rtn_25_cat = abs(predictions_model_prod_rtn25 - cat_avgwgt_rtn_25)
  )


data_rtn_final_11Feb25_prod <- data_rtn_final_11Feb25_prod %>%
  mutate(
    final_rtn_4 = case_when(
      # Rule 1 for rtn_4: Highest priority
      as.numeric(as.character(supply)) > 90 ~ 1,
      
      # Rule 2 for rtn_4: When weighted average is missing, revert to cat or model
      is.na(rtn_4) & diff_rtn_4_cat > 4 ~ cat_avgwgt_rtn_4,
      is.na(rtn_4) & diff_rtn_4_cat <= 4 ~ predictions_model_prod_rtn4,
      
      # Rule 3 for rtn_4: When weighted average is present, revert to actual or model
      !is.na(rtn_4) & diff_rtn_4 > 2 ~ rtn_4,
      !is.na(rtn_4) & diff_rtn_4 <= 2 ~ predictions_model_prod_rtn4,
      
      # Fallback/default
      TRUE ~ NA_real_
    ),
    final_rtn_7 = case_when(
      # Rule 1 for rtn_7: Highest priority for rtn_7
      as.numeric(as.character(supply)) > 180 ~ 1,
      
      # Rule 2 for rtn_7: When weighted average is missing
      is.na(rtn_7) & diff_rtn_7_cat > 4 ~ cat_avgwgt_rtn_7,
      is.na(rtn_7) & diff_rtn_7_cat <= 4 ~ predictions_model_prod_rtn7,
      
      # Rule 3 for rtn_7: When weighted average is present
      !is.na(rtn_7) & diff_rtn_7 > 2 ~ rtn_7,
      !is.na(rtn_7) & diff_rtn_7 <= 2 ~ predictions_model_prod_rtn7,
      
      # Fallback/default
      TRUE ~ NA_real_
    ),
    final_rtn_13 = case_when(
      # Rule 1 for rtn_13: Highest priority for rtn_13
      # NO RULE HERE :- supply > 85 ~ 1,
      
      # Rule 2 for rtn_13: When weighted average is missing
      is.na(rtn_13) & diff_rtn_13_cat > 4 ~ cat_avgwgt_rtn_13,
      is.na(rtn_13) & diff_rtn_13_cat <= 4 ~ predictions_model_prod_rtn13,
      
      # Rule 3 for rtn_13: When weighted average is present
      !is.na(rtn_13) & diff_rtn_13 > 2 ~ rtn_13,
      !is.na(rtn_13) & diff_rtn_13 <= 2 ~ predictions_model_prod_rtn13,
      
      # Fallback/default
      TRUE ~ NA_real_
    ),
    final_rtn_19 = case_when(
      # Rule 1 for rtn_19: Highest priority for rtn_13
      # NO RULE HERE :- supply > 85 ~ 1,
      
      # Rule 2 for rtn_19: When weighted average is missing
      is.na(rtn_19) & diff_rtn_19_cat > 4 ~ cat_avgwgt_rtn_19,
      is.na(rtn_19) & diff_rtn_19_cat <= 4 ~ predictions_model_prod_rtn19,
      
      # Rule 3 for rtn_19: When weighted average is present
      !is.na(rtn_19) & diff_rtn_19 > 2 ~ rtn_19,
      !is.na(rtn_19) & diff_rtn_19 <= 2 ~ predictions_model_prod_rtn19,
      
      # Fallback/default
      TRUE ~ NA_real_
    ),
    final_rtn_25 = case_when(
      # Rule 1 for rtn_25: Highest priority for rtn_25
      # NO RULE HERE :- supply > 85 ~ 1,
      
      # Rule 2 for rtn_25: When weighted average is missing
      is.na(rtn_25) & diff_rtn_25_cat > 4 ~ cat_avgwgt_rtn_25,
      is.na(rtn_25) & diff_rtn_25_cat <= 4 ~ predictions_model_prod_rtn25,
      
      # Rule 3 for rtn_25: When weighted average is present
      !is.na(rtn_25) & diff_rtn_25 > 2 ~ rtn_25,
      !is.na(rtn_25) & diff_rtn_25 <= 2 ~ predictions_model_prod_rtn25,
      
      # Fallback/default
      TRUE ~ NA_real_
    )
  )

write.csv(data_rtn_final_11Feb25_prod, file = "/Users/cdowsett/Library/CloudStorage/GoogleDrive-cdowsett@forhims.com/My Drive/data_rtn_final_14Feb25_prod.csv", row.names = FALSE)

