# Load necessary libraries
library(dplyr)
library(magrittr)

# Define a function to process each metric and calculate the weighted average per prod_grp
process_metric <- function(metric_name, dataProd) {
  # Filter the data for the given metric where it's not NA and cnt_subs > 10
  data_metric <- dataProd %>%
    filter(!is.na(!!sym(metric_name)) & cnt_subs > 10) %>%
    arrange(prod_grp, desc(create_mth)) %>%
    group_by(prod_grp) %>%
    mutate(row_num = row_number()) %>%
    ungroup()
  
  # Get the top 3 rows per prod_grp for the given metric
  data_metric_top3 <- data_metric %>%
    filter(row_num <= 3) %>%
    select(prod_grp, create_mth, cnt_subs, !!sym(metric_name), row_num)
  
  # Calculate the weighted average per prod_grp for the given metric
  weighted_avg_column <- paste0("weighted_avg_", metric_name)
  
  data_metric_top3 <- data_metric_top3 %>%
    group_by(prod_grp) %>%
    mutate(!!weighted_avg_column := weighted.mean(!!sym(metric_name), w = cnt_subs, na.rm = TRUE)) %>%
    ungroup()
  
  return(data_metric_top3)
}

# Process each of the metrics
metrics <- c("rtn_4", "rtn_7", "rtn_13", "rtn_19", "rtn_25")

final_data <- NULL

for (metric in metrics) {
  data_processed <- process_metric(metric, dataProd)
  
  # Add the data to the final_data table
  if (is.null(final_data)) {
    final_data <- data_processed
  } else {
    final_data <- bind_rows(final_data, data_processed)
  }
}

# Add the new column where cnt_subs < 50 gets 1, else 0
final_data <- final_data %>%
  mutate(cnt_subs_check = if_else(cnt_subs < 50, 1, 0))

# View the final combined data with weighted averages per prod_grp
print(final_data)

# ---- SECTION 2 

# Calculate the max of each specified column for each prod_grp

safe_max <- function(x) {
  if (all(is.na(x))) {
    NA_real_
  } else {
    max(x, na.rm = TRUE)
  }
}

# Calculate the max of each specified column for each prod_grp and handle missing values
final_summary <- final_data %>%
  group_by(prod_grp) %>%
  summarize(
    min_cnt_subs = safe_max(cnt_subs),
    max_cnt_subs = safe_max(cnt_subs),
    max_rtn_4 = safe_max(rtn_4),
    max_rtn_7 = safe_max(rtn_7),
    max_rtn_13 = safe_max(rtn_13),
    max_rtn_19 = safe_max(rtn_19),
    max_rtn_25 = safe_max(rtn_25),
    max_weighted_avg_rtn_4 = safe_max(weighted_avg_rtn_4),
    max_weighted_avg_rtn_7 = safe_max(weighted_avg_rtn_7),
    max_weighted_avg_rtn_13 = safe_max(weighted_avg_rtn_13),
    max_weighted_avg_rtn_19 = safe_max(weighted_avg_rtn_19),
    max_weighted_avg_rtn_25 = safe_max(weighted_avg_rtn_25)
  ) %>%
  mutate(
    rtn_25_flg = if_else(max_weighted_avg_rtn_25 > max_weighted_avg_rtn_19, 1, 0),
    rtn_19_flg = if_else(max_weighted_avg_rtn_19 > max_weighted_avg_rtn_13, 1, 0),
    rtn_13_flg = if_else(max_weighted_avg_rtn_13 > max_weighted_avg_rtn_7, 1, 0),
    rtn_7_flg = if_else(max_weighted_avg_rtn_7 > max_weighted_avg_rtn_4, 1, 0)
  )

    

# View the final summary table
print(final_summary)

# Write the final summary table to a CSV file
write.csv(final_summary, "final_summary.csv", row.names = FALSE)

