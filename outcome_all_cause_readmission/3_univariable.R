# Clear environment
rm(list = ls())

# Load necessary packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr, 
                 magrittr,
                 crrSC, 
                 cmprsk)
})

# Load data
df <- readRDS("data/readmission60_data.RData")

# Define explanatory variables
explanatory <- c("age_new", "sex", 
                 "country_region", "country_income",
                 "hpd_admreason",
                 "comorbidities_Chalson", 
                 "severity_score_scale",
                 "icu_hd_ap", 
                 "pathogen_combined_types",
                 "GN_car_thir")

# Select relevant columns and remove rows with missing values
df <- df %>%
  select(readm_death_time, readm_event, all_of(explanatory), infection_types) %>%
  filter(complete.cases(.))
# Convert readm_event to factor
df$readm_event <- as.factor(df$readm_event)

# Save
saveRDS(df, "data/readmission_table_index.RData")

# ----------------------------------------------
# Define custom summary function for crrs model
custom_summary_crrs <- function(model, var) {
  # Extract coefficients and variance-covariance matrix
  coefs <- model$coef
  var_matrix <- model$var
  
  # Compute standard errors, z-values, and p-values
  se <- sqrt(diag(var_matrix))
  z_values <- coefs / se
  p_values <- 2 * pnorm(-abs(z_values))
  
  # Format p-values
  p_values_formatted <- ifelse(p_values < 0.001, "<0.001", 
                               ifelse(p_values < 0.01, "<0.01", 
                                      ifelse(p_values < 0.05, "<0.05", sprintf("%.3f", p_values))))
  
  # Exponentiate coefficients and compute confidence intervals
  exp_coefs <- exp(coefs)
  conf_int <- matrix(NA, nrow = length(coefs), ncol = 2)
  for (i in 1:length(coefs)) {
    conf_int[i,] <- exp(c(coefs[i] - 1.96 * se[i], coefs[i] + 1.96 * se[i]))
  }
  
  # Create summary table
  summary_table <- data.frame(
    variable = var,
    coef = coefs,
    exp_coef = exp_coefs,
    se_coef = se,
    z = z_values,
    p_value = p_values_formatted,
    exp_neg_coef = 1 / exp_coefs,
    lower_95 = conf_int[,1],
    upper_95 = conf_int[,2]
  )
  
  return(summary_table)
}

# Initialize list to store results
results_list <- list()

# Iterate over explanatory variables
for (var in explanatory) {
  # Create model matrix for the current variable
  covariates <- model.matrix(as.formula(paste("~", var)), data = df)[,-1]
  
  # Fit the model
  fg_mod_strat <- crrs(ftime = df$readm_death_time, 
                       fstatus = df$readm_event, 
                       covariates, 
                       strata = df$infection_types, 
                       failcode = 1, cencode = 0, ctype = 1)
  
  # Get summary for the current variable
  summary_table <- custom_summary_crrs(fg_mod_strat, var)
  
  # Add variable levels to the summary table, excluding the reference level
  if (is.factor(df[[var]]) || is.character(df[[var]])) {
    levels_var <- levels(df[[var]])
    if (length(levels_var) > (nrow(summary_table) + 1)) {
      summary_table$level <- levels_var[2:(nrow(summary_table) + 1)]
    } else {
      summary_table$level <- levels_var[-1]
    }
  } else {
    summary_table$level <- NA
  }
  
  # Reorder columns to place 'level' as the second column
  summary_table <- summary_table %>% select(variable, level, everything())
  
  # Store results in the list
  results_list[[var]] <- summary_table
}

# Combine all summary tables into a single data frame
combined_results <- bind_rows(results_list, .id = "variable")

# Display the combined results
print(combined_results)

# -------------------------------------------------------------------
# Plot
hazard_ratio_df <- transform(combined_results, 
                             Hazard_Ratio = sprintf("%.3f", exp_coef), 
                             Lower_CI = sprintf("%.3f", lower_95), 
                             Upper_CI = sprintf("%.3f", upper_95))

hazard_ratio_df$`Hazard ratio (95%CI)` = paste0(hazard_ratio_df$Hazard_Ratio, " (", hazard_ratio_df$Lower_CI, "-", hazard_ratio_df$Upper_CI, ")", sep = "")


df_used <- hazard_ratio_df %>% 
  select(variable, level, Hazard_Ratio, Lower_CI, Upper_CI, `Hazard ratio (95%CI)`, p_value)

###
result <- rbind(
  c("Characteristics", NA, NA, NA, "Crude HR (95%CI)", "p.value"),
  c("Age", NA, NA, NA, NA, NA),
  c(df_used[1, c(2:7)]),
  c("Sex", NA, NA, NA, NA, NA),
  c("Female", NA, NA, NA, "Ref", NA),
  c(df_used[2, c(2:7)]),
  c("Region", NA, NA, NA, NA, NA),
  c("Eastern Mediterranean Region", NA, NA, NA, "Ref", NA),
  c(df_used[3, c(2:7)]),
  c(df_used[4, c(2:7)]),
  c("World Bank income status", NA, NA, NA, NA, NA),
  c("High income", NA, NA, NA, "Ref", NA),
  c(df_used[5, c(2:7)]),
  c(df_used[6, c(2:7)]),
  c(df_used[7, c(2:7)]),
  c("Primary admission reason", NA, NA, NA, NA, NA),
  c("Infectious disease", NA, NA, NA, "Ref", NA),
  c(df_used[8, c(2:7)]),
  c(df_used[9, c(2:7)]),
  c(df_used[10, c(2:7)]),
  c(df_used[11, c(2:7)]),
  c(df_used[12, c(2:7)]),
  c(df_used[13, c(2:7)]),
  c(df_used[14, c(2:7)]),
  c(df_used[15, c(2:7)]),
  c(df_used[16, c(2:7)]),
  c("Charlson comorbidity index", NA, NA, NA, NA, NA),
  c(df_used[17, c(2:7)]),
  c("Severity score of disease", NA, NA, NA, NA, NA),
  c(df_used[18, c(2:7)]),
  c("ICU/HD admissions of enrolment", NA, NA, NA, NA, NA),
  c("No", NA, NA, NA, "Ref", NA),
  c(df_used[19, c(2:7)]),
  c("Type of pathogens", NA, NA, NA, NA, NA),
  c("Monomicrobial", NA, NA, NA, "Ref", NA),
  c(df_used[20, c(2:7)]),
  c("Gram-negative bacteria", NA, NA, NA, NA, NA),
  c("Third-generation cephalosporins-susceptible", NA, NA, NA, "Ref", NA),
  c(df_used[21, c(2:7)]),
  c(df_used[22, c(2:7)]))

#
result <- as.data.frame(result)

result[, 5:6] <- lapply(result[, 5:6], as.character)
result[, 2:4] <- lapply(result[, 2:4], as.numeric)

# Save table
result_save <- result[, c(1, 5, 6)]
saveRDS(result_save, "data/readmission_table_univariable.RData")
###