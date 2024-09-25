# Clear environment
rm(list = ls())

# Load necessary packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr, magrittr, crrSC, cmprsk, forestplot)
})

# Load data
df <- readRDS("data/readmission_table_index.RData")

# Delete comorbidities_Chalson
x.factors <- model.matrix(~ df$sex + df$country_region +
                            df$country_income + 
                            df$hpd_admreason +
                            df$icu_hd_ap + df$pathogen_combined_types+
                            df$GN_car_thir)[,-1]

# for num
x.factors <- as.matrix(data.frame(x.factors, df$age_new, 
                                  df$severity_score_scale))

# Competing risk model
fg_mod_strat <- crrs(ftime = df$readm_death_time, 
                     fstatus = df$readm_event, 
                     x.factors, 
                     strata = df$infection_types, 
                     failcode = 1, cencode = 0, ctype = 1)

#
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

combined_results <- custom_summary_crrs(fg_mod_strat, colnames(x.factors))

#
# Plot
hazard_ratio_df <- transform(combined_results, 
                             Hazard_Ratio = sprintf("%.3f", exp_coef), 
                             Lower_CI = sprintf("%.3f", lower_95), 
                             Upper_CI = sprintf("%.3f", upper_95))

hazard_ratio_df$`Hazard ratio (95%CI)` = paste0(hazard_ratio_df$Hazard_Ratio, " (", hazard_ratio_df$Lower_CI, "-", hazard_ratio_df$Upper_CI, ")", sep = "")


df_used <- hazard_ratio_df %>% 
  select(variable, Hazard_Ratio, Lower_CI, Upper_CI, `Hazard ratio (95%CI)`, p_value)

###
result <- rbind(
  c("Characteristics", NA, NA, NA, "Adjusted HR (95%CI)", "p.value"),
  c("Age", NA, NA, NA, NA, NA),
  c(NA, df_used[24, c(2:6)]),
  c("Sex", NA, NA, NA, NA, NA),
  c("Female", NA, NA, NA, "Ref", NA),
  c("Male", df_used[1, c(2:6)]),
  c("Region", NA, NA, NA, NA, NA),
  c("Eastern Mediterranean Region", NA, NA, NA, "Ref", NA),
  c("South-East Asian Region", df_used[2, c(2:6)]),
  c("Western Pacific Region", df_used[3, c(2:6)]),
  c("World Bank income status", NA, NA, NA, NA, NA),
  c("High income", NA, NA, NA, "Ref", NA),
  c("Upper middle income", df_used[4, c(2:6)]),
  c("Lower middle income", df_used[5, c(2:6)]),
  c("Low income", df_used[6, c(2:6)]),
  c("Primary admission reason", NA, NA, NA, NA, NA),
  c("Infectious disease", NA, NA, NA, "Ref", NA),
  c("Cardiovascular condition", df_used[7, c(2:6)]),
  c("Endocrine / Metabolic disorder", df_used[8, c(2:6)]),
  c("Gastrointestinal disorder", df_used[9, c(2:6)]),
  c("Haematological disease", df_used[10, c(2:6)]),
  c("Neurological disease", df_used[11, c(2:6)]),
  c("Oncologic disorder", df_used[12, c(2:6)]),
  c("Orthopaedic condition", df_used[13, c(2:6)]),
  c("Pulmonary disease", df_used[14, c(2:6)]),
  c("Renal disorder", df_used[15, c(2:6)]),
  c("Trauma", df_used[16, c(2:6)]),
  c("Others", df_used[17, c(2:6)]),
  c("Charlson comorbidity index", NA, NA, NA, NA, NA),
  c(NA, NA, NA, NA, NA, NA),
  c("Severity score of disease", NA, NA, NA, NA, NA),
  c(NA, df_used[25, c(2:6)]),
  c("ICU/HD admissions of enrolment", NA, NA, NA, NA, NA),
  c("No", NA, NA, NA, "Ref", NA),
  c("Yes", df_used[18, c(2:6)]),
  c("Types of pathogens", NA, NA, NA, NA, NA),
  c("Gram-negative bacteria", NA, NA, NA, "Ref", NA),
  c("Gram-positive bacteria", df_used[19, c(2:6)]),
  c("Fungi", df_used[20, c(2:6)]),
  c("Polymicrobial", df_used[21, c(2:6)]),
  c("Gram-negative bacteria", NA, NA, NA, NA, NA),
  c("Third-generation cephalosporins-susceptible", NA, NA, NA, "Ref", NA),
  c("Third-generation cephalosporins-resistant", df_used[22, c(2:6)]),
  c("Carbapenem-resistant", df_used[23, c(2:6)]))

#
result <- as.data.frame(result)

result[, 5:6] <- lapply(result[, 5:6], as.character)
result[, 2:4] <- lapply(result[, 2:4], as.numeric)

# Save table
result_save <- result[, c(1, 5, 6)]
saveRDS(result_save, "data/readmission_table_multivariable_1.RData")
###