#Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr,
                 magrittr,
                 survival,
                 survminer,
                 ggplot2)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/att_first28.RData")

###
# Acinetobacter, carbapenem-resistant
df <- as.data.frame(df[[1]])
df$ris_Carbapenems <- as.factor(ifelse(df$ris_Carbapenems == 1, 1, 0))

# Revise "Gram-negative bacteria" to "Monomicrobial"
df$pathogen_combined_types <- factor(df$pathogen_combined_types, 
                                     levels = c("Gram-negative bacteria", "Polymicrobial"),
                                     labels = c("Monomicrobial", "Polymicrobial"))

# Delete Intercept and log(scale)
del_row <- c("(Intercept)", "VAP", "Hospital-acquired BSI", "Healthcare-associated BSI")

# MODEL 1
# Delete pathogen_combined_types, country_region, icu_hd_ap
model <- survreg(formula = Surv(time, event) ~ 
                    age_new + sex + 
                    country_income +
                    hpd_admreason + 
                    comorbidities_Chalson + severity_score_scale +
                    ris_Carbapenems + strata(infection_types), 
                  data = df, dist = "lognormal")

p_model <- data.frame(summary(model)$table)
p_model$ratio <- 1/exp(p_model$Value)
p_model$upper_ci <- 1/exp(p_model$Value - 1.96 * p_model$Std..Error)
p_model$lower_ci <- 1/exp(p_model$Value + 1.96 * p_model$Std..Error)

p_model$p.value <- ifelse(p_model$p < 0.001, "<0.001", 
                           ifelse(p_model$p < 0.01, "<0.01", 
                                  ifelse(p_model$p < 0.05, "<0.05", sprintf("%.3f", p_model$p))))

p_model <- p_model[-which(rownames(p_model) %in% del_row), ]

ratio_df <- transform(p_model, 
                      Ratio = sprintf("%.3f", ratio), 
                      Lower_CI = sprintf("%.3f", lower_ci), 
                      Upper_CI = sprintf("%.3f", upper_ci))

ratio_df$`ratio (95%CI)` = paste0(ratio_df$Ratio, " (", ratio_df$Lower_CI, "-", ratio_df$Upper_CI, ")", sep = "")

###
rows <- list(
  c("Characteristics", NA, NA, NA, "Adjusted HR (95%CI)", "p.value"),
  c("Age", NA, NA, NA, NA, NA),
  c(NA, ratio_df[1, c(9:12, 8)]),
  c("Sex", NA, NA, NA, NA, NA),
  c("Female", NA, NA, NA, "Ref", NA),
  c("Male", ratio_df[2, c(9:12, 8)]),
  c("Region", NA, NA, NA, NA, NA),
  c("Eastern Mediterranean Region", NA, NA, NA, NA, NA),
  c("South-East Asian Region", NA, NA, NA, NA, NA),
  c("Western Pacific Region", NA, NA, NA, NA, NA),
  c("World Bank income status", NA, NA, NA, NA, NA),
  c("High income", NA, NA, NA, "Ref", NA),
  c("Upper middle income", ratio_df[3, c(9:12, 8)]),
  c("Lower middle income", ratio_df[4, c(9:12, 8)]),
  c("Low income", ratio_df[5, c(9:12, 8)]),
  c("Primary admission reason", NA, NA, NA, NA, NA),
  c("Infectious disease", NA, NA, NA, "Ref", NA),
  c("Cardiovascular condition", ratio_df[6, c(9:12, 8)]),
  c("Gastrointestinal disorder", ratio_df[7, c(9:12, 8)]),
  c("Neurological disease", ratio_df[8, c(9:12, 8)]),
  c("Oncologic disorder", ratio_df[9, c(9:12, 8)]),
  c("Pulmonary disease", ratio_df[10, c(9:12, 8)]),
  c("Renal disorder", ratio_df[11, c(9:12, 8)]),
  c("Trauma", ratio_df[12, c(9:12, 8)]),
  c("Others", ratio_df[13, c(9:12, 8)]),
  c("Charlson comorbidity index", NA, NA, NA, NA, NA),
  c(NA, ratio_df[14, c(9:12, 8)]),
  c("Severity score of disease", NA, NA, NA, NA, NA),
  c(NA, ratio_df[15, c(9:12, 8)]),
  c("ICU/HD admissions of enrolment", NA, NA, NA, NA, NA),
  c("No", NA, NA, NA, NA, NA),
  c("Yes", NA, NA, NA, NA, NA),
  c("Types of pathogens", NA, NA, NA, NA, NA),
  c("Monomicrobial", NA, NA, NA, NA, NA),
  c("Polymicrobial", NA, NA, NA, NA, NA),
  c("Acinetobacter spp.", NA, NA, NA, NA, NA),
  c("Carbapenem-susceptible", NA, NA, NA, "Ref", NA),
  c("Carbapenem-resistant", ratio_df[16, c(9:12, 8)]))

# Bind the rows into a matrix
result <- do.call(rbind, rows)

#
result <- as.data.frame(result)

result[, 5:6] <- lapply(result[, 5:6], as.character)
result[, 2:4] <- lapply(result[, 2:4], as.numeric)

# Save table
result_save <- result[, c(1, 5, 6)]
saveRDS(result_save, "data/att28_table_multi_1.RData")
saveRDS(df, "data/CRA_data.RData") 