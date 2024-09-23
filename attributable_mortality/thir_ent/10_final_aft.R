# Clear the environment
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr, survival, gt)
})

# Define working directory
wd <- "./"
setwd(wd)

# data
df <- readRDS("data/att_first28.RData")
organism <- readRDS("data/clean data/ast_all_index.RData")

# ------
# Prevalence
organism <- organism %>% 
  select(recordid, org_names_all, infection_types, starts_with("ris")) %>% 
  group_by(recordid, org_names_all, infection_types) %>%  
  slice(1) %>%
  ungroup()

# Split the organism dataframe by the infection_types column
df_organism <- split(organism, organism$infection_types)

# Initialize an empty list to store the results
pop_exp <- list()

# Loop through each subset of df_organism
for (i in seq_along(df_organism)) {
  # Calculate the proportion of cases where `ris_Third-generation cephalosporins` is 1
  pop_p <- length(which((df_organism[[i]]$`ris_Third-generation cephalosporins` == 1) & (df_organism[[i]]$org_names_all %in% c("E.coli", "Klebsiella pneumoniae", "Klebsiella", "Enterobacter", "Serratia", "Proteus", "Morganella")))) / nrow(df_organism[[i]])
  
  # Store the infection_types and the calculated proportion in the results list
  pop_exp[[i]] <- data.frame(
    Infection_Type = names(df_organism)[i],
    proportions_exposed = pop_p
  )
}

# Combine the results into a single dataframe
pop_exp_df <- do.call(rbind, pop_exp)

# Enterobacterales, third-generation cephalosporin-resistant
df <- as.data.frame(df[[3]])
df <- df %>%
  rename(ris_third = `ris_Third-generation cephalosporins`)

df$ris_third <- as.factor(ifelse(df$ris_third == 1, 1, 0))

# Model
model <- survreg(formula = Surv(time, event) ~ 
                   age_new + sex + 
                   country_income +
                   hpd_admreason +
                   severity_score_scale +
                   icu_hd_ap + pathogen_combined_types + 
                   ris_third + strata(infection_types), 
                 data = df, dist = "lognormal")

# Extract coefficients
coef_summary <- summary(model)$table
coef_values <- coef_summary[, "Value"]
coef_se <- coef_summary[, "Std. Error"]
p_values <- coef_summary[, "p"]

# Define model scales
model_scales <- c("VAP" = 1.784, 
                  "Hospital-acquired BSI" = 2.014, 
                  "Healthcare-associated BSI" = 1.891)

# Function to calculate time ratios
calculate_time_ratios <- function(value, se, scales) {
  ratios <- exp(value / scales)
  lower_ci <- exp((value - 1.96 * se) / scales)
  upper_ci <- exp((value + 1.96 * se) / scales)
  return(data.frame(Time_Ratio = ratios, Lower_CI = lower_ci, Upper_CI = upper_ci))
}

# Calculate time ratios and hazard ratios
results <- lapply(1:length(coef_values), function(i) {
  var_name <- names(coef_values)[i]
  value <- coef_values[i]
  se <- coef_se[i]
  p_value <- p_values[i]
  time_ratios <- calculate_time_ratios(value, se, model_scales)
  
  # Hazard Ratio
  time_ratios$Hazard_Ratio <- 1 / time_ratios$Time_Ratio
  time_ratios$Lower_HR <- 1 / time_ratios$Upper_CI
  time_ratios$Upper_HR <- 1 / time_ratios$Lower_CI
  time_ratios$P_Value <- ifelse(p_value < 0.001, "<0.001", 
                                ifelse(p_value < 0.01, "<0.01", 
                                       ifelse(p_value < 0.05, "<0.05", sprintf("%.3f", p_value))))
  time_ratios$Variable <- var_name
  time_ratios$Scale <- names(model_scales)
  return(time_ratios)
})

# Combine results into a dataframe
time_ratio_df <- do.call(rbind, results)

# Extract the hazard ratio for ris_third
hr_third <- time_ratio_df[time_ratio_df$Variable == "ris_third1", c("Hazard_Ratio", "Lower_HR", "Upper_HR", "Scale")]

# Function to calculate incidence rate among non-exposed individuals for each scale
calculate_incidence_rate <- function(df, infection_type) {
  subset_df <- df[df$infection_types == infection_type & df$ris_third == 0, ]
  incidence_rate <- sum(subset_df$event == 1) / nrow(subset_df)
  return(incidence_rate)
}

# Calculate I0 for each scale
I0_VAP <- calculate_incidence_rate(df, "VAP")
I0_BSI <- calculate_incidence_rate(df, "Hospital-acquired BSI")
I0_HABSI <- calculate_incidence_rate(df, "Healthcare-associated BSI")

# Create a vector of I0 values corresponding to each scale
I0_values <- c(I0_VAP, I0_BSI, I0_HABSI)

# Function to calculate attributable risk (AR)
calculate_ar <- function(hazard_ratio, I0) {
  ar <- I0 * (hazard_ratio - 1)
  return(ar)
}

# Function to calculate attributable fraction (AF)
calculate_af <- function(hazard_ratio) {
  af <- (hazard_ratio - 1) / hazard_ratio
  return(af)
}

# Calculate AR and AF for ris_third
ar_af_third <- do.call(rbind, lapply(1:nrow(hr_third), function(i) {
  hr <- hr_third$Hazard_Ratio[i]
  lower_hr <- hr_third$Lower_HR[i]
  upper_hr <- hr_third$Upper_HR[i]
  scale <- hr_third$Scale[i]
  I0 <- I0_values[i]
  ar <- calculate_ar(hr, I0)
  af <- calculate_af(hr)
  lower_ar <- calculate_ar(lower_hr, I0)
  upper_ar <- calculate_ar(upper_hr, I0)
  lower_af <- calculate_af(lower_hr)
  upper_af <- calculate_af(upper_hr)
  return(data.frame(Infection_Type = scale, 
                    Hazard_Ratio = hr, Lower_HR = lower_hr, Upper_HR = upper_hr,
                    Attributable_Risk = ar, Lower_AR = lower_ar, Upper_AR = upper_ar, 
                    Attributable_Fraction = af, Lower_AF = lower_af, Upper_AF = upper_af))
}))

# 
rownames(ar_af_third) <- NULL

# Define proportions of exposed subjects for each infection types
# Add proportions to the AR and AF dataframe
ar_af_third <- left_join(ar_af_third, pop_exp_df, by = "Infection_Type")


# Function to calculate population attributable fraction (PAF)
calculate_paf <- function(hr, proportion_exposed) {
  return((proportion_exposed * (hr - 1)) / (1 + proportion_exposed * (hr - 1)))
}


# Initialize PAF column
ar_af_third$Population_Attributable_Fraction <- NA
ar_af_third$Lower_PAF <- NA
ar_af_third$Upper_PAF <- NA

# Calculate PAF for each scale with the corresponding proportion of exposed subjects
for (i in 1:nrow(ar_af_third)) {
  hr <- ar_af_third$Hazard_Ratio[i]
  lower_hr <- ar_af_third$Lower_HR[i]
  upper_hr <- ar_af_third$Upper_HR[i]
  proportion_exposed <- ar_af_third$proportions_exposed[i]
  ar_af_third$Population_Attributable_Fraction[i] <- calculate_paf(hr, proportion_exposed)
  ar_af_third$Lower_PAF[i] <- calculate_paf(lower_hr, proportion_exposed)
  ar_af_third$Upper_PAF[i] <- calculate_paf(upper_hr, proportion_exposed)
}

# Format the AR, AF, and PAF columns as percentages with 95% CI
ar_af_third <- ar_af_third %>%
  mutate(
    AR_CI = sprintf("%.2f%% (%.2f%%, %.2f%%)", Attributable_Risk * 100, Lower_AR * 100, Upper_AR * 100),
    AF_CI = sprintf("%.2f%% (%.2f%%, %.2f%%)", Attributable_Fraction * 100, Lower_AF * 100, Upper_AF * 100),
    PAF_CI = sprintf("%.2f%% (%.2f%%, %.2f%%)", Population_Attributable_Fraction * 100, Lower_PAF * 100, Upper_PAF * 100)
  ) %>%
  dplyr::select(Infection_Type, AR_CI, proportions_exposed, PAF_CI)

ar_af_third$proportions_exposed <- sprintf("%.2f%%", ar_af_third$proportions_exposed * 100)

# Save
saveRDS(ar_af_third, "data/att28_table_paf_02.RData")

#
# Create your gt table
table <- ar_af_third %>%
  gt() %>%
  cols_label(
    `Infection_Type` = md("Infection types"),
    `AR_CI` = md("Attributable Risk (95%CI)"),
    `proportions_exposed` = md("Proportion of cases exposed"),
    `PAF_CI` = md("Population Attributable Fraction (95%CI)")
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  )%>%
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2.5),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2.5),
    table_body.hlines.color = "white",
    table_body.hlines.width = px(0),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(0),
    data_row.padding = px(0)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(2.5)
    ),
    locations = cells_body(
      rows = nrow(ar_af_third)
    )
  ) %>%
  cols_align(
    align = "center",
    columns = 2:4
  ) %>%
  cols_width(
    1 ~ px(200),
    2 ~ px(200),
    3 ~ px(210),
    4 ~ px(260)
  ) %>%
  tab_style(
    style = cell_text(align = "left", v_align = "middle"),
    locations = cells_column_labels(columns = 1)
  )  %>%
  tab_source_note(
    source_note = "Abbreviations: CI = Confidence Interval."
  ) 

table

# Save
gtsave(data = table, filename = "output/table/table_paf.html")
