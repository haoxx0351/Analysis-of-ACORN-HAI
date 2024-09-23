# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr,
                 survival,
                 ggplot2,
                 MASS,
                 glmnet,
                 car,
                 gt,
                 purrr,
                 extrafont)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load fonts
loadfonts()

# Load data
df <- readRDS("data/clean data_V2/first28_data.RData")

# Accelerated Failure Time
model_aft <- survreg(Surv(time, event) ~ age_new + sex + 
                       country_region + country_income +
                       hpd_admreason + comorbidities_Chalson + 
                       severity_score_scale +
                       icu_hd_ap + pathogen_combined_types +
                       GN_car_thir + strata(infection_types), 
                     data = df, dist = "lognormal")
summary(model_aft)

# LASSO
# For variable selection when there is multicollinearity among the predictors
# Matrix for factor
x.factors <- model.matrix(~ df$sex + 
                            df$country_region + df$country_income +
                            df$hpd_admreason + df$pathogen_combined_types +
                            df$icu_hd_ap + df$GN_car_thir)[,-1]

# for num
x.factors <- as.matrix(data.frame(x.factors, df$age_new, df$comorbidities_Chalson, df$severity_score_scale))

#
y <- Surv(df$time, df$event == 1)

lasso <- glmnet(x.factors, y, family = "cox", alpha = 1)

#
par(mar=c(1,1,1,1))
plot(lasso, xvar="lambda", label=T)

#
set.seed(1234)
lasso_comp_cv <- cv.glmnet(x.factors, y,
                           family="cox", 
                           alpha=1,
                           nfolds=5)

plot(lasso_comp_cv)
coef(lasso_comp_cv, s="lambda.min")

###
x.factors.df <- data.frame(x.factors)
dummy_y <- rnorm(nrow(x.factors.df))

model <- lm(dummy_y ~ ., data = x.factors.df)

# Variance Inflation Factor (VIF)
vif_values <- vif(model)
vif_df <- as.data.frame(vif_values)
vif_df$vif_values <- round(vif_df$vif_values, 3)

# Creat table
table_row <- list(
  c("Age", NA),
  c(NA, vif_df$vif_values[23]),
  c("Sex", NA),
  c("Female", "—"),
  c("Male", vif_df$vif_values[1]),
  c("Region", NA),
  c("Eastern Mediterranean Region", "—"),
  c("South-East Asian Region", vif_df$vif_values[2]),
  c("Western Pacific Region", vif_df$vif_values[3]),
  c("World Bank income status", NA),
  c("High income", "—"),
  c("Upper middle income", vif_df$vif_values[4]),
  c("Lower middle income", vif_df$vif_values[5]),
  c("Low income", vif_df$vif_values[6]),
  c("Primary admission reason", NA),
  c("Infectious disease", "—"),
  c("Cardiovascular condition", vif_df$vif_values[7]),
  c("Gastrointestinal disorder", vif_df$vif_values[8]),
  c("Haematological disease", vif_df$vif_values[9]),
  c("Neurological disease", vif_df$vif_values[10]),
  c("Oncologic disorder", vif_df$vif_values[11]),
  c("Orthopaedic condition", vif_df$vif_values[12]),
  c("Pulmonary disease", vif_df$vif_values[13]),
  c("Renal disorder", vif_df$vif_values[14]),
  c("Trauma", vif_df$vif_values[15]),
  c("Others", vif_df$vif_values[16]),
  c("Charlson comorbidity index", NA),
  c(NA, vif_df$vif_values[24]),
  c("Severity score of disease", NA),
  c(NA, vif_df$vif_values[25]),
  c("ICU/HD admissions of enrolment", NA),
  c("No", "—"),
  c("Yes", vif_df$vif_values[20]),
  c("Types of pathogens", NA),
  c("Gram-negative bacteria", "—"),
  c("Gram-positive bacteria", vif_df$vif_values[17]),
  c("Fungi", vif_df$vif_values[18]),
  c("Polymicrobial", vif_df$vif_values[19]),
  c("Gram-negative bacteria", NA),
  c("Third-generation cephalosporins-susceptible", "—"),
  c("Third-generation cephalosporins-resistant", vif_df$vif_values[21]),
  c("Carbapenem-resistant", vif_df$vif_values[22])
)


# Bind the rows into a matrix
table_vif <- do.call(rbind, table_row)

#
table_vif <- table_vif %>%
  as.data.frame() %>%
  mutate(across(1, ~ ifelse(.x == "NA", "", .x)))

colnames(table_vif) <- c("Variables", "Variance inflation factor")

#
table_gt <- table_vif %>%
  gt() %>%
  sub_missing(
    columns = 1:2,
    missing_text = ""
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  ) %>%
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
      rows = nrow(table_vif)
    )
  ) %>%
  cols_align(
    align = "center",
    columns = 2
  )  %>%
  tab_style(
    style = cell_text(align = "left", v_align = "middle"),
    locations = cells_column_labels(columns = 1)
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      columns = 1,
      rows = c(1, 3, 6, 10, 15, 27, 29, 31, 34, 39) 
    )
  ) %>%
  tab_style(
    style = cell_text(
      font = c("Times New Roman"),
      size = px(14)
    ),
    locations = list(
      cells_title(groups = "title"),
      cells_title(groups = "subtitle"),
      cells_column_labels(),
      cells_body()
    )
  ) 

print(table_gt)

# Save
gtsave(table_gt, filename = "output/table/first28_LASSO.html")

# ----------------------------------------------------------
# Univariable analysis
# potential variables
variables <- c("infection_types", 
               "age_new", "sex", 
               "country_region", "country_income",
               "hpd_admreason", "comorbidities_Chalson", 
               "severity_score_scale",
               "icu_hd_ap",
               "pathogen_combined_types",
               "GN_car_thir")

# Initialize the data frame to store results
p_values <- data.frame(p.value = character(), ratio = numeric(), lower_ci = numeric(), upper_ci = numeric(), stringsAsFactors = FALSE)

for (var in variables) {
  formula <- as.formula(paste("Surv(time, event) ~", var, "+ strata(infection_types)"))
  univariate_model <- survreg(formula, data = df, dist = "lognormal")
  model_summary <- summary(univariate_model)
  
  # Skip Intercept and scale
  variable_names <- rownames(model_summary$table)
  values <- model_summary$table[, 1]
  se_values <- model_summary$table[, 2]
  p_values_for_var <- model_summary$table[, 4]
  
  for (i in seq_along(variable_names)) {
    if (!grepl("Intercept|log\\(scale\\)|VAP|BSI", variable_names[i])) {
      p_value <- p_values_for_var[i]
      value <- values[i]
      se <- se_values[i]
      
      ratio <- 1/exp(value)
      upper_ci <- 1/exp(value - 1.96 * se)
      lower_ci <- 1/exp(value + 1.96 * se)
      
      significance <- ifelse(p_value < 0.001, "<0.001", 
                             ifelse(p_value < 0.01, "<0.01", 
                                    ifelse(p_value < 0.05, "<0.05", sprintf("%.3f", p_value))))
      
      p_values <- rbind(p_values, data.frame(p.value = significance, ratio = ratio, lower_ci = lower_ci, upper_ci = upper_ci))
    }
  }
}

# Display the final data frame
p_values

# -------------------------------------------------------------------
ratio_df <- transform(p_values, 
                      Ratio = sprintf("%.3f", ratio), 
                      Lower_CI = sprintf("%.3f", lower_ci), 
                      Upper_CI = sprintf("%.3f", upper_ci))

ratio_df$`ratio (95%CI)` = paste0(ratio_df$Ratio, " (", ratio_df$Lower_CI, "-", ratio_df$Upper_CI, ")", sep = "")

###
rows <- list(
  c("Characteristics", NA, NA, NA, "Crude HR (95%CI)", "p.value"),
  c("Age", NA, NA, NA, NA, NA),
  c(NA, ratio_df[1, c(5:8, 1)]),
  c("Sex", NA, NA, NA, NA, NA),
  c("Female", NA, NA, NA, "Ref", NA),
  c("Male", ratio_df[2, c(5:8, 1)]),
  c("Region", NA, NA, NA, NA, NA),
  c("Eastern Mediterranean Region", NA, NA, NA, "Ref", NA),
  c("South-East Asian Region", ratio_df[3, c(5:8, 1)]),
  c("Western Pacific Region", ratio_df[4, c(5:8, 1)]),
  c("World Bank income status", NA, NA, NA, NA, NA),
  c("High income", NA, NA, NA, "Ref", NA),
  c("Upper middle income", ratio_df[5, c(5:8, 1)]),
  c("Lower middle income", ratio_df[6, c(5:8, 1)]),
  c("Low income", ratio_df[7, c(5:8, 1)]),
  c("Primary admission reason", NA, NA, NA, NA, NA),
  c("Infectious disease", NA, NA, NA, "Ref", NA),
  c("Cardiovascular condition", ratio_df[8, c(5:8, 1)]),
  c("Gastrointestinal disorder", ratio_df[9, c(5:8, 1)]),
  c("Haematological disease", ratio_df[10, c(5:8, 1)]),
  c("Neurological disease", ratio_df[11, c(5:8, 1)]),
  c("Oncologic disorder", ratio_df[12, c(5:8, 1)]),
  c("Orthopaedic condition", ratio_df[13, c(5:8, 1)]),
  c("Pulmonary disease", ratio_df[14, c(5:8, 1)]),
  c("Renal disorder", ratio_df[15, c(5:8, 1)]),
  c("Trauma", ratio_df[16, c(5:8, 1)]),
  c("Others", ratio_df[17, c(5:8, 1)]),
  c("Charlson comorbidity index", NA, NA, NA, NA, NA),
  c(NA, ratio_df[18, c(5:8, 1)]),
  c("Severity score of disease", NA, NA, NA, NA, NA),
  c(NA, ratio_df[19, c(5:8, 1)]),
  c("ICU/HD admissions of enrolment", NA, NA, NA, NA, NA),
  c("No", NA, NA, NA, "Ref", NA),
  c("Yes", ratio_df[20, c(5:8, 1)]),
  c("Types of pathogens", NA, NA, NA, NA, NA),
  c("Gram-negative bacteria", NA, NA, NA, "Ref", NA),
  c("Gram-positive bacteria", ratio_df[21, c(5:8, 1)]),
  c("Fungi", ratio_df[22, c(5:8, 1)]),
  c("Polymicrobial", ratio_df[23, c(5:8, 1)]),
  c("Gram-negative bacteria", NA, NA, NA, NA, NA),
  c("Third-generation cephalosporins-susceptible", NA, NA, NA, "Ref", NA),
  c("Third-generation cephalosporins-resistant", ratio_df[24, c(5:8, 1)]),
  c("Carbapenem-resistant", ratio_df[25, c(5:8, 1)])
 )

# Bind the rows into a matrix
result <- do.call(rbind, rows)

#
result <- as.data.frame(result)

result[, 5:6] <- lapply(result[, 5:6], as.character)
result[, 2:4] <- lapply(result[, 2:4], as.numeric)

# Save table
result_save <- result[, c(1, 5, 6)]
saveRDS(result_save, "data/clean data_V2/frist28_table_univariable.RData")

###
# Multi model
aft_fit <- survreg(formula = Surv(time, event) ~ 
                     age_new + sex + country_income+
                     hpd_admreason +
                     icu_hd_ap + GN_car_thir +
                     comorbidities_Chalson + severity_score_scale +
                     strata(infection_types), 
                   data = df, dist = "lognormal")

summary(aft_fit)

residuals_aft <- residuals(aft_fit, type = "deviance")
df$residuals <- residuals_aft

# 
ggplot(df, aes(x = residuals)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Deviance Residuals", x = "Residuals", y = "Frequency")

qqnorm(df$residuals, main = "QQ Plot of Deviance Residuals")
qqline(df$residuals, col = "red")

# Shapiro-Wilk
shapiro_test <- shapiro.test(df$residuals)
print(shapiro_test)
