# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magrittr,
                 dplyr,
                 gt,
                 gtsummary)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/clean data/data_table_index_new.RData")

#
df$hpd_adm_date <- as.Date(df$hpd_adm_date, format = "%Y-%m-%d")
df$readm_ap_1_2 <- as.Date(df$readm_ap_1_2, format = "%Y/%m/%d")
df$readm_ori <- df$hpd_adm_date + 60

#
df$readm_60days <- ifelse(df$readm_ap == "Yes" & df$readm_ori >= df$readm_ap_1_2, 1, 0)

df$readm_observed_end <- as.Date(ifelse(df$readm_60days == 1, 
                                        df$readm_ap_1_2, 
                                        df$readm_ori), origin = "1970-01-01")

df$readm_60days_death <- ifelse(df$mortality_date <= df$readm_observed_end, 2, 0)
#
df$readm_event <- ifelse(!is.na(df$readm_60days_death) & df$readm_60days_death == 2, 
                         df$readm_60days_death, 
                         coalesce(df$readm_60days, df$readm_60days_death))

df$readm_death_end <- as.Date(ifelse(df$readm_event == 2, 
                                     df$mortality_date, 
                                     df$readm_observed_end), origin = "1970-01-01")

df$readm_death_time <- as.numeric(difftime(df$readm_death_end, df$hpd_adm_date, units = "days"))

# Delete readm_death_time <= 0 
df <- df[df$readm_death_time > 0, ]

# Delete NA
df <- df[complete.cases(df[c("infection_types", 
                             "age_new", "sex", 
                             "country_region", "country_income",
                             "hpd_admreason",
                             "comorbidities_Chalson", 
                             "severity_score_scale",
                             "icu_hd_ap", 
                             "pathogen_combined_types",
                             "GN_car_thir",
                             "readm_death_time", "readm_event")]), ]

#
df <- df %>%
  mutate(readm_death_trans = case_when(
    readm_event == 0 ~ "Alive",
    readm_event == 1 ~ "Readmission",
    readm_event == 2 ~ "Dead",
    TRUE ~ as.character(readm_event)
  ))

df$readm_death_trans <- factor(df$readm_death_trans,
                               levels = c("Readmission", "Dead", "Alive"))

# Revise labels
set_labels <- function(df, labels) {
  for (col in names(labels)) {
    df <- labelled::set_variable_labels(df, !!sym(col) := labels[[col]])
  }
  return(df)
}

labels <- list(
  sex = "Sex",
  age_new = "Age (years)",
  age_group = "Age group",
  country_region = "Region",
  country_income = "World Bank income status",
  
  hpd_admreason = "Primary admission reason",
  
  hai_have_med_device___pcv = "Have medical device (PIVC)",
  hai_have_med_device___cvc = "Have medical device (CVC)",
  hai_have_med_device___iuc = "Have medical device (IUC)",
  hai_have_med_device___vent = "Have medical device (VENT)",
  med_device = "Usage of medical devices",
  
  length_before_onset = "Time duration between hospital admission and infection onset (days)",
  icu_hd_ap = "ICU/HD admissions of enrolment",
  readm_ap = "Have readmissions",
  
  first_los = "Length of hospital stay (days)",
  first28_icu = "Length of ICU stay (days)",
  first28_mv = "Duration of mechanical ventilation (days)",
  
  GN_car_r = "Carbapenem-resistant Gram-negative bacteria (GNB)",
  GN_thir_r = "Third-generation cephalosporins-resistant and Carbapenem-susceptible GNB",
  GN_sus = "Carbapenem-susceptible and Third-generation cephalosporins-susceptible GNB",
  GN_car_thir = "Gram-negative bacteria",
  pathogen_combined_types = "Types of pathogens",
  
  comorbidities_Chalson = "Charlson comorbidity index",
  sofa_score = "SOFA score",
  severity_score_scale = "Severity score of disease",
  fbis_score = "FBIS score",
  pitt_score = "PITT score",
  qpitt_score = "qPITT score",
  eq_5d_3l = "EQ-5D-3L score"
)

df <- set_labels(df, labels)

# Create the summary table
table <- df %>%
  select(age_new, sex, 
         country_region, country_income,
         hpd_admreason, 
         comorbidities_Chalson, severity_score_scale, med_device, 
         icu_hd_ap, 
         pathogen_combined_types, GN_car_thir, 
         infection_types, readm_death_trans) %>%
  tbl_strata(
    strata = readm_death_trans,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = infection_types,
                  missing = "ifany",
                  type = list(
                    all_continuous() ~ "continuous2",
                    severity_score_scale ~ "continuous2",
                    comorbidities_Chalson ~ "continuous2",
                    med_device ~ "categorical",
                    icu_hd_ap ~ "categorical"
                  ),
                  statistic = list(
                    age_new ~ "{mean} ± {sd}",
                    comorbidities_Chalson ~ "{median} [{p25}, {p75}]",
                    severity_score_scale ~ "{median} [{p25}, {p75}]"
                  ),
                  digits = all_continuous() ~ 0,
                  missing_text = "Missing"),
    .header = "**{strata}, N = {format(n, big.mark = ',')}**"
  ) %>%
  modify_header(
    update = list(
      label ~ "**Characteristics**",
      all_stat_cols() ~ gt::html("**{level}**<br>N = {format(n, big.mark = ',')}")
    )
  ) %>%
  bold_labels() %>%
  modify_footnote(update = everything() ~ NA) %>%
  modify_table_styling(
    columns = label,
    rows = label == "Severity score of disease",
    footnote = "Standardized severity score included qSOFA for adults, sepsis six recognition features for children, and general WHO severity signs for neonates.") %>% 
  modify_table_styling(
    columns = label,
    rows = label == "Usage of medical devices",
    footnote = "Medical devices involved one or more of the following: peripheral intravenous catheter, central venous catheter, indwelling urinary catheter, and intubation/machine ventilator.") %>% 
  as_gt() %>%
  tab_options(footnotes.marks = c("*", "†")) %>%
  tab_style(
    style = cell_text(align = "left", v_align = "middle"),
    locations = cells_column_labels(columns = 1:5)
  ) 

# Customize the headers to split lines
gt_table <- table %>%
  tab_options(
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(2.5),
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2.5),
    table_body.hlines.color = "white",
    table_body.hlines.width = px(0),
    table.border.bottom.color = "white",
    table.border.bottom.width = px(0),
    data_row.padding = px(0),
    footnotes.padding = px(0)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(2.5)
    ),
    locations = cells_body(
      rows = nrow(table$`_data`)
    )
  ) %>%
  cols_width(
    1:5 ~ px(280),
    6 ~ px(90),
    7 ~ px(100),
    8 ~ px(130),
    10 ~ px(90),
    11 ~ px(100),
    12 ~ px(130),
    14 ~ px(90),
    15 ~ px(100),
    16 ~ px(130)
  ) %>%
  tab_style(
    style = cell_text(
      font = "Times New Roman",
      size = px(14)
    ),
    locations = list(
      cells_title(groups = "title"),
      cells_title(groups = "subtitle"),
      cells_column_labels(),
      cells_body()
    )
  ) %>%
  tab_style(
    style = cell_text(
      font = "Times New Roman",
      size = px(12)
    ),
    locations = cells_footnotes()
  ) %>%
  tab_style(
    style = cell_text(
      font = "Times New Roman",   
      size = px(14),             
      weight = "bold"           
    ),
    locations = cells_column_spanners(spanners = tidyselect::matches("Readmission|Dead|Alive"))
  )

# Print the table
gt_table

# Save
gtsave(data = gt_table, filename = "output/table/table_summary.html")

# ----------------------------------------------------------------------------
# Combine small leves of hpd_admreason, remain top 5
reason_counts <- sort(table(df$hpd_admreason), decreasing = TRUE)
small_levels <- names(which(table(df$hpd_admreason) <= 50))
# "Connective tissue disease"      "Dermatological disease"        
# "Endocrine / Metabolic disorder" "Genitourinary disorder"        
# "Gynaecological disorder"        "Undetermined" 
df$hpd_admreason <- as.character(df$hpd_admreason)
df$hpd_admreason[df$hpd_admreason %in% small_levels] <- "Others"
df$hpd_admreason <- as.factor(df$hpd_admreason)
df$hpd_admreason <- factor(df$hpd_admreason, 
                           levels = c(setdiff(levels(df$hpd_admreason), "Others"), "Others"))
df$hpd_admreason <- relevel(factor(df$hpd_admreason), ref = "Infectious disease")

# Set reference
df$icu_hd_ap <- relevel(factor(df$icu_hd_ap), ref = "No")
df$GN_car_thir <- relevel(factor(df$GN_car_thir), 
                          ref = "Third-generation cephalosporins-susceptible")

# Save
saveRDS(df, "data/clean data_V2/readmission60_data.RData")