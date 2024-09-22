# Clean
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(openxlsx,
                 magrittr,
                 dplyr,
                 purrr,
                 labelled,
                 gtsummary,
                 gt,
                 htmltools,
                 extrafont)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/clean data/baseline_outcomes_index.RData")
epi <- readRDS("data/clean data/vap_bsi_index.RData")
ast <- readRDS("data/clean data/ast_all_index.RData")
hos_reason <- read.xlsx("data/hos_reasons.xlsx")

# Load fonts
loadfonts()

#####
# Delete NA in infection types
df[df == ""] <- NA
df <- df[-which(is.na(df$infection_types)),]

## Carbapenem-resistant, Third-generation cephalosporins GNB
df_add <- ast %>% 
  select(recordid, infection_types, Class, 
         starts_with("ris_"),
         pathogen_combined_types)

# GN_car_r, GN_thir_r as binary
df_add <- df_add %>%
  mutate(
    GN_car_r = case_when(
      ris_Carbapenems == 1 & Class == "GNB" ~ 1,
      is.na(ris_Carbapenems) | is.na(Class) ~ NA_real_,
      TRUE ~ 0
    ),
    GN_thir_r = case_when(
      `ris_Third-generation cephalosporins` == 1 & ris_Carbapenems %in% c(2,3) & Class == "GNB" ~ 1,
      is.na(`ris_Third-generation cephalosporins`) | is.na(Class) ~ NA_real_,
      TRUE ~ 0
    )
  ) %>% 
  select(recordid, infection_types, pathogen_combined_types, GN_car_r, GN_thir_r) %>% 
  distinct() %>%
  group_by(recordid, infection_types, pathogen_combined_types) %>%
  summarize(
    GN_car_r = ifelse(any(GN_car_r == 1, na.rm = TRUE), 1, ifelse(all(is.na(GN_car_r)), NA, 0)),
    GN_thir_r = ifelse(any(GN_thir_r == 1, na.rm = TRUE), 1, ifelse(all(is.na(GN_thir_r)), NA, 0))
  ) %>%
  ungroup() %>%
  mutate(
    GN_sus = case_when(
      GN_car_r == 1 | GN_thir_r == 1 ~ 0,
      GN_car_r == 0 & GN_thir_r == 0 ~ 1,
      TRUE ~ NA_real_
    ),
    GN_car_thir = case_when(
      GN_car_r == 1 ~ 2,
      GN_thir_r == 1 ~ 1,
      GN_sus == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) 

###
# Medical devices / procedures
df_epi <- bind_rows(
  lapply(epi, function(x) select(x, recordid, infection_types, starts_with("hai_have")))
)
###
df_list <- list(df, df_epi, df_add)
# Convert the `infection_types` column to factor type in all data frames
df_list <- lapply(df_list, function(df) {
  df %>% mutate(infection_types = as.factor(infection_types))
})

df <- reduce(df_list, inner_join, by = c("recordid", "infection_types"))

df <- df %>% 
  group_by(recordid) %>%
  arrange(inf_onset) %>%  
  slice(1) %>%
  ungroup()

df_med_device <- df |>
  select(starts_with("hai_have"))

df_med_device$hai_have_med_device___none <- ifelse(df_med_device$hai_have_med_device___none == 1, 0, 1)

df$med_device <- apply(df_med_device, 1, function(row) {
  if (all(is.na(row))) {
    return(NA)
  } else if (any(row == 1, na.rm = TRUE)) {
    return(1)
  } else {
    return(0)
  }
})

###
# Set variables
df_factor <- c("age_group", "country", "country_region", "country_income",
               "infection_types", "sex", "siteid",
               "hpd_admtype", "hpd_admreason", "med_device",
               "hai_have_med_device___none", "hai_have_med_device___pcv",
               "hai_have_med_device___cvc", "hai_have_med_device___iuc",
               "hai_have_med_device___vent",
               "GN_car_r", "GN_thir_r","GN_sus", "GN_car_thir",
               "pathogen_combined_types",
               "readm_ap", "icu_hd_ap")

df_num <- c("age_raw", "age_new", "comorbidities_Chalson", 
            "first28_icu", "first28_mv", "first_los",
            "readm_ap_1", "icu_hd_ap_1", "mv_ap_1",
            "severity_score_scale", 
            "fbis_score","eq_5d_3l", "pitt_score", "qpitt_score")

df_factor_index <- which(names(df) %in% df_factor)
df_num_index <- which(names(df) %in% df_num)

#
df <- as.data.frame(df)
for (i in df_factor_index){df[,i] = as.factor(df[,i])}
for (i in df_num_index){df[,i] = as.numeric(df[,i])}

###
# Split Primary admission reason
split_condition <- function(condition) {
  # Find the first " "
  pos <- regexpr(" ", condition)
  # 
  code <- substr(condition, 1, pos - 1)
  description <- substr(condition, pos + 1, nchar(condition))
  return(c(code, description))
}

#
split_hos <- t(sapply(hos_reason$Primary.admission.reason, split_condition))

# 
split_hos_reason <- data.frame(abb = split_hos[, 1], 
                               names = split_hos[, 2], 
                               stringsAsFactors = FALSE)

## Set new levels for hpd_admreason
new_levels <- c(setdiff(levels(df$hpd_admreason), "OTH"), "OTH")
df$hpd_admreason <- factor(df$hpd_admreason, levels = new_levels)
level_admreason <- levels(df$hpd_admreason)

# Match new names
match_indices <- match(level_admreason, split_hos_reason$abb)
#
new_level_admreason <- split_hos_reason$names[match_indices]
#
df$hpd_admreason <- factor(df$hpd_admreason, levels = levels(df$hpd_admreason), labels = seq_along(levels(df$hpd_admreason)))

###
df <- within(df, {c(
  infection_types <- factor(infection_types, 
                            levels = c("VAP",
                                       "Hospital-acquired BSI",
                                       "Healthcare-associated BSI")),
  
  sex <- factor(sex, levels = c("F", "M"), labels = c("Female", "Male")),
  
  age_group <- factor(age_group, 
                      levels = c(1:3), 
                      labels = c("Adult (Age >= 18 years)",
                                 "Child (Age 1 month - 17 years)",
                                 "Neonate (Age < 28 days)")),
  
  country_income <- factor(country_income, 
                           levels = c("High income", "Upper middle income",
                                      "Lower middle income", "Low income")),
  
  GN_car_r <- factor(GN_car_r, levels = c(1, 0), labels = c("Presence", "Absence")),
  
  GN_thir_r <- factor(GN_thir_r, levels = c(1, 0), labels = c("Presence", "Absence")),
  
  GN_sus <- factor(GN_sus, levels = c(1, 0), labels = c("Presence", "Absence")),
  
  GN_car_thir <- factor(GN_car_thir, levels = c(0, 1, 2), 
                        labels = c("Third-generation cephalosporins-susceptible", 
                                   "Third-generation cephalosporins-resistant",
                                   "Carbapenem-resistant")),
  
  pathogen_combined_types <- factor(pathogen_combined_types,
                                    levels = c("Gram-negative bacteria",
                                               "Gram-positive bacteria",
                                               "Fungi",
                                               "Polymicrobial")),

  hpd_admreason <- factor(hpd_admreason,
                          levels = c(1:17),
                          labels = new_level_admreason),
  
  hai_have_med_device___pcv <- factor(hai_have_med_device___pcv, 
                                      levels = c(1,0), labels = c("Yes", "No")),
  
  hai_have_med_device___cvc <- factor(hai_have_med_device___cvc, 
                                      levels = c(1,0), labels = c("Yes", "No")),
  
  hai_have_med_device___iuc <- factor(hai_have_med_device___iuc, 
                                      levels = c(1,0), labels = c("Yes", "No")),
  
  hai_have_med_device___vent <- factor(hai_have_med_device___vent, 
                                       levels = c(1,0), labels = c("Yes", "No")),
  
  med_device <- factor(med_device, levels = c(1,0), labels = c("Yes", "No")),
  
  icu_hd_ap <- factor(icu_hd_ap, levels = c(1, 2), labels = c("Yes", "No")),
  readm_ap <- factor(readm_ap, levels = c(1, 2), labels = c("Yes", "No"))
  
)})

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

# Filter those were included in both VAP and BSI groups
df_both <- df[grep("(_VAP|_BSI)$", df$recordid), ] %>%
  group_by(infection_types) %>%
  summarise(n = n())

# Table summary
table <- df %>%
  select(age_new, sex, 
         country_region, country_income, hpd_admreason, 
         comorbidities_Chalson, severity_score_scale, med_device, 
         icu_hd_ap, readm_ap, first_los, first28_icu, first28_mv, 
         pathogen_combined_types, infection_types) %>%
  tbl_summary(
    by = infection_types,
    missing = "ifany",
    type = list(
      all_continuous() ~ "continuous2",
      severity_score_scale ~ "continuous2",
      med_device ~ "categorical",
      icu_hd_ap ~ "categorical",
      readm_ap ~ "categorical"
    ),
    statistic = list(
      age_new ~ "{mean} ± {sd}",
      comorbidities_Chalson ~ "{median} [{p25}, {p75}]",
      severity_score_scale ~ "{median} [{p25}, {p75}]",
      first_los ~ "{median} [{p25}, {p75}]",
      first28_icu ~ "{median} [{p25}, {p75}]",
      first28_mv ~ "{median} [{p25}, {p75}]",
      med_device ~ "{n} ({p}%)",
      icu_hd_ap ~ "{n} ({p}%)", 
      readm_ap ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 0,
    missing_text = "Missing"
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
  modify_table_styling(
    columns = label,
    rows = case_when(
      label == "Length of ICU stay (days)" ~ TRUE,
      label == "Duration of mechanical ventilation (days)" ~ TRUE,
      TRUE ~ FALSE
    ),
    footnote = "Follow-up 28 days since the index infection."
  ) %>%
  as_gt() %>%
  tab_footnote(
    footnote = "80 patients had both index VAP and BSI episodes (76 with hospital-acquired BSI and 4 with healthcare-associated BSI).",
    locations = cells_column_labels(
      columns = 6:8
    )
  ) %>%
  tab_options(footnotes.marks = c("*", "†", "#", "‡")) %>%
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
    1:5 ~ px(333),
    6 ~ px(100),
    7 ~ px(180),
    8 ~ px(190)
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
  ) %>%
  tab_style(
    style = cell_text(
      font = c("Times New Roman"),
      size = px(12)
    ),
    locations = cells_footnotes()
  )

# Print the table
gt_table

# Save
gtsave(gt_table, filename = "output/table/table_baseline.html")
saveRDS(df, "data/clean data/data_table_index_new.RData")
