# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magrittr,
                 dplyr,
                 labelled)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/clean_data_RData/data_table_index_new.RData")
organism <- readRDS("data/clean_data_RData/ast_all_index.RData")

# ------------------------------------------------------------
# 2024-05 Critical priority:
# Acinetobacter baumannii, carbapenem-resistant;
# Enterobacterales, third-generation cephalosporin-resistant; 
# Enterobacterales, carbapenem-resistant;
# (E.coli, Klebsiella, or Enterobacter)
#####
organism <- organism %>% 
  select(recordid, org_names_all, starts_with("ris"))

# Acinetobacter, carbapenem-resistant
car_aci <- organism[which(organism$org_names_all == "Acinetobacter"), c("recordid", "ris_Carbapenems")] %>%
  group_by(recordid) %>%
  arrange(ris_Carbapenems) %>%  
  slice(1) %>%
  ungroup()

# Enterobacterales, carbapenem-resistant
# Enterobacterales, third-generation cephalosporin-resistant
car_ent <- organism[which(organism$org_names_all %in% c("E. coli", "K. pneumoniae", "Klebsiella", "Enterobacter", "Serratia", "Proteus", "Morganella")), c("recordid", "ris_Carbapenems", "ris_Third-generation cephalosporins")] %>%
  group_by(recordid) %>%
  arrange(ris_Carbapenems, `ris_Third-generation cephalosporins`) %>%  
  slice(1) %>%
  ungroup()

###
# Define columns to check for NA values
columns_to_check <- c("first28_patient_days", "first28_death")

# Define a function to merge and clean the data frames
merge_and_clean <- function(main_df, ref_df, by_col, columns_to_check) {
  merged_df <- left_join(main_df, ref_df, by = by_col)
  cleaned_df <- merged_df[complete.cases(merged_df[, columns_to_check]), ]
  return(cleaned_df)
}

# Define a function to process each data frame
process_dataframe <- function(df) {
  
  # Delete rows with first28_patient_days <= 0
  df <- df[df$first28_patient_days > 0, ]
  
  # Combine small levels of hpd_admreason
  reason_counts <- sort(table(df$hpd_admreason), decreasing = TRUE)
  small_levels <- names(which(table(df$hpd_admreason) <= 50))
  df$hpd_admreason <- as.character(df$hpd_admreason)
  df$hpd_admreason[df$hpd_admreason %in% small_levels] <- "Others"
  df$hpd_admreason <- as.factor(df$hpd_admreason)
  df$hpd_admreason <- factor(df$hpd_admreason, 
                             levels = c(setdiff(levels(df$hpd_admreason), "Others"), "Others"))
  df$hpd_admreason <- relevel(factor(df$hpd_admreason), ref = "Infectious disease")
  
  # Relevel factors
  df$pathogen_combined_types <- droplevels(df$pathogen_combined_types)
  
  # Add time and event columns
  df$time <- df$first28_patient_days
  df$event <- df$first28_death
  
  set_labels <- function(df, labels) {
    for (col in names(labels)) {
      df <- labelled::set_variable_labels(df, !!sym(col) := labels[[col]])
    }
    return(df)
  }
  
  labels <- list(
    hpd_admreason = "Primary admission reason",
    pathogen_combined_types = "Type of pathogens"
  )
  
  df <- set_labels(df, labels)
  
  return(df)
}

# Use merge_and_clean function to process each data frame
car_aci_clean <- merge_and_clean(car_aci, df, "recordid", columns_to_check)
car_ent_clean <- merge_and_clean(car_ent, df, "recordid", columns_to_check)

# Apply the process_dataframe function to each cleaned data frame
car_aci_clean <- process_dataframe(car_aci_clean)
car_ent_clean <- process_dataframe(car_ent_clean)

# Save
df_list <- list(car_aci_clean, car_ent_clean)
saveRDS(df_list, "data/att_first28_table.RData")

###
# Define a function to process each data frame
process_dataframe_add <- function(df) {

  set_labels <- function(df, labels) {
    for (col in names(labels)) {
      df <- labelled::set_variable_labels(df, !!sym(col) := labels[[col]])
    }
    return(df)
  }
  
  df$icu_hd_ap <- relevel(df$icu_hd_ap, ref = "No")
  df$med_device <- relevel(df$med_device, ref = "No")
  
  labels <- list(
    icu_hd_ap = "ICU/HD admissions of enrolment",
    med_device = "Usage of medical devices"
  )
  
  df <- set_labels(df, labels)
  
  return(df)
}

#
car_aci_clean_add <- process_dataframe_add(car_aci_clean)
car_ent_clean_add <- process_dataframe_add(car_ent_clean)

#
df_list_add <- list(car_aci_clean_add, car_ent_clean_add)

# Save
saveRDS(df_list_add, "data/att_first28_missing.RData")
