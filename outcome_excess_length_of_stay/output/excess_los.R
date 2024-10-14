# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magrittr,
                 dplyr,
                 survival,
                 boot,
                 gt,
                 extrafont)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/clean_data_RData/data_table_index_new.RData")
organism <- readRDS("data/clean_data_RData/ast_all_index.RData")

# Load fonts
loadfonts()

# Hospital admission and transfer date
df$combined_adm_date <- ifelse(!is.na(df$hpd_hosp_date), df$hpd_hosp_date, df$hpd_adm_date)
df$combined_adm_date <- as.Date(df$combined_adm_date)
df$ho_discharge_date <- as.Date(df$ho_discharge_date, format = "%Y/%m/%d")
df$d28_date <- as.Date(df$d28_date, format = "%Y/%m/%d")

# Create status variables for state transitions
df_alive <- df %>%
  filter(ho_dischargestatus %in% c("ALIVE", "LAMA") | d28_status != "DEAD") %>%
  mutate(time_to_discharge = as.numeric(ho_discharge_date - combined_adm_date),
         time_to_death_without_discharge = NA,
         time_to_death_after_discharge = NA,
         event_discharge = 1,
         event_dead = 0)

df_dead <- df %>%
  filter(ho_dischargestatus == "DEAD") %>%
  mutate(time_to_death_without_discharge = as.numeric(ho_discharge_date - combined_adm_date),
         time_to_discharge = NA,
         time_to_death_after_discharge = NA,
         event_discharge = 0,
         event_dead = 2)

df_dis_dead <- df %>%
  filter(ho_dischargestatus == "MORIBUND" & d28_status == "DEAD") %>%
  mutate(time_to_discharge = as.numeric(ho_discharge_date - combined_adm_date),
         time_to_death_after_discharge = as.numeric(d28_date - combined_adm_date),
         time_to_death_without_discharge = NA,
         event_discharge = 1,
         event_dead = 2)

df_used <- bind_rows(df_alive, df_dead, df_dis_dead)

# Remove rows with non-positive time values
positive_time_rows <- apply(df_used[, c("time_to_discharge", 
                                        "time_to_death_without_discharge", 
                                        "time_to_death_after_discharge")], 1, function(x) all(x[!is.na(x)] > 0))

rows_with_at_least_one_non_na_time <- apply(df_used[, c("time_to_discharge", 
                                                        "time_to_death_without_discharge", 
                                                        "time_to_death_after_discharge")], 1, function(x) any(!is.na(x)))

rows_with_at_least_one_non_na_event <- apply(df_used[, c("event_discharge", "event_dead")], 1, function(x) any(!is.na(x)))

valid_rows <- positive_time_rows & rows_with_at_least_one_non_na_time & rows_with_at_least_one_non_na_event

df_used_new <- df_used[valid_rows, ]

# Prepare resistance data
car_aci <- organism %>%
  filter(org_names_all == "Acinetobacter" & ris_Carbapenems %in% 1:3) %>%
  group_by(recordid) %>%
  arrange(ris_Carbapenems) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(ast_ris = ifelse(ris_Carbapenems == 1, 1, 0))

car_ent <- organism %>%
  filter(org_names_all %in% c("E. coli", "K. pneumoniae", "Klebsiella", "Enterobacter", "Serratia", "Proteus", "Morganella") & ris_Carbapenems %in% 1:3) %>%
  group_by(recordid) %>%
  arrange(ris_Carbapenems) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(ast_ris = ifelse(ris_Carbapenems == 1, 1, 0))

thir_ceph_ent <- organism %>%
  filter(org_names_all %in% c("E. coli", "K. pneumoniae", "Klebsiella", "Enterobacter", "Serratia", "Proteus", "Morganella") & `ris_Third-generation cephalosporins` %in% 1:3) %>%
  group_by(recordid) %>%
  arrange(`ris_Third-generation cephalosporins`) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(ast_ris = ifelse(`ris_Third-generation cephalosporins` == 1, 1, 0))

# ---------------
# Join resistance data with df_used_new
car_aci <- inner_join(car_aci, df_used_new, by = "recordid")
car_ent <- inner_join(car_ent, df_used_new, by = "recordid")
thir_ceph_ent <- inner_join(thir_ceph_ent, df_used_new, by = "recordid")



# Define a function to process each data frame
process_data <- function(data) {
  # Initialize an empty data frame to store the multi-state data
  ms_data <- data.frame(recordid = integer(), from = integer(), to = integer(), 
                        Tstart = numeric(), Tstop = numeric(), status = integer(), stringsAsFactors = FALSE)
  
  # Add transitions for State 0 -> State 2 and State 0 -> State 3
  df_state0 <- data %>%
    filter(ast_ris == 0)
  ms_data_state0 <- bind_rows(
    df_state0 %>%
      mutate(from = 0, to = 2, Tstart = 0, Tstop = time_to_discharge, status = event_discharge) %>%
      select(recordid, from, to, Tstart, Tstop, status),
    df_state0 %>%
      mutate(from = 0, to = 3, Tstart = 0, Tstop = time_to_death_without_discharge, status = event_dead) %>%
      select(recordid, from, to, Tstart, Tstop, status)
  )
  
  # Add transitions for State 1 -> State 2 and State 1 -> State 3
  df_state1 <- data %>%
    filter(ast_ris == 1)
  ms_data_state1 <- bind_rows(
    df_state1 %>%
      mutate(from = 1, to = 2, Tstart = 0, Tstop = time_to_discharge, status = event_discharge) %>%
      select(recordid, from, to, Tstart, Tstop, status),
    df_state1 %>%
      mutate(from = 1, to = 3, Tstart = 0, Tstop = time_to_death_without_discharge, status = event_dead) %>%
      select(recordid, from, to, Tstart, Tstop, status)
  )
  
  # Add transitions for State 2 -> State 3
  df_state2 <- data %>%
    filter(!is.na(time_to_death_after_discharge))
  ms_data_state2 <- df_state2 %>%
    mutate(from = 2, to = 3, Tstart = time_to_discharge, Tstop = time_to_death_after_discharge, status = event_dead) %>%
    select(recordid, from, to, Tstart, Tstop, status)
  
  # Combine results for the current data frame
  ms_data <- bind_rows(ms_data_state0, ms_data_state1, ms_data_state2)
  
  # Remove rows where status is 0
  ms_data <- ms_data %>% filter(status != 0)
  
  # Clean recordid
  ms_data <- ms_data %>%
    mutate(recordid = gsub("(_VAP|_BSI)$", "", recordid)) %>%
    distinct()
  
  # Handle duplicates
  duplicate_rows <- ms_data %>%
    group_by(recordid) %>%
    filter(n() > 1)
  
  non_duplicate_rows <- ms_data %>%
    group_by(recordid) %>%
    filter(n() == 1)
  
  ms_data_cleaned <- duplicate_rows %>%
    filter(!is.na(Tstop)) %>%
    group_by(recordid, from, Tstart, Tstop) %>%
    filter(if_else(status == 1, TRUE, !any(status == 1))) %>%
    ungroup()
  
  ms_data_final <- bind_rows(ms_data_cleaned, non_duplicate_rows)
  
  # Ensure Tstop > Tstart
  ms_data_final <- ms_data_final %>%
    mutate(Tstop = pmin(Tstop, 30)) %>%
    filter(Tstart < Tstop)
  
  # Add baseline variables (assuming df is available in your environment)
  add_data <- df %>%
    select(recordid, age_new, sex, 
           country_region, country_income, 
           hpd_admreason, comorbidities_Chalson, severity_score_scale,
           icu_hd_ap) %>%
    mutate(recordid = gsub("(_VAP|_BSI)$", "", recordid)) %>%
    distinct(recordid, .keep_all = TRUE)
  
  ms_data_final <- left_join(ms_data_final, add_data, by = "recordid")
  ms_data_final$status <- ms_data_final$status + 1
  
  return(ms_data_final)
}

# Process each data frame separately
process_and_analyze <- function(data, name) {
  ms_data_final <- process_data(data)
  
  # Fit Cox models
  cox.01 <- coxph(Surv(Tstart, Tstop, to == 2) ~ age_new + sex + country_region + country_income +
                    comorbidities_Chalson + severity_score_scale + icu_hd_ap + cluster(recordid),
                  data = ms_data_final, subset = from == 0) 
  
  cox.02 <- coxph(Surv(Tstart, Tstop, to == 2) ~ age_new + sex + country_region + country_income +
                    comorbidities_Chalson + severity_score_scale + icu_hd_ap + cluster(recordid),
                  data = ms_data_final, subset = from == 1) 
  
  pred_01 <- survfit(cox.01)
  pred_02 <- survfit(cox.02)
  
  mean_01 <- summary(pred_01)$table["rmean"]
  mean_02 <- summary(pred_02)$table["rmean"]
  
  # Calculate excess LOS
  mean_excess_los <- mean_02 - mean_01
  
  # Bootstrap function
  boot.excess_los <- function(data, nboot) {
    res <- numeric(nboot)
    
    for (i in seq_len(nboot)) {
      index <- sample(unique(data$recordid), replace = TRUE)
      dboot <- data %>%
        filter(recordid %in% index) %>%
        mutate(new.id = as.numeric(factor(recordid, levels = unique(recordid))))
      
      tryCatch({
        # Fit Cox models
        cox.01 <- coxph(Surv(Tstart, Tstop, to == 2) ~ age_new + sex + country_region + country_income +
                          comorbidities_Chalson + severity_score_scale + icu_hd_ap + cluster(recordid), 
                        data = dboot, subset = from == 0, control = coxph.control(iter.max = 100, eps = 1e-8)) 
        cox.02 <- coxph(Surv(Tstart, Tstop, to == 2) ~ age_new + sex + country_region + country_income +
                          comorbidities_Chalson + severity_score_scale + icu_hd_ap + cluster(recordid), 
                        data = dboot, subset = from == 1, control = coxph.control(iter.max = 100, eps = 1e-8)) 
        
        pred.01 <- survfit(cox.01)
        pred.02 <- survfit(cox.02)
        
        mean_01 <- summary(pred.01)$table["rmean"]
        mean_02 <- summary(pred.02)$table["rmean"]
        
        res[i] <- mean_02 - mean_01
      }, error = function(e) {
        res[i] <- NA
      })
    }
    
    res <- res[!is.na(res)]
    return(res)
  }
  
  # Perform bootstrapping with 1000 iterations
  set.seed(1234)
  nboot <- 1000
  boot_results <- boot.excess_los(ms_data_final, nboot)
  
  # Calculate the 95% CI
  ci <- quantile(boot_results, probs = c(0.025, 0.975))
  
  list(mean_excess_los = mean_excess_los, ci = ci, name = name)
}

# Run analysis for each data frame
results_car_aci <- process_and_analyze(car_aci, "car_aci")
results_car_ent <- process_and_analyze(car_ent, "car_ent")
results_thir_ceph_ent <- process_and_analyze(thir_ceph_ent, "thir_ceph_ent")


# Define the results from each dataset
mean_excess_los_car_aci <- as.integer(results_car_aci$mean_excess_los)
ci_car_aci <- as.integer(round(results_car_aci$ci))
mean_excess_los_car_ent <- as.integer(results_car_ent$mean_excess_los)
ci_car_ent <- as.integer(round(results_car_ent$ci))
mean_excess_los_thir_ceph_ent <- as.integer(results_thir_ceph_ent$mean_excess_los)
ci_thir_ceph_ent <- as.integer(round(results_thir_ceph_ent$ci))

# Combine results into a data frame
results_combined <- data.frame(
  pathogen = c("Carbapenem-resistant Acinetobacter spp.",
               "Third-generation cephalosporin-resistant Enterobacterales",
               "Carbapenem-resistant Enterobacterales"
               ),
  excess = c(paste0(mean_excess_los_car_aci, " (", ci_car_aci[1], ",", ci_car_aci[2], ")"),
             paste0(mean_excess_los_thir_ceph_ent, " (",ci_thir_ceph_ent[1],  ",", ci_thir_ceph_ent[2], ")"),
             paste0(mean_excess_los_car_ent, " (",ci_car_ent[1],  ",", ci_car_ent[2], ")")
            ),
  stringsAsFactors = FALSE
)

# Print the combined results
print(results_combined)

# Table
table <- results_combined %>%
  gt() %>%
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
    data_row.padding = px(0),
    footnotes.padding = px(0),
    source_notes.padding = px(0)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(2.5)
    ),
    locations = cells_body(
      rows = nrow(results_combined)
    )
  ) %>%
  cols_label(
    `pathogen` = md("Critical priority pathogens"),
    `excess` = md("Excess LOS"),
  ) %>%
  cols_align(
    align = "center",
    columns = 2
  ) %>%
  cols_align(
    align = "left",
    columns = 1
  ) %>%
  tab_style(
    style = cell_text(align = "left", v_align = "middle"),
    locations = cells_column_labels(columns = 1)
  ) %>%
  tab_footnote(
    footnote = "Mean (95%CI)",
    locations = cells_column_labels(columns = "excess")
  ) %>%
  tab_options(footnotes.marks = "†") %>%
  tab_source_note(
    source_note = "Abbreviations: LOS = Length of stay, CI = Confidence Interval."
  )  %>%
  cols_width(
    1 ~ px(430),
    2 ~ px(110)
  ) %>%
  tab_style(
    style = cell_text(
      font = c("Poppins"),
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
      font = c("Poppins"),
      size = px(12)
    ),
    locations = cells_footnotes()
  ) %>%
  tab_style(
    style = cell_text(
      font = "Poppins",
      size = px(12)
    ),
    locations = cells_source_notes()
  ) 


print(table)

# Save
gtsave(data = table, filename = "output/table/excess_LOS.html")

