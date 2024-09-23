# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magrittr,
                 dplyr,
                 purrr,
                 labelled,
                 gtsummary,
                 gt,
                 Hmisc,
                 extrafont)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/att_first28_missing.RData")

# Load fonts
loadfonts()

# Enterobacterales
df <- as.data.frame(df[[2]])

df$ris_Carbapenems <- as.factor(ifelse(df$ris_Carbapenems == 1, 1,
                                       ifelse(df$ris_Carbapenems %in% c(2, 3), 0,
                                              ifelse(df$ris_Carbapenems %in% c(4, 5), 2, NA))))

df$ris_Carbapenems <- factor(df$ris_Carbapenems, 
                             levels = c(1, 0, 2),
                             labels = c("Carbapenem-resistant", 
                                        "Carbapenem-susceptible",
                                        "Unknown (or missing)"))

label(df$ris_Carbapenems) <- "Enterobacterales(carbapenem sensitivity)"



df <- df %>%
  rename(ris_third = `ris_Third-generation cephalosporins`)

df$ris_third <- as.factor(ifelse(df$ris_third == 1, 1,
                                 ifelse(df$ris_third %in% c(2, 3), 0,
                                        ifelse(df$ris_third %in% c(4, 5), 2, NA))))

df$ris_third <- factor(df$ris_third, 
                       levels = c(1, 0, 2),
                       labels = c("Third-generation cephalosporin-resistant", 
                                  "Third-generation cephalosporin-susceptible",
                                  "Unknown (or missing)"))

label(df$ris_third) <- "Enterobacterales (3GC sensitivity)"


# Event
df$event <- ifelse(df$event == 1, "Dead", "Alive")
df$event <- factor(df$event, levels = c("Dead", "Alive"))

# Revise "Gram-negative bacteria" to "Monomicrobial"
df$pathogen_combined_types <- factor(df$pathogen_combined_types, 
                                     levels = c("Gram-negative bacteria", "Polymicrobial"),
                                     labels = c("Monomicrobial", "Polymicrobial"))

# Modify the label
label(df$pathogen_combined_types) <- "Types of pathogens"

# Create the summary table
table <- df %>%
  select(age_new, sex, 
         country_region, country_income, 
         hpd_admreason, 
         comorbidities_Chalson, severity_score_scale,
         icu_hd_ap, 
         pathogen_combined_types, ris_Carbapenems, ris_third,
         infection_types, event) %>%
  tbl_strata(
    strata = event,
    .tbl_fun =
      ~ .x %>%
      tbl_summary(by = infection_types,
                  missing = "ifany",
                  type = list(
                    all_continuous() ~ "continuous2",
                    severity_score_scale ~ "continuous2",
                    comorbidities_Chalson ~ "continuous2",
                    icu_hd_ap ~ "categorical",
                    sex ~ "categorical"
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
  as_gt() %>%
  tab_options(footnotes.marks = c("*")) %>%
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
    1:5 ~ px(270),
    6 ~ px(90),
    7 ~ px(100),
    8 ~ px(110),
    10 ~ px(90),
    11 ~ px(100),
    12 ~ px(110)
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
    locations = cells_column_spanners(spanners = tidyselect::matches("Dead|Alive"))
  )

# Print the table
gt_table

# Save
gtsave(data = gt_table, filename = "output/table/table_summary_missing.html")
