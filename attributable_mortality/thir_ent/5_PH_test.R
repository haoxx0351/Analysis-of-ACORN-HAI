# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr,
                 survival,
                 gt,
                 extrafont)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/att_first28.RData")

# Load fonts
loadfonts()

# Enterobacterales, third-generation cephalosporin-resistant
df <- as.data.frame(df[[3]])

df <- df %>%
  rename(ris_third = `ris_Third-generation cephalosporins`)

df$ris_third <- as.factor(ifelse(df$ris_third == 1, 1, 0))


# Delete med_device  coefficient may be infinite. 
# Test the proportional hazards assumption for select MODEL
model_cox <- coxph(Surv(time, event) ~ age_new + sex + 
                     country_region + country_income +
                     hpd_admreason + comorbidities_Chalson + 
                     severity_score_scale +
                     icu_hd_ap + 
                     pathogen_combined_types + ris_third +
                     strata(infection_types), 
                   data = df)

# Perform the Cox.zph test
cox_zph_result <- cox.zph(model_cox)

# Convert the Cox.zph result to a data frame
cox_zph_df <- as.data.frame(cox_zph_result$table)

cox_zph_df$Variables <- c("Age", "Sex", 
                         "Region", "World Bank income status",
                         "Primary admission reason",
                         "Charlson comorbidity index",
                         "Severity score of disease",
                         "ICU/HD admissions of enrolment",
                         "Types of pathogens",
                         "Enterobacterales",
                         "GLOBAL")

rownames(cox_zph_df) <- NULL

# Reorder columns to match the desired output
cox_zph_df <- cox_zph_df[,c(4,1:3)]

cox_zph_df$p <- ifelse(cox_zph_df$p < 0.001, "<0.001", 
                       ifelse(cox_zph_df$p < 0.01, "<0.01", 
                              ifelse(cox_zph_df$p < 0.05, "<0.05", sprintf("%.3f", cox_zph_df$p))))

table <- cox_zph_df %>%
  gt() %>%
  fmt_number(columns=2,decimals= 3) %>%
  cols_label(
    chisq = md("Chi-Squared Statistic"),
    df = md("Degrees of freedom"),
    p = md("*p*")) %>%
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
      rows = nrow(cox_zph_df)
    )
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_body(
      rows = nrow(cox_zph_df)
    )
  )%>%
  cols_align(
    align = "center",
    columns = 2:4
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

print(table)

# Save
gtsave(data = table, filename = "output/table/cox_zph_att28_death.html")
