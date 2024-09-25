#Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr,
                 magrittr,
                 survival,
                 survminer,
                 gt,
                 purrr,
                 extrafont)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load fonts
loadfonts()

# Combine
res1 <- readRDS("data/att28_table_univariable.RData")
res2 <- readRDS("data/att28_table_multi_1.RData")
res3 <- readRDS("data/att28_table_multi_2.RData")
res4 <- readRDS("data/att28_table_multi_3.RData")


res <- cbind.data.frame(res1, res2[,c(2,3)], res3[,c(2,3)], res4[,c(2,3)])

# Set col names
col_names <- as.character(unlist(res[1, ]))
res <- res[-1, ]
rownames(res) <- NULL


colnames(res) <- c("Characteristics", "Crude HR (95%CI)_uni", "P value_uni",
                   "Adjusted HR (95%CI)_multi_1", "P value_multi_1",
                   "Adjusted HR (95%CI)_multi_2", "P value_multi_2",
                   "Adjusted HR (95%CI)_multi_3", "P value_multi_3")

res[res == "NA"] <- ""
# 
process_missing_values <- function(...) {
  row <- list(...)
  for (i in 2:9) {
    if (is.na(row[[i]])) {
      if (i > 1 && (is.na(row[[i-1]]) || row[[i-1]] == "")) {
        row[[i]] <- ""
      } else if (i > 1 && row[[i-1]] == "Ref") {
        row[[i]] <- "—"
      }
    }
  }
  return(row)
}

#
res <- res %>%
  mutate(across(1, ~ ifelse(.x == "NA", "", .x)))%>%
  mutate(across(everything(), as.character)) %>%
  pmap_dfr(process_missing_values)

# Create table
table <- res %>%
  gt() %>%
  sub_missing(
    columns = 1:9,
    missing_text = ""
  ) %>%
  tab_spanner(
    label = md("**<span style = 'color:black;'>Univariable analysis</span><sup>[1]</sup>**"),
    columns = 2:3, 
    id = "univariable_analysis_1"
  ) %>%
  tab_spanner(
    label = md("**<span style = 'color:black;'>Multivariable analysis</span><sup>[1]</sup>**"),
    columns = 4:5, 
    id = "multivariable_analysis_1"
  ) %>%
  tab_spanner(
    label = md("**<span style = 'color:black;'>Multivariable analysis</span><sup>[1]</sup>**"),
    columns = 6:7, 
    id = "multivariable_analysis_2"
  ) %>%
  tab_spanner(
    label = md("**<span style = 'color:black;'>Multivariable analysis</span><sup>[1,2]</sup>**"),
    columns = 8:9, 
    id = "multivariable_analysis_3"
  ) %>%
  cols_label(
    `Crude HR (95%CI)_uni` = md("Crude HR (95%CI)"),
    `P value_uni` = md("*p*"),
    `Adjusted HR (95%CI)_multi_1` = md("Adjusted HR (95%CI)"),
    `P value_multi_1` = md("*p*"),
    `Adjusted HR (95%CI)_multi_2` = md("Adjusted HR (95%CI)"),
    `P value_multi_2` = md("*p*"),
    `Adjusted HR (95%CI)_multi_3` = md("Adjusted HR (95%CI)"),
    `P value_multi_3` = md("*p*")
  ) %>%
  cols_align(
    align = "center",
    columns = 2:9
  ) %>%
  cols_align(
    align = "left",
    columns = 1
  ) %>%
  tab_style(
    style = cell_text(align = "left", v_align = "middle", font = "Times New Roman"),
    locations = cells_column_labels(columns = 1)
  ) %>%
  cols_width(
    1 ~ px(260),
    2 ~ px(150),
    3 ~ px(90),
    4 ~ px(150),
    5 ~ px(90),
    6 ~ px(150),
    7 ~ px(90),
    8 ~ px(150),
    9 ~ px(90)
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
    source_notes.padding = px(0)
  ) %>%
  tab_style(
    style = cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(2.5)
    ),
    locations = cells_body(
      rows = nrow(res)
    )
  ) %>%
  tab_source_note(
    source_note = "Note: [1] stratified by infection types, [2] final model."
  ) %>%
  tab_source_note(
    source_note = "Abbreviations: HR = Hazard Ratio, CI = Confidence Interval."
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", font = "Times New Roman"),
    locations = cells_body(
      rows = which(rowSums(is.na(res[, 2:9]) | res[, 2:9] == "", na.rm = TRUE) == 8),
      columns = 1
    )
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
    locations = cells_source_notes()
  ) %>%
  tab_style(
    style = cell_text(
      font = "Times New Roman",   
      size = px(14),             
      weight = "bold"           
    ),
    locations = cells_column_spanners(spanners = tidyselect::matches("analysis"))
  ) %>%
  tab_style(
    style = list(cell_text(weight = "bold")),
    locations = cells_column_labels(everything())
  )

print(table)

#
gtsave(data = table, filename = "output/table/table_analysis.html")
