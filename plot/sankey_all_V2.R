# Clean
rm(list = ls())

# Load packages
# devtools::install_github("davidsjoberg/ggsankey")
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magrittr,
                 dplyr,
                 tidyr,
                 purrr,
                 ggplot2,
                 extrafont,
                 ggsankey,
                 RColorBrewer,
                 grid,
                 gridExtra,
                 patchwork,
                 Cairo)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df_data <- readRDS("data/clean data/anti_treat_index.RData")
df_baseline <- readRDS("data/clean data/baseline_outcomes_index.RData")

# Load fonts
loadfonts() 

# Delete Azoles
df_data <- df_data[-which(df_data$anti_group == "Azoles"),]

# Outcomes
df_baseline <- df_baseline %>% 
  select(recordid, ho_discharge_date, mortality_date) %>% 
  mutate(outcomes = case_when(
    is.na(ho_discharge_date) & is.na(mortality_date) ~ "Missing",
    TRUE ~ "Discharged/Died"
  ))

# emp data
emp_used <- left_join(df_data, df_baseline, by = "recordid") %>%
  select(recordid, anti_group, aci_car, ent_thir, ent_car, emp) %>%
  mutate(emp = ifelse(is.na(emp), 0, emp)) %>%  # Replace NA in emp with 0
  mutate(anti_group = case_when(
    emp == 0 ~ "None",
    emp == 1 & is.na(anti_group) ~ "Missing",
    TRUE ~ anti_group
  ))

emp_used <- emp_used %>%
  group_by(recordid) %>%
  filter(!(emp == 0 & any(emp == 1))) %>%  
  group_by(recordid, anti_group) %>%  
  slice(1)  
  


# def data 
def_used <- left_join(df_data, df_baseline, by = "recordid") %>%
  select(recordid, anti_group, aci_car, ent_thir, ent_car, def, outcomes) %>%
  mutate(def = ifelse(is.na(def), 0, def)) %>%
  mutate(
    anti_group = case_when(
      is.na(anti_group) ~ "Missing",
      def == 0 ~ outcomes,
      TRUE ~ anti_group  
    )
  )


def_used <- def_used %>%
  group_by(recordid) %>%
  filter(!(def == 0 & any(def == 1))) %>%  
  group_by(recordid, anti_group) %>%  
  slice(1)                                 

###
# Critical priority:
# Acinetobacter baumannii, carbapenem-resistant
# Enterobacterales, third-generation cephalosporin-resistant 
# Enterobacterales, carbapenem-resistant
aci_car_id <- df_data$recordid[which(df_data$aci_car == 1)]
ent_thir_id <- df_data$recordid[which(df_data$ent_thir == 1)]
ent_car_id <- df_data$recordid[which(df_data$ent_car == 1)]

# Combine as a list
id_list <- list(aci_car_id, ent_thir_id, ent_car_id)
id_list_names <- c("aci_car", "ent_thir", "ent_car")

df <- list(emp_used, def_used)
df_names <- c("emp_used", "def_used")

duplicate_combinations <- list()
combination_info <- list()

system.time({
  counter <- 1
  for (i in seq_along(df)) {
    for (j in seq_along(id_list)) {
      df_used <- df[[i]]
      df_used <- df_used[which(df_used$recordid %in% id_list[[j]]), ]
      
      df_used_list <- df_used[, c("recordid", "anti_group")] %>% 
        distinct()
      
      # Group by recordid and summarize unique anti_group combinations
      duplicate_combinations[[counter]] <- df_used_list %>%
        group_by(recordid) %>%
        summarise(unique_anti_groups = toString(sort(unique(anti_group))))
      
      # Store the combination information with descriptive names
      combination_info[[counter]] <- list(df_name = df_names[i], id_list_name = id_list_names[j])
      
      counter <- counter + 1
    }
  }
})

# Reorder anti groups
reorder_groups <- function(group) {
  priority <- c("Polymyxins", "Carbapenems", "Beta-lactam/Beta-lactamase", 
                "Aminoglycosides", "Third-generation cephalosporins", "Glycylcyclines", "Missing")
  group_split <- strsplit(group, ", ")[[1]]
  
  # Separate "Missing" if it's in the group
  missing <- group_split[group_split == "Missing"]
  non_missing <- group_split[group_split != "Missing"]
  
  # Sort the non-missing items by priority
  sorted_non_missing <- non_missing[order(match(non_missing, priority))]
  
  # Combine sorted non-missing items with "Missing" placed at the end
  sorted_group <- c(sorted_non_missing, missing)
  
  return(paste(sorted_group, collapse = ", "))
}


for (i in seq_along(duplicate_combinations)) {duplicate_combinations[[i]]$unique_anti_groups <- sapply(duplicate_combinations[[i]]$unique_anti_groups, reorder_groups)}

###
# Create lists to store results for counts and percentages
anti_group_summaries <- list()

# Loop through each data frame in subset_duplicate_combinations
for (i in seq_along(duplicate_combinations)) {
  # Group by unique_anti_groups and summarize counts and percentages
  anti_group_summaries[[i]] <- duplicate_combinations[[i]] %>%
    group_by(unique_anti_groups) %>%
    summarise(counts = n()) %>%
    mutate(percentage = counts / sum(counts) * 100) %>%
    mutate(total_counts = sum(counts))
}

# Name the list elements
names(anti_group_summaries) <- paste0("summary_", seq_along(duplicate_combinations))

# Combine subset_duplicate_combinations and anti_group_summaries based on unique_anti_groups
combined_results <- list()

for (i in seq_along(duplicate_combinations)) {
  combined_df <- merge(
    duplicate_combinations[[i]],
    anti_group_summaries[[i]],
    by = "unique_anti_groups",
    all.x = TRUE
  )
  combined_df$df_name <- combination_info[[i]]$df_name
  combined_df$id_list_name <- combination_info[[i]]$id_list_name
  combined_results[[i]] <- combined_df
}

# Concatenate unique_anti_groups with counts and rename the column
for (i in seq_along(combined_results)) {
  combined_results[[i]] <- combined_results[[i]] %>%
    mutate(`unique_anti_groups (counts)` = paste0(unique_anti_groups, " (", counts, ")")) %>%
    select(-unique_anti_groups) %>%
    rename(unique_anti_groups = `unique_anti_groups (counts)`)
}

# Name the combined list elements
names(combined_results) <- paste0("combined_summary_", seq_along(duplicate_combinations))

# Function to combine columns
combine_columns <- function(data, indices) {
  result <- data[indices] %>% reduce(full_join, by = c("recordid", "id_list_name"))
  return(result)
}

generate_sankey <- function(combined_results, columns, main_title) {
  
  df_combined <- combine_columns(combined_results, columns) %>%
    filter(percentage.x >= 0.5) %>%
    arrange(percentage.x, desc(unique_anti_groups.x), desc(unique_anti_groups.y)) %>%
    select(unique_anti_groups.x, unique_anti_groups.y, total_counts.x, total_counts.y) %>%
    rename(emp = unique_anti_groups.x, def = unique_anti_groups.y)
  
  # 
  print("Total counts in df_combined (x):")
  print(unique(df_combined$total_counts.x))
  print("Total counts in df_combined (y) (non-NA values):")
  print(unique(df_combined$total_counts.y[!is.na(df_combined$total_counts.y)]))
  
  df_stack <- df_combined %>%
    make_long(emp, def) %>%
    filter(!is.na(node))
  
  # Extract the part of the node before the parentheses for color mapping
  df_stack$node_base <- sub(" \\(.*\\)", "", df_stack$node)
  
  df_stack$node <- factor(df_stack$node, levels = unique(c(df_combined$emp, df_combined$def)))
  df_stack$node_base <- factor(df_stack$node_base, levels = unique(df_stack$node_base))
  
  # Define color palette
  qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
  mycol <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))) 

  
  # Plot
  p <- ggplot(df_stack, aes(x = x, next_x = next_x,
                            node = node, next_node = next_node,
                            fill = node_base,  
                            label = node)) + 
    geom_sankey(flow.alpha = 1,  
                smooth = 6, 
                width = 0.05, 
                alpha = 1,
                space = 200) +
    geom_sankey_label(
      aes(
        x = stage(x,
                  after_stat = x + .1*
                    dplyr::case_when(
                      x == 1 ~ -0.27,
                      x == 2 ~ 0.27,
                      .default = 0
                    )
        ),
        hjust = dplyr::case_when(
          x == "emp" ~ 1,
          x == "def" ~ 0,
          .default = .5
        )
      ),
      size = 5,
      color = "white",
      fill = "black",
      fontface = "bold",
      label.r = unit(1, "mm"),
      label.size = 0.5,
      space = 200
    ) +
    theme_void() +  
    annotate("text", x = 0.95, y = Inf, 
             label = paste0("Empirical prescriptions"), 
             hjust = 1, vjust = 1, size = 7, 
             fontface = "bold", family = "Times New Roman") +
    annotate("text", x = 2.05, y = Inf, 
             label = paste0("Definitive prescriptions"), 
             hjust = 0, vjust = 1, size = 7, 
             fontface = "bold", family = "Times New Roman") +
    ggtitle(paste0(main_title, " (total = ", 
                   unique(df_combined$total_counts.x[!is.na(df_combined$total_counts.x)]), ")")) +
    theme(plot.title = element_text(hjust = 0.5, 
                                    face = "bold", size = 20, 
                                    family = "Times New Roman"),
          legend.position = 'none',
          text = element_text(family = "Times New Roman")) + 
    scale_fill_manual(values = mycol) 
  
  return(p)
}

# Generate plots
p1 <- generate_sankey(combined_results, c(1, 4), "Carbapenem-resistant Acinetobacter")
p2 <- generate_sankey(combined_results, c(2, 5), 
                      "Third-generation cephalosporin-resistant Enterobacterales")
p3 <- generate_sankey(combined_results, c(3, 6), "Carbapenem-resistant Enterobacterales")

# Add labels
p1 <- arrangeGrob(p1, top = textGrob("(a)", x = unit(0.09, "npc"), y = unit(0.9, "npc"), 
                                     just = c("left", "top"), 
                                     gp = gpar(fontsize = 20, fontface = "bold", 
                                               family = "Times New Roman")))

p2 <- arrangeGrob(p2, top = textGrob("(b)", x = unit(0.09, "npc"), y = unit(0.9, "npc"), 
                                     just = c("left", "top"), 
                                     gp = gpar(fontsize = 20, fontface = "bold", 
                                               family = "Times New Roman")))

p3 <- arrangeGrob(p3, top = textGrob("(c)", x = unit(0.09, "npc"), y = unit(0.9, "npc"), 
                                     just = c("left", "top"), 
                                     gp = gpar(fontsize = 20, fontface = "bold", 
                                               family = "Times New Roman")))

# Combine the plots
combined_plot <- wrap_plots(p1, p2, p3, ncol = 1)

# Save
cairo_pdf("output/figure/sankey_top3.pdf", width = 37, height = 55)
print(combined_plot)
dev.off()




ggsave("output/figure/sankey_top3.pdf", combined_plot, width = 37, height = 45)
