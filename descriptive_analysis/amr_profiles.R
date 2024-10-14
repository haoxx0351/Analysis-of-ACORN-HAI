# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magrittr, 
                 dplyr,
                 tibble,
                 tidyr,
                 extrafont,
                 ggupset,
                 ggplot2,
                 gridExtra,
                 grid,
                 Cairo)
})

# Define working directory
wd <- "../"
setwd(wd)

# Load data
df <- readRDS("data/clean_data_RData/ast_all_index.RData")

# Load fonts
loadfonts()

#
df$infection_types <- factor(df$infection_types,
                             levels = c("VAP",
                                        "Hospital-acquired BSI",
                                        "Healthcare-associated BSI"))

df <- df %>%
  select(recordid, infection_types, starts_with("ris_"), org_new) %>%
  na.omit() %>%
  filter(org_new != "Others") %>%
  mutate(across(starts_with("ris_"), ~ ifelse(. == 1, 1, 0))) %>%
  filter(rowSums(select(., starts_with("ris_"))) > 0) 

# Split data by infection types and then by org_new
df_used <- split(df, list(df$infection_types, df$org_new), drop = TRUE)

# ------------------------------------------------------------------------------
# Check unique(recordid) and nrow
result <- lapply(df_used, function(sub_df) {
  n_unique_recordid <- length(unique(sub_df$recordid))
  n_rows <- nrow(sub_df)
  list(
    n_unique_recordid = n_unique_recordid,
    n_rows = n_rows,
    same = n_unique_recordid == n_rows
  )
})

# 
for (i in seq_along(result)) {
  cat("Dataframe", i, ":\n")
  cat("Unique recordid count:", result[[i]]$n_unique_recordid, "\n")
  cat("Total row count:", result[[i]]$n_rows, "\n")
  cat("Same:", result[[i]]$same, "\n\n")
}

# ------------------------------------------------------------------------------
# Initialize an empty list to store combined results
combined_ris_list <- list()

# Get unique org_new
org_new_list <- unique(unlist(lapply(df_used, function(x) unique(x$org_new))))

# Loop through each unique org_new
for (org_new in org_new_list) {
  # Filter data frames with the current org_new
  filtered_dfs <- lapply(df_used, function(sub_df) {
    if (unique(sub_df$org_new) == org_new) {
      return(sub_df)
    } else {
      return(NULL)
    }
  })
  filtered_dfs <- filtered_dfs[!sapply(filtered_dfs, is.null)]
  
  # Process each filtered data frame
  tidy_sub_dfs <- lapply(filtered_dfs, function(sub_df) {
    # Select the necessary columns
    sub_df <- sub_df %>%
      select(recordid, starts_with("ris_"), infection_types) %>%
      rename_with(~ gsub("^ris_", "", .), starts_with("ris_"))
    
    # Extract the infection_types column from the original dataframe
    infection_type <- unique(sub_df$infection_types)
    
    # Make recordid values unique in the format A1, A2, ...
    sub_df <- sub_df %>%
      mutate(recordid = make.unique(paste0("A", seq_len(nrow(sub_df))))) %>%
      column_to_rownames(var = "recordid") %>% 
      select(-infection_types)
    
    # Transpose the data frame and convert it back to a data frame
    sub_df <- t(sub_df) %>% 
      as.data.frame() %>%
      rownames_to_column("anti_class")
    
    # Remove columns that contain only zeros
    sub_df <- sub_df[, colSums(sub_df != 0) > 0]
    
    # Transform the data to long format
    tidy_sub_df <- sub_df %>%
      pivot_longer(cols = -c(anti_class), names_to = "ID", values_to = "Member") %>%
      filter(Member != 0) %>%
      select(-Member)
    
    # Add the infection_types column back
    tidy_sub_df$infection_types <- infection_type
    
    return(tidy_sub_df)
  })
  
  # Combine all tidy_sub_dfs for the current org_new
  combined_tidy_sub_df <- do.call(rbind, tidy_sub_dfs)
  
  # Store the combined result in the list
  combined_ris_list[[org_new]] <- combined_tidy_sub_df
}

###
# Initialize a list to store plots
p_list <- list()

# Iterate over combined_ris_list and create plots
for (org_new in names(combined_ris_list)) {
  
  processed_data_ori <- combined_ris_list[[org_new]] %>%
    group_by(ID, infection_types) %>%
    summarise(anti_class = list(anti_class), .groups = 'drop') %>%
    ungroup() %>%
    mutate(anti_class_str = sapply(anti_class, toString)) %>%
    group_by(infection_types, anti_class_str) %>%
    mutate(count = n()) %>%
    arrange(infection_types)
  
  anti_class_list <- unlist(strsplit(processed_data_ori$anti_class_str, ", "))
  anti_class_frequency <- as.data.frame(table(anti_class_list))
  colnames(anti_class_frequency) <- c("anti", "frequency")
  
  frequency_map <- setNames(anti_class_frequency$frequency, anti_class_frequency$anti)
  
  update_anti_class <- function(anti_class_str, frequency_map) {
    classes <- unlist(strsplit(anti_class_str, ", "))
    updated_classes <- sapply(classes, function(class) {
      freq <- frequency_map[[class]]
      paste(class, "(", freq, ")", sep = "")
    })
    paste(updated_classes, collapse = ", ")
  }
  
  processed_data_ori$anti_class_str <- sapply(processed_data_ori$anti_class_str, update_anti_class, frequency_map)
  
  processed_data_ori$anti_class_list <- strsplit(processed_data_ori$anti_class_str, ", ")
  
  total_index_episode <- nrow(processed_data_ori)
  
  # If org_new is "Stenotrophomonas", remove rows with Carbapenems or Third-generation cephalosporins
  if (org_new == "Stenotrophomonas") {
    processed_data_ori <- processed_data_ori %>%
      filter(!sapply(anti_class_list, function(x) any(grepl("Carbapenems|Third-generation cephalosporins", x))))
  }
  
  processed_data <- processed_data_ori %>%
    filter(count > 1)
  
  max_count <- max(processed_data$count)
  break_interval <- ceiling(max_count / 10)
  
  p <- ggplot(processed_data, aes(x = anti_class_list)) +
    geom_bar(fill = "#0073C2FF", color = "black") +
    scale_x_upset() +
    scale_y_continuous(breaks = seq(0, max_count, by = break_interval),
                       expand = c(0, 0),
                       position = "right") +  # Move y-axis to the right
    facet_grid(infection_types ~ ., scales = "free", space = "free", 
               switch = "y") +  # Move facet labels to the left
    theme_minimal(base_size = 15) +
    theme_combmatrix(combmatrix.label.text = element_text(color = "black", size=12),
                     combmatrix.label.make_space = FALSE,
                     plot.margin = unit(c(2, 4, 2, 110), "pt")) +
    theme(panel.grid.major = element_line(color = "grey80", linetype = "dashed"),
          panel.grid.minor = element_line(color = "grey90", linetype = "dashed"),
          panel.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(hjust = 0.5, face = "bold", size = 18),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          axis.title.y = element_text(face = "bold", size = 14),
          axis.text.y = element_text(color = "black", size = 12),
          strip.text.y.left = element_text(angle = 0, face = "bold", size = 12), 
          strip.placement = "outside",
          strip.switch.pad.grid = unit(0.6, "lines"), 
          panel.spacing = unit(1, "lines"),
          text = element_text(family = "Times New Roman")) +
    labs(x = "",
         y = "Index episodes",
         title = org_new,
         subtitle = paste("n =", total_index_episode))
  
  p_list[[org_new]] <- p
  print(p_list[[org_new]])
}

# Save
p1 <- p_list[[1]]
p2 <- p_list[[2]]
p3 <- p_list[[3]]
p4 <- p_list[[4]]
p5 <- p_list[[5]]
p7 <- p_list[[7]]
p8 <- p_list[[8]]
p6 <- p_list[[6]]
p9 <- p_list[[9]]

# Create an empty grob for the first column
empty_col <- nullGrob()  # Alternatively, you can use textGrob("") if you prefer

# Arrange in a 3-column layout
p6_9 <- arrangeGrob(empty_col, p6, p9, ncol = 3, widths = c(0.5, 1, 1.4))

# Arrange plots in rows
row1 <- arrangeGrob(p1, p2, ncol = 2, widths = c(1, 1.2))
row2 <- arrangeGrob(p4, p5, p7, ncol = 3)
row3 <- arrangeGrob(p3, p8, p6_9, ncol = 3, widths = c(1.7, 1.2, 1))

# Create note as grobs
note <- textGrob("Note: n means total resistance index episodes.", 
                 x = 0.01, y = 0.8,
                 just = "left",
                 gp = gpar(fontsize = 12, fontfamily = "Times New Roman"))

# Combine all grobs into one
combined_plot <- arrangeGrob(
  row1,
  row2,
  row3,
  note,
  ncol = 1,
  heights = c(0.45, 0.45, 0.45, 0.01)
)

# Save the plot
ggsave("resistance_profiles_MDROS.pdf", 
       plot = combined_plot,
       width = 39, height = 30, 
       device = cairo_pdf,
       path = "output/figure/",
       limitsize = FALSE)
###