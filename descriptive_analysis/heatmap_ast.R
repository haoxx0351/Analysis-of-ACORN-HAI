# Clean
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magrittr, 
                 dplyr,
                 viridis,
                 ggplot2,
                 ggh4x,
                 grid,
                 cowplot,
                 patchwork,
                 extrafont,
                 Cairo)
})

# Define working directory
wd <- "../"
setwd(wd)

# Load fonts
loadfonts() 

# Load data
df <- readRDS("data/clean_data_RData/percent_RIS.RData")
index_number <- readRDS("data/clean_data_RData/index_episode_number.RData")

###
# Select R and cut proportion
df_used <- list()

system.time({
  for (i in seq_along(df)) {
    
    levels(df[[i]]$organism_names)[5] <- "Serratia/Proteus/\nMorganella/Enterobacter"
    
    levels(df[[i]]$anti_group)[c(4, 7, 8, 10, 23, 25, 27)] <- 
      c("Beta-lactam/\nBeta-lactamase",
        "Fifth-generation\ncephalosporins",
        "First-generation\ncephalosporins",
        "Fourth-generation\ncephalosporins",
        "Second-generation\ncephalosporins",
        "Sulfamethoxazole/\ntrimethoprim",
        "Third-generation\ncephalosporins")
    
    r_row <- which(df[[i]]$anti_susceptibility == "Resistance (R)")
    
    df_used[[i]] <- df[[i]][r_row,]
    
    df_used[[i]]$per_cut <- cut(df_used[[i]]$per_count,
                                breaks = c(seq(0, 1.0, 0.1)),
                                levels = 1:10,
                                labels = c("≤10%", "10 to ≤20%", 
                                           "20 to ≤30%", "30 to ≤40%",
                                           "40 to ≤50%", "50 to ≤60%",
                                           "60 to ≤70%", "70 to ≤80%",
                                           "80 to ≤90%", "90 to ≤100%"),
                                right = T, 
                                ordered_result = T)
    
    df_used[[i]]$organism_group <- factor(df_used[[i]]$organism_group,
                                          levels = c("GNB", "GPB", "Fungi"))
    # Move new order
    new_order <- c("Third-generation\ncephalosporins", "Fourth-generation\ncephalosporins", "Polymyxins", "Fluoroquinolones", "Sulfamethoxazole/\ntrimethoprim", "Glycopeptides", "Macrolides", "Penicillins", "Azoles", "Echinocandins", "Polyenes")
    
    anti_index <- which(levels(df_used[[i]]$anti_group) %in% new_order)

    levels_reordered <- c(levels(df_used[[i]]$anti_group)[-anti_index], new_order)
    
    # Update levels for anti_group column
    df_used[[i]]$anti_group <- factor(df_used[[i]]$anti_group, levels = levels_reordered)
  }
})

# Define the color palette and common legend
col <- viridis_pal(option = "plasma", end = 0.9, direction = -1)(10)
names(col) <- c("≤10%", "10 to ≤20%", "20 to ≤30%", "30 to ≤40%",
                "40 to ≤50%", "50 to ≤60%", "60 to ≤70%", "70 to ≤80%",
                "80 to ≤90%", "90 to ≤100%")

# Create plots
dev.new()

# Function to create plots without individual legends
create_plot <- function(data, title, count, show_x_axis = FALSE, caption = "") {
  
  sum_data <- data %>%
    distinct(organism_names, anti_group, sum_count)
  
  plot_title <- paste0(title, " (n = ", count, ")")
  
  plot <- ggplot(data, aes(anti_group, organism_names)) +
    geom_tile(aes(fill = per_cut)) + 
    geom_text(aes(label = sum_count), color = "white", size = 5) +
    scale_fill_manual(values = col, drop = FALSE) +
    theme_bw() +
    labs(x = NULL, y = NULL, caption = caption) +
    ggtitle(plot_title) +  
    theme(
      panel.grid = element_blank(),
      panel.border = element_blank(),
      axis.line.x = if (show_x_axis) element_line(color = "black", linewidth = 0.5) else element_blank(),
      axis.line.y = element_line(color = "black", linewidth = 0.5),
      axis.text.x = if (show_x_axis) element_text(angle = 270, vjust = 0.5, hjust = 0, size = 14, color = "black") else element_blank(),
      axis.text.y = element_text(color = "black", size = 14),
      axis.ticks.x = if (show_x_axis) element_line() else element_blank(),
      axis.ticks.y = element_line(),
      legend.position = "none", 
      strip.text = element_blank(),
      strip.background = element_rect(color = "black", fill = "grey80", linewidth = NA),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      plot.caption = element_text(hjust = 0, size = 10),
      text = element_text(family = "Times New Roman")
    ) +
    facet_grid(organism_group ~ ., scales = "free_y", space = "free")
  
  return(plot)
}

# Create individual plots
p1 <- create_plot(df_used[[3]], "VAP", 
                  index_number$total_counts[[3]], show_x_axis = TRUE)
p2 <- create_plot(df_used[[2]], "Hospital-acquired BSI", 
                  index_number$total_counts[[2]], show_x_axis = TRUE)
p3 <- create_plot(df_used[[1]], "Healthcare-associated BSI", 
                  index_number$total_counts[[1]], 
                  show_x_axis = TRUE)

#
combined_plot <- p1 + p2 + p3 + plot_layout(ncol = 3, widths = c(0.8, 1, 1))


# Create a dummy plot for the legend
dummy_data <- data.frame(
  anti_group = factor(rep("Dummy", 10), levels = "Dummy"),
  organism_names = factor(names(col), levels = names(col)),
  per_cut = factor(names(col), levels = names(col)),
  sum_count = rep("", 10)
)

legend_plot <- ggplot(dummy_data, aes(anti_group, organism_names)) +
  geom_tile(aes(fill = per_cut)) +
  scale_fill_manual(values = col, drop = FALSE) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_text(size = 14, hjust = 0, vjust = 1,
                                margin = margin(t = 0, r = 30, b = 0, l = 0)),
    legend.text = element_text(size = 14, margin = margin(r = 8, l = 4, unit = "pt")),
    legend.margin = margin(b = 10),
    legend.background = element_rect(fill = "white", color = NA),
    text = element_text(family = "Times New Roman"),
    legend.spacing.x = unit(16, 'pt')
  ) +
  guides(fill = guide_legend(title = "Proportion of resistance\n(with total index organisms)", 
                             nrow = 1))

# Extract the legend
get_legend <- function(my_plot) {
  tmp <- ggplot_gtable(ggplot_build(my_plot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(legend_plot)

# Create the final plot layout with legend and caption
final_plot <- combined_plot / legend + 
  plot_layout(heights = c(10, 1)) +
  plot_annotation(
    caption = "Note: n represents the total number of index episodes, excluding those with missing organisms.",
    theme = theme(
      plot.caption = element_text(hjust = 0, vjust = 1, size = 12, margin = margin(t = 10),
                                  family = "Times New Roman")
    )
  ) +
  theme(plot.background = element_rect(fill = "white", color = NA))

# Open a new PDF device
CairoPDF("output/figure/Heatmap_R_index.pdf", width = 26, height = 7, family = "Times New Roman")

# Print the combined plots with the extracted legend at the bottom
print(final_plot)

# Close the PDF device
dev.off()

