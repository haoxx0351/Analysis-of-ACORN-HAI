# Clean
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magrittr, 
                 dplyr,
                 ggplot2,
                 grid,
                 gridExtra,
                 patchwork,
                 extrafont,
                 Cairo)
})
###
# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/clean data/percent_RIS.RData")
index_number <- readRDS("data/clean data/index_episode_number.RData")

# Load fonts
loadfonts()

# Make plots
options(timeout = 60)

data_list <- plot_list <- list()

# Split data
system.time({
  for (i in seq_along(df)) {
    
    df_used <- df[[i]]
    
    levels(df_used$organism_names)[5] <- "Serratia/Proteus/\nMorganella/Enterobacter"
    
    levels(df_used$anti_group)[c(4, 7, 8, 10, 23, 25, 27)] <- 
      c("Beta-lactam/\nBeta-lactamase",
        "Fifth-generation\ncephalosporins",
        "First-generation\ncephalosporins",
        "Fourth-generation\ncephalosporins",
        "Second-generation\ncephalosporins",
        "Sulfamethoxazole/\ntrimethoprim",
        "Third-generation\ncephalosporins")
    
    # Save
    data_list[[i]] <- df_used
    
    #
    data_list[[i]]$organism_group <- factor(data_list[[i]]$organism_group,
                                            levels = c("GNB", "GPB", "Fungi"))
    # Move new order
    new_order <- c("Third-generation\ncephalosporins", "Fourth-generation\ncephalosporins", "Polymyxins", "Fluoroquinolones", "Sulfamethoxazole/\ntrimethoprim", "Glycopeptides", "Macrolides", "Penicillins", "Azoles", "Echinocandins", "Polyenes")
    
    anti_index <- which(levels(data_list[[i]]$anti_group) %in% new_order)
    
    levels_reordered <- c(levels(data_list[[i]]$anti_group)[-anti_index], new_order)
    
    # Update levels for anti_group column
    data_list[[i]]$anti_group <- factor(data_list[[i]]$anti_group, levels = levels_reordered)
  }
})

temp <- data_list[[1]]
data_list[[1]] <- data_list[[3]]
data_list[[3]] <- temp


index_number$total_counts <- c(index_number$total_counts[3:1])

# Plot
options(expressions = 50000) 

system.time({
  for (i in 1:3) {
    df_used <- data_list[[i]]
    
    tit <- levels(df_used$infection_types)
    total_count <- index_number$total_counts[i]
    df_used$organism_names <- factor(df_used$organism_names, 
                                     levels = unique(df_used$organism_names))
    
    # Plot
    p <- ggplot(df_used, 
                aes(y = organism_names,
                    x = per_count, 
                    fill = anti_susceptibility)) +
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      geom_text(aes(label = sum_count, x = 0.93, y = organism_names), 
                vjust = 0.5, size = 3, color = "white") +
      facet_grid(organism_group ~ anti_group, scales = "free",
                 space = "free", switch = "y") +
      scale_fill_manual(
        values = c("Resistance (R)" = "#440154FF",  
                   "Susceptible (I)" = "#FDE725FF",
                   "Susceptible (S)" = "#27AD81FF",
                   "Unknown (U)" = "#C3C3C3"),
        breaks = c("Resistance (R)", "Susceptible (I)", "Susceptible (S)", "Unknown (U)")) +
      scale_x_continuous(labels = scales::percent) +
      theme_bw() + 
      labs(x = "", y = "", title = "") +
      theme(
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size = 12, hjust = 0, vjust = 1,
                                    margin = margin(r = 20, unit = "pt")),
        legend.text = element_text(size = 12, 
                                   margin = margin(l = 3, r = 6, unit = "pt")),
        legend.position = ifelse(i == 3, "bottom", "none"),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color = "black", size = 10,
                                    hjust = 0, vjust = 1),
        axis.text.x = element_text(angle = 270, 
                                   hjust = 0, vjust = 0.5, 
                                   colour = "black", size = 12),  
        axis.text.y = element_text(margin = margin(r = 2),
                                   hjust = 1, vjust = 0.5,
                                   colour = "black", size = 12),
        axis.line = element_line(colour = "black", linewidth = 0.45),
        panel.spacing.x = unit(0.5, "lines"),
        strip.text.y = element_blank(),
        strip.text.x = element_text(size = 12),
        strip.background.x = element_rect(color = "black", 
                                          fill = c("#0099FF"),
                                          linewidth = NA),
        text = element_text(family = "Times New Roman")
      ) +
      ggtitle(paste0(tit[i], " (n = ", total_count, ")")) +
      theme(plot.title = element_text(size = 16, color = "black",
                                      face = "bold", hjust = 0.5)) +
      labs(fill = "Susceptibility testing categories\n(with total index organisms)",
           caption = ifelse(i == 3, 
                            "Note: n represents the total number of index episodes, excluding those with missing organisms; Susceptible(I), increased exposure; Susceptible(S), standard dosing regimen.", "")) 
    
    plot_list[[i]] <- p
  }
})
# 
all_p <- wrap_plots(plot_list, nrow = 3) 

plot(all_p)

# Save figure
CairoPDF(file = "output/figure/percent_RIS_index.pdf", 
         width = 33, height = 17)
print(all_p)
dev.off()
###