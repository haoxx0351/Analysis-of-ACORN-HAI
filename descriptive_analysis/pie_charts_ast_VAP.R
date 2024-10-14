# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr,
                 ggplot2,
                 scatterpie,
                 RColorBrewer,
                 extrafont,
                 Cairo)
})

# Define working directory
wd <- "../"
setwd(wd)

# Load data
df <- readRDS("data/clean_data_RData/pie_specific_anti_ast.RData")

# Load fonts
loadfonts() 

# Plot
df_plot <- df[which(df$infection_types %in% "VAP"),]
df_plot <- df_plot[which(df_plot$ris %in% c("Resistance (R)", "Susceptible (I)", "Susceptible (S)")),]
df_plot[is.na(df_plot)] <- 0

# Labels for plot
anti_group_labels <- levels(factor(df_plot$anti_group))
orgi_new_labels <- levels(factor(df_plot$org_new))

anti_group_labels[c(3, 7, 13, 14)] <- 
  c("Beta-lactam/\nBeta-lactamase",
    "Fourth-generation\ncephalosporins",
    "Sulfamethoxazole/\ntrimethoprim",
    "Third-generation\ncephalosporins")

orgi_new_labels[7] <- "Serratia/Proteus/\nMorganella/Enterobacter"

# Trans to num
df_plot$anti_group <- as.numeric(as.factor(df_plot$anti_group))
df_plot$org_new <- as.numeric(as.factor(df_plot$org_new))
#####
# Set colors
qual_col_pals <- brewer.pal.info[brewer.pal.info$category == 'qual',]
colors <- unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals))) 

# 
df_plot$ris <- droplevels(df_plot$ris)
df_plot$ris <- factor(df_plot$ris ,levels = c("Resistance (R)", "Susceptible (I)", "Susceptible (S)"))
#
p <- ggplot() +
  geom_scatterpie(data = df_plot, 
                  aes(x = anti_group, y = org_new, r = 0.4, fill = anti_group),
                  cols = c(colnames(df_plot)[-(1:5)])) +
  scale_fill_manual(values = colors, guide = guide_legend(ncol = 2)) +
  scale_y_continuous(limits = c(0.5, 9.5), 
                     breaks = seq(1, 9, by = 1), 
                     expand = c(0, 0),
                     labels = orgi_new_labels) +
  scale_x_continuous(limits = c(0.5, 14.5), 
                     breaks = seq(1, 14, by = 1), 
                     expand = c(0, 0),
                     labels = anti_group_labels) +
  theme_bw() + 
  labs(x = "", y = "", title = "AST results (hospital-acquired VAP)",
       fill = "Proportion of specific antibiotic (vertical)",
       caption = "Note: Susceptible(I), increased exposure; Susceptible(S), standard dosing regimen.") +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    legend.title = element_text(size = 12), 
    legend.text = element_text(size = 12),
    legend.position = "right",   
    panel.border = element_blank(),
    plot.caption = element_text(color = "black", size = 10,
                                hjust = 0, vjust = 1),
    plot.title = element_text(size = 14, color = "black",
                              face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 270, 
                               hjust = 0, vjust = 0.5, 
                               colour = "black", size = 12),  
    axis.text.y = element_text(margin = margin(r = 0.5, t = -2),
                               colour = "black", size = 12),
    axis.line = element_line(colour = "black", linewidth = 0.45),
    strip.text.y = element_text(size = 12, face = "bold"),
    strip.background.y = element_rect(color = "black", 
                                      fill = c("grey90"),
                                      linewidth = NA),
    text = element_text(family = "Times New Roman")
  ) + 
  facet_grid(ris ~ ., scales = "free_x", space = "free_x", switch = "x")
#
plot(p)

# Save
CairoPDF("output/figure/pie_charts_ast_vap.pdf", width = 12, height = 10.5)
print(p)
dev.off()
###