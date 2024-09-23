# Clean
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magrittr, 
                 dplyr,
                 ggpubr,
                 scales,
                 extrafont,
                 Cairo)
})

# Define working directory
wd <- "../"
setwd(wd)

# Load data
df <- readRDS("data/clean_data_RData/data_table_index_new.RData")

#
df <- df[,c("recordid", "country", "siteid", "infection_types", "pathogen_combined_types")]

# Delete NA
df[df == ""] <- NA
df <- df %>% 
  filter(!is.na(country) & !is.na(infection_types) & !is.na(pathogen_combined_types))

# Counts by contry and infection types
fre <- df %>%
  group_by(country, infection_types) %>%
  summarise(counts_episode = n(), .groups = 'drop') %>%
  left_join(
    df %>% group_by(country) %>% summarise(counts_episode_sum = n(), .groups = 'drop'),
    by = "country"
  ) %>%
  mutate(
    proportion = counts_episode / counts_episode_sum,
    country_with_episode = paste(country, " (", counts_episode_sum, ")", sep = "")
  )


# Plot
loadfonts() 
fre$infection_types <- factor(fre$infection_types, 
                              levels = c("Healthcare-associated BSI", 
                                         "Hospital-acquired BSI", "VAP"))

p <- ggbarplot(fre, 
               x = 'country_with_episode', 
               y = 'proportion',                    
               fill = 'infection_types',                    
               color = 'gray30', 
               position = position_dodge(0.6),
               orientation = "horiz",
               width = 0.6, 
               size = 0.2) +
  scale_y_continuous(labels = percent_format(), expand = c(0, 0), 
                     limits = c(0, 1), breaks = seq(0, 1, by = 0.2)) +
  theme_minimal() + 
  theme(
    legend.title = element_text(size = 14, color = "black", family = "Times New Roman"),  
    legend.text = element_text(size = 14, color = "black", family = "Times New Roman"),  
    axis.text = element_text(size = 14, color = "black", family = "Times New Roman"),  
    axis.title.x = element_text(size = 14, color = "black",
                                family = "Times New Roman", margin = margin(t = 10)),
    axis.title.y = element_text(size = 14, color = "black", 
                                family = "Times New Roman", margin = margin(r = 15)),
    legend.position = "top", 
    legend.direction = "horizontal", 
    legend.box.margin = margin(t = 0, r = 90, b = 0, l = 0, unit = "pt"),
    plot.caption = element_text(hjust = 0, vjust = 0, size = 12, 
                                margin = margin(t = 10),
                                family = "Times New Roman"),
    plot.margin = unit(c(0.5, 0.7, 0.5, 0.5), "cm")
  ) +
  labs(
    x = "Country (number of index episodes)",  
    y = "Proportion of infection types (%)",
    fill = "Infection types",
    caption = "Note: Have deleted index episodes with missing organisms."
  ) +
  scale_fill_manual(values = c("VAP" = "#8DD3C7",  
                               "Hospital-acquired BSI" = "#FFFFB3",
                               "Healthcare-associated BSI" = "#B3DE69"),
                    breaks = c("VAP", "Hospital-acquired BSI", "Healthcare-associated BSI"))

plot(p)

# Save figure
CairoPDF(file = "output/figure/proportion_index_episode.pdf", 
         width = 8, height = 12)
print(p)
dev.off()

###
# Index episode number
index_episode_number <- fre %>% 
  group_by(infection_types) %>% 
  summarise(total_counts = sum(counts_episode, na.rm = TRUE))

# Save
saveRDS(index_episode_number, file = "data/clean_data_RData/index_episode_number.RData")
###