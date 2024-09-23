# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr,
                 magrittr,
                 survival,
                 survminer,
                 ggplot2,
                 gridExtra,
                 ggpubr,
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

# Fit a Kaplan-Meier model
fit <- survfit(Surv(time, event) ~ infection_types + ris_third, data = df)

# Risk table
ggsurv <- ggsurvplot(fit, 
                     df,
                     risk.table = TRUE, 
                     break.time.by = 7,
                     xlim = c(0, 28),
                     tables.theme = theme_minimal())

tbl_fct <- 
  ggplot(ggsurv$table$data, aes(time, ris_third, color = ris_third))+ 
  geom_text(aes(label = n.risk), size = 4.5, family = "Times New Roman") +
  facet_wrap(~infection_types, labeller = label_both) + 
  theme_minimal(base_family = "Times New Roman") + 
  theme(panel.grid.major = element_line(color = "gray90", linewidth = 0.5), 
        panel.grid.minor = element_line(color = "gray90", linewidth = 0.25),
        strip.background = element_rect(fill="white", linewidth = 1),
        axis.text = element_text(color="black", size = 12, 
                                 family = "Times New Roman"),
        strip.text = element_blank(),
        legend.position = "none",
        panel.spacing = unit(0.6, "cm"),
        axis.ticks = element_line(color = "black", linewidth = 0.5), 
        axis.ticks.length = unit(0.1, "cm")) +
  scale_y_discrete(limits = c("0", "1"),
                   labels = c("1" = "3GCRE", "0" = "3GCSE")) +
  scale_x_continuous(
    limits = c(0, 28), 
    breaks = c(0, 7, 14, 21, 28),
    labels = c("0", "7", "14", "21", "28")
  ) +
  scale_color_manual(
    values = c("0" = "#EFC000FF", "1" = "#0073c2FF"), 
    labels = c("1" = "3GCRE", "0" = "3GCSE")
  ) +
  ggtitle(" ") +
  xlab("") + 
  ylab("") 

# Plot
pvals_df <- data.frame(
  infection_types = c("VAP", "Hospital-acquired BSI", "Healthcare-associated BSI"),
  pval = c("< 0.0001", "< 0.0001", "= 0.51"),
  x = c(4, 4, 4),   
  y = c(0.2, 0.2, 0.2) 
)

pvals_df$infection_types <- factor(pvals_df$infection_types, 
                                   levels = c("VAP", 
                                              "Hospital-acquired BSI", 
                                              "Healthcare-associated BSI"))

# Plot the survival curves using ggsurvplot_facet
plt_fct <- ggsurvplot_facet(
  fit,
  data = df,
  facet.by = "infection_types",
  pval = F,  
  palette = c("#EFC000FF", "#0073c2FF"),
  conf.int = TRUE,
  linetype = 1,
  ggtheme = theme_minimal(base_family = "Times New Roman") +
    theme(
      panel.grid.major = element_line(color = "gray90", linewidth = 0.5), 
      panel.grid.minor = element_line(color = "gray90", linewidth = 0.25),
      axis.text = element_text(color = "black", size = 12, 
                               family = "Times New Roman"),
      panel.spacing = unit(0.6, "cm"),
      axis.ticks = element_line(color = "black", size = 0.5), 
      axis.ticks.length = unit(0.1, "cm"),
      axis.title = element_text(margin = margin(t = 10), size = 12, 
                                  family = "Times New Roman"), 
      strip.text.x = element_text(size = 12),
      strip.background.x = element_rect(color = "black", 
                                        fill = c("gray90"),
                                        linewidth = NA)
    ),
  title = " ",
  xlab = "Follow-up time since infection onset (days)",
  ylab = "Survival probability",
  font.tickslab = c(12, "plain"),
  font.legend = c(12, "plain"),
  break.time.by = 7,
  xlim = c(0, 28),
  panel.labs = list(infection_types = c("VAP", 
                                        "Hospital-acquired BSI", 
                                        "Healthcare-associated BSI")), 
  panel.labs.font = list(size = 12, family = "Times New Roman"),
  short.panel.labs = TRUE,
  legend.title = " ",
  legend.labs = c("Third-generation cephalosporin-susceptible Enterobacterales",
                  "Third-generation cephalosporin-resistant Enterobacterales"),
  legend = "none"
) +
  geom_text(data = pvals_df, aes(x = x, y = y, label = paste("p", pval)),
            size = 4, family = "Times New Roman")

# Combine plots with Times New Roman font
combined_plot <- ggarrange(
  plt_fct + theme(plot.margin = margin(t = 2, r = 1, b = 2, l = 1)), 
  tbl_fct + theme(plot.margin = margin(t = 0, r = 1, b = 2, l = 1)),
  ncol = 1, heights = c(7.5, 2.5)
) + theme(plot.margin = margin(t = 0, r = 5, b = 0, l = 5))

# Display the combined plot
print(combined_plot)

# Print the Kaplan-Meier plot
pdf(file = "output/figure/att28_death_KM_02.pdf", width = 13, height = 6, onefile = FALSE)
print(combined_plot)
dev.off()
###