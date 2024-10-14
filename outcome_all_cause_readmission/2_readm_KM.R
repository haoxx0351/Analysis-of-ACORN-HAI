# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr,
                 magrittr,
                 tidycmprsk, 
                 ggsurvfit,
                 ggplot2,
                 grid,
                 patchwork,
                 tidyverse,
                 survminer,
                 survival,
                 extrafont)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/readmission60_data.RData")

# Load fonts
loadfonts()

# outcome variable is already a labelled factor, so reset to numeric
df$readm_event <- as.numeric(df$readm_event)

# factor with levels and labels whatever you want them to be
df$readm_event <- factor(df$readm_event,
                         levels = c(0, 2, 1),
                         labels = c("censor", "All-cause death", "Readmission"))

#
df$readm_death_time[df$readm_death_time == 60 & df$readm_event %in% c(1,2)] <- 59.999

#
df$infection_types <- factor(df$infection_types,
                             levels = c("VAP", 
                                        "Hospital-acquired BSI",
                                        "Healthcare-associated BSI"))

#
cuminc <- cuminc(Surv(readm_death_time, readm_event) ~ infection_types, data = df)

# Customize gg_cuminc plot
gg_cuminc <- cuminc |>
  ggcuminc(outcome = c("Readmission", "All-cause death")) +
  scale_ggsurvfit() +
  scale_x_continuous(breaks = seq(0, 60, by = 5), limits = c(0, 60)) +
  theme_classic() +
  theme(
    legend.position = 'top',
    legend.direction = "horizontal",
    axis.text = element_text(size = 10, color = "black", family = "Poppins"),
    axis.title = element_text(size = 10, color = "black", family = "Poppins"),
    legend.text = element_text(size = 10, color = "black", family = "Poppins"),
    legend.title = element_text(size = 10, color = "black", face = "plain", family = "Poppins")
  ) +
  labs(x = "Follow-up time (days)") +
  add_confidence_interval() +
  scale_color_manual(
    name = "Infection syndromes",
    values = c("VAP" = "#db6968",
               "Hospital-acquired BSI" = "#459943",
               "Healthcare-associated BSI" = "#0074b3"),
    breaks = c("VAP", 
               "Hospital-acquired BSI", 
               "Healthcare-associated BSI")
  ) +
  scale_fill_manual(
    name = "Infection syndromes",
    values = c("VAP" = "#db6968",
               "Hospital-acquired BSI" = "#459943",
               "Healthcare-associated BSI" = "#0074b3"),
    breaks = c("VAP", 
               "Hospital-acquired BSI", 
               "Healthcare-associated BSI")
  ) +
  scale_linetype_manual(
    name = "Events",
    values = c("Readmission" = "solid", "All-cause death" = "dashed"),
    breaks = c("Readmission", "All-cause death"),
    labels = c("Readmission", "All-cause death")
  ) +
  annotate("text", x = 3.3, y = 0.3, label = "p < 0.001 (all-cause death)",
           size = 3.5, color = "black", family = "Poppins") +
  annotate("text", x = 2.5, y = 0.34, label = "p < 0.001 (readmission)", 
           size = 3.5, color = "black", family = "Poppins")


# Build the risk table plot with custom facet strip colors
gg_risktable <- 
  cuminc |>
  tidy_cuminc(times = c(seq(0, 60, by = 5))) |>
  select(outcome, strata, time, n.risk, cum.event) %>%
  {
    distinct(., strata, time, n.risk) |> 
      mutate(outcome = "At Risk") |> 
      rename(stat = n.risk) |> 
      bind_rows(select(., outcome, strata, time, stat = cum.event))
  } |>
  mutate(outcome = recode(outcome, 
                          "All-cause death" = "death", 
                          "Readmission" = "readm", 
                          "At Risk" = "trisk")) |> 
  mutate(outcome = factor(outcome, levels = c("death", "readm", "trisk"))) |>
  ggplot(aes(x = time, y = factor(outcome), label = stat)) +
  geom_text(size = 3.5, color = "black", family = "Poppins",
            position = position_dodge(width = 0.9), 
            check_overlap = TRUE) +
  labs(y = NULL, x = NULL) +
  facet_grid(strata ~ .) +
  coord_cartesian(xlim = c(0, 60)) +
  theme_light() +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    strip.text = element_text(size = 0),
    strip.placement = "outside",
    axis.text = element_text(size = 10, color = "black", family = "Poppins")
  )

# Convert the ggplot object to a gtable
g <- ggplot_gtable(ggplot_build(gg_risktable))

# Identify the facet strips
stripr <- which(grepl('strip-r', g$layout$name))

# Define the colors you want for the facet strips
fills <- c(adjustcolor("#db6968", alpha.f = 0.7), 
           adjustcolor("#459943", alpha.f = 0.7), 
           adjustcolor("#0074b3", alpha.f = 0.7))

# Modify the background color of the facet strips
k <- 1
for (i in stripr) {
  j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
  g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  k <- k + 1
}

# Align and combine the plots
gg_combined <- list(gg_cuminc, g)

# Visualize the combined plots
combined_plot <- wrap_plots(
  gg_combined[[1]], 
  gg_combined[[2]], 
  ncol = 1,
  heights = c(1, 0.6)
) + 
  plot_layout(widths = c(1.5)) + 
  plot_annotation(
    theme = theme(plot.margin = margin(10, 10, 10, 45))
  )

# Print the combined plot
print(combined_plot)

# Save figure
cairo_pdf(file = "output/figure/readmission_CIF.pdf", 
          width = 12, height = 7)
print(combined_plot)
dev.off()
###
