# Clear
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr,
                 magrittr,
                 survival,
                 survminer,
                 extrafont,
                 Cairo)
})

# Define working directory
wd <- "./"
setwd(wd)

# Load data
df <- readRDS("data/first28_data.RData")

# Load fonts
loadfonts()

#
df$time[df$time == 28 & df$event == 1] <- 27.999
# Fit a survival object
surv_object <- Surv(time = df$time, event = df$event)

# Fit a Kaplan-Meier model
km_fit <- survfit(surv_object ~ infection_types, data = df)

# Define custom theme settings
custom_theme <- theme_minimal(base_family = "Poppins") +
  theme(
    axis.text.x = element_text(size = 12, color = "black", 
                               family = "Poppins"), 
    plot.subtitle = element_text(size = 12, color = "black",
                                 family = "Poppins"),
    axis.title.x = element_text(size = 12, color = "black",
                                family = "Poppins"),
    axis.title.y = element_text(size = 12, color = "black",
                                family = "Poppins"),
    plot.title = element_text(size = 12, color = "black",
                              family = "Poppins"),
    legend.title = element_text(size = 12, color = "black",
                                family = "Poppins"),
    legend.text = element_text(size = 12, color = "black",
                               family = "Poppins")
  )

# Create Kaplan-Meier plot with custom theme
km_plot <- ggsurvplot(
  km_fit,
  data = df,
  pval = TRUE,
  pval.size = 4,
  font.family = "Poppins",
  conf.int = TRUE,
  risk.table = TRUE,
  risk.table.col = "strata",
  linetype = 1,
  ggtheme = custom_theme,
  title = " ",
  xlab = "Follow-up time since infection onset (days)",
  ylab = "Survival probability",
  legend.title = "Infection syndromes",
  legend.labs = c("VAP", 
                  "Hospital-acquired BSI", 
                  "Healthcare-associated BSI"),
  palette = c("#db6968", "#459943", "#0074b3"),
  risk.table.title = "Number at risk",
  tables.theme = theme_cleantable(),
  risk.table.y.text = F,
  font.title = c(12, "plain", "black"),
  font.x = c(12, "plain", "black"),
  font.y = c(12, "plain", "black"),
  font.tickslab = c(12, "plain", "black"),
  font.risk.table = c(12, "plain", "black"),
  break.time.by = 7,
  xlim = c(0, 28)
)

# Save figure
cairo_pdf(file = "output/figure/first28_death_KM.pdf", 
         width = 12, height = 7)
print(km_plot)
dev.off()
###
