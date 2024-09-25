# Clear environment
rm(list = ls())

# Load necessary packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(dplyr, 
                 magrittr, 
                 crrSC, 
                 cmprsk,
                 ggsurvfit,
                 patchwork,
                 rms,
                 tidyverse,
                 gt,
                 purrr,
                 Cairo)
})

# Load data
df <- readRDS("data/readmission_table_index.RData")

# Delete GN_car_thir
x.factors <- model.matrix(~ df$sex + df$country_region +
                            df$country_income + 
                            df$hpd_admreason +
                            df$icu_hd_ap +df$GN_car_thir)[,-1]

# for num
x.factors <- as.matrix(data.frame(x.factors, df$age_new, 
                                  df$severity_score_scale))

# Set labels
label(df$age_new) <- "Age (years)"
label(df$sex) <- "Sex"
label(df$country_region) <- "Region"
label(df$country_income) <- "World Bank income status"
label(df$hpd_admreason) <- "Primary admission reason"
label(df$severity_score_scale) <- "Severity score of disease"
label(df$icu_hd_ap) <- "ICU/HD admissions of enrolment"
label(df$GN_car_thir) <- "Gram-negative bacteria"

levels(df$hpd_admreason) <- c(
  "INF",  # Infectious disease
  "CARD", # Cardiovascular condition
  "EMD",  # Endocrine / Metabolic disorder
  "GIT",  # Gastrointestinal disorder
  "HMD",  # Haematological disease
  "NRD",  # Neurological disease
  "ONC",  # Oncologic disorder
  "ORT",  # Orthopaedic condition
  "PMD",  # Pulmonary disease
  "REN",  # Renal disorder
  "TRA",  # Trauma
  "OTH"   # Others
)

levels(df$GN_car_thir) <- c("3GCS", "3GCR", "CR")

#
dd <- datadist(df)
options(datadist = "dd")

cox_mod <- cph(Surv(readm_death_time, readm_event == 1) ~ age_new + sex + 
                 country_region + country_income +
                 hpd_admreason + severity_score_scale + icu_hd_ap +
                 GN_car_thir + strat(infection_types),
               data = df, x = TRUE, y = TRUE, 
               surv = TRUE, time.inc = 60)

nom <- nomogram(cox_mod,
                fun=function(x)1/(1+exp(-x)),
                fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                funlabel="Risk of 60-day readmission",
                conf.int=F,
                abbrev=F,
                minlength=1,
                lp=F) 

###
# For death
cox_mod_death <- cph(Surv(readm_death_time, readm_event == 2) ~ 
                       age_new + sex + 
                       country_region + country_income +
                       hpd_admreason + severity_score_scale + icu_hd_ap +
                       GN_car_thir + strat(infection_types),
                     data = df, x = TRUE, y = TRUE, 
                     surv = TRUE, time.inc = 60)

nom_death <- nomogram(cox_mod_death,
                      fun=function(x)1/(1+exp(-x)),
                      fun.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                      funlabel="Risk of 60-day death",
                      conf.int=F,
                      abbrev=F,
                      minlength=1,
                      lp=F) 

#
CairoPDF("output/figure/nomo_readmission.pdf", width = 23, height = 9)
par(mar = c(2.5, 2.5, 1, 1.5), family = "Times")
plot(nom, xfrac=.25, 
     total.points.label="Sum of all points", 
     cex.axis = 1.3,
     cex = 1.4,
     force.label = TRUE,
     tcl = -0.3,
     lmgp = 0.2,
     vnames="labels",
     col.grid=gray(c(0.85,0.95)))
dev.off()

CairoPDF("output/figure/nomo_death.pdf", width = 23, height = 9)
par(mar = c(2.5, 2.5, 1, 1.5), family = "Times")
plot(nom_death, xfrac=.25, 
     total.points.label="Sum of all points", 
     cex.axis = 1.3,
     cex = 1.4,
     force.label = TRUE,
     tcl = -0.3,
     lmgp = 0.2,
     vnames="labels",
     col.grid=gray(c(0.85,0.95)))
dev.off()
