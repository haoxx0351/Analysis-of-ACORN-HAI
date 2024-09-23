# Clean
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magick)
})

# Define working directory
wd <- "../"
setwd(wd)

# pdf to fig
pdf1_image <- image_read_pdf("output/figure/pie_charts_ast_vap.pdf")
pdf2_image <- image_read_pdf("output/figure/pie_charts_ast_bsi_hosp.pdf")
pdf3_image <- image_read_pdf("output/figure/pie_charts_ast_bsi_health.pdf")

# Combine 
final_image <- image_append(c(pdf1_image, pdf2_image, pdf3_image), stack = F)

# 
print(final_image)

# Save
image_write(final_image, path = "output/figure/pie_charts_ast_all.pdf", format = "pdf")
###