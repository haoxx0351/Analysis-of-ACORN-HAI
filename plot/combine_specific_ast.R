# Clean
rm(list = ls())

# Load packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magick)
})

# Define working directory
wd <- "./"
setwd(wd)

# pdf to fig
pdf1_image <- image_read_pdf("output/figure/specific_ast_vap.pdf")
pdf2_image <- image_read_pdf("output/figure/specific_ast_bsi_hosp.pdf")
pdf3_image <- image_read_pdf("output/figure/specific_ast_bsi_health.pdf")

# Combine 
final_image <- image_append(c(pdf1_image, pdf2_image, pdf3_image), stack = F)

# 
print(final_image)

# Save
image_write(final_image, path = "output/figure/com_specific_ast.pdf", format = "pdf")
###