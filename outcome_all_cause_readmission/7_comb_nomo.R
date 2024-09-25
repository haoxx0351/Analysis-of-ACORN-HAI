# Clear environment
rm(list = ls())

# Load necessary packages
suppressPackageStartupMessages({
  require(pacman)
  pacman::p_load(magick)
})

# Define working directory
wd <- "./"
setwd(wd)

# Read PDF images
pdf1_image <- image_read_pdf("output/figure/nomo_readmission.pdf")
pdf2_image <- image_read_pdf("output/figure/nomo_death.pdf")

# Adjust the label positions slightly lower
pdf1_image <- image_annotate(pdf1_image, "(a)", gravity = "northwest", size = 22, color = "black", location = "+20+40", weight = 600, font = "Times New Roman")
pdf2_image <- image_annotate(pdf2_image, "(b)", gravity = "northwest", size = 22, color = "black", location = "+20+40", weight = 600, font = "Times New Roman")

# Combine images with reduced spacing
final_image <- image_append(c(pdf1_image, pdf2_image), stack = TRUE)


# Add a note to the bottom left corner using Times New Roman font
final_image <- image_annotate(final_image, "
Abbreviations: CARD, cardiovascular condition; EMD, endocrine / metabolic disorder; GIT, gastrointestinal disorder; HMD, haematological disease; INF, infectious disease; NRD, neurological disease; ONC, oncologic disorder; ORT, orthopaedic condition; PMD, pulmonary disease;\nREN, renal disorder; TRA, trauma; OTH, others; 3GCS, third-generation cephalosporin-susceptible; 3GCR, third-generation cephalosporin-resistant; CR, carbapenem-resistant.", gravity = "southwest", size = 55, color = "black", location = "+20+20", weight = 600, font = "Times New Roman")

# Preview the final image
print(final_image)

# Save the combined image
image_write(final_image, path = "output/figure/com_nomo.pdf", format = "pdf")

