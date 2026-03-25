#!/usr/bin/env Rscript
# Convert all figure PDFs to PNGs for easy preview/uploading
# USAGE: Rscript convert_figures_to_png.R

if (!requireNamespace("pdftools", quietly = TRUE)) {
  install.packages("pdftools", repos = "https://cran.r-project.org")
}
library(pdftools)

fig_dir <- "figures_boosting"
pdfs <- list.files(fig_dir, pattern = "\\.pdf$", full.names = TRUE)

if (length(pdfs) == 0) {
  stop("No PDFs found in ", fig_dir, ". Run generate_all_figures.R first.")
}

for (f in pdfs) {
  out <- sub("\\.pdf$", ".png", f)
  pdf_convert(f, format = "png", dpi = 200, filenames = out)
  cat("Converted:", basename(f), "->", basename(out), "\n")
}

cat("\nDone! PNGs are in", fig_dir, "/\n")
