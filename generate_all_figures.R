#!/usr/bin/env Rscript
# ===========================================================================
# generate_all_figures.R
#
# Standalone script: sources AntBoost package files directly and generates
# all 5 figures + Table 1 CSV for the Part II manuscript.
#
# USAGE:
#   Rscript generate_all_figures.R
#
# REQUIREMENTS:
#   install.packages(c("ggplot2", "patchwork", "dplyr", "tidyr"))
#
# OUTPUT:
#   figures_boosting/
#     figure1_weak_learnability.pdf
#     figure2_weight_pheromone.pdf
#     figure3_margin_quorum.pdf
#     figure4_convergence.pdf
#     figure5_noise_robustness.pdf
#     table1_performance.csv
# ===========================================================================

cat("========================================\n")
cat(" AntBoost Figure Generator\n")
cat(" Part II: Boosting <-> Ant Recruitment\n")
cat("========================================\n\n")

# --- Load libraries --------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
  library(dplyr)
  library(tidyr)
})

# --- Source package R files ------------------------------------------------
# Works whether you run from the repo root or from within the AntBoost dir
pkg_dir <- if (file.exists("AntBoost/R/adaboost.R")) {
  "AntBoost/R"
} else if (file.exists("R/adaboost.R")) {
  "R"
} else {
  stop("Cannot find AntBoost/R/ directory. Run from the repo root.")
}

cat("Sourcing package files from:", pkg_dir, "\n")
for (f in list.files(pkg_dir, pattern = "\\.R$", full.names = TRUE)) {
  source(f)
}

# --- Run -------------------------------------------------------------------
cat("\nStarting simulations (this may take 5--15 minutes)...\n\n")
generate_all_figures(output_dir = "figures_boosting", seed = 19671210)

cat("\n========================================\n")
cat(" Done! Figures are in figures_boosting/\n")
cat("========================================\n")
