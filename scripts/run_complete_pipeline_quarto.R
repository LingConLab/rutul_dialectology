#!/usr/bin/env Rscript

# ============================================================
# COMPLETE PIPELINE FOR RUTUL DIALECTOLOGY WEBSITE USING QUARTO
# ============================================================
# This script:
# 1. Generates all QMD files (English and Russian)
# 2. Renders everything to HTML using Quarto profiles
# 3. Creates a complete multilingual website

cat("🚀 Starting complete Quarto pipeline for Rutul dialectology website...\n\n")

# Install and load required packages
packages <- c("tidyverse", "lingtypology", "DT", "knitr", "glue")
to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(to_install) > 0) {
  cat("📦 Installing required packages...\n")
  install.packages(to_install, dependencies = TRUE)
}

suppressPackageStartupMessages({
  library(tidyverse)
  library(lingtypology)
  library(DT)
  library(knitr)
  library(glue)
})

cat("✅ All packages loaded successfully\n\n")

# ============================================================
# STEP 1: GENERATE ALL QMD FILES
# ============================================================

# Source the multilingual QMD generation script
source("scripts/generate_multilingual_qmd.R")

cat("QMD generation completed!\n\n")

# ============================================================
# STEP 2: RENDER CORE PAGES WITH QUARTO
# ============================================================

# Render core pages (index, team, features)
cat("Rendering core pages...\n")
system("quarto render index.qmd team.qmd features.qmd --to html --output-dir docs", intern = FALSE)

if (file.exists("docs/index.html") && file.exists("docs/team.html") && file.exists("docs/features.html")) {
  cat("Core pages rendered successfully!\n")
} else {
  cat("Core pages rendering failed!\n")
}

cat("\n")

# ============================================================
# STEP 3: RENDER ENGLISH FEATURE PAGES WITH QUARTO
# ============================================================
# Render English feature pages
cat("Rendering English feature pages...\n")
system("quarto render --profile english", intern = FALSE)

if (dir.exists("docs") && length(list.files("docs", pattern = "\\.html$")) > 3) {
  cat("English feature pages rendered successfully!\n")
} else {
  cat("English feature pages rendering failed!\n")
}

cat("\n")

# ============================================================
# STEP 4: RENDER RUSSIAN FEATURE PAGES WITH QUARTO
# ============================================================
cat("Rendering Russian feature pages...\n")
system("quarto render --profile russian", intern = FALSE)

if (dir.exists("docs/ru") && length(list.files("docs/ru", pattern = "\\.html$")) > 0) {
  cat("Russian feature pages rendered successfully!\n")
  
  # Clean up copied files
  cat("Cleaning up temporary files...\n")
  if (dir.exists("docs/ru/data")) {
    system("rm -rf docs/ru/data", intern = FALSE)
    cat("✓ Temporary data directory removed\n")
  }
  if (file.exists("docs/ru/styles.css")) {
    system("rm docs/ru/styles.css", intern = FALSE)
    cat("✓ Temporary styles file removed\n")
  }
} else {
  cat("Russian feature pages rendering failed!\n")
}

cat("\n")

# ============================================================
# COMPLETION SUMMARY
# ============================================================
cat("QUARTO PIPELINE COMPLETED!\n")