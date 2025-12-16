# Cleaning script for Rutul dialectology database

# Load required packages
library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

# Set working directory to the project root
# setwd('rutul_dialectology-master')

cat("Starting database cleaning process...\n")

# Check what data files are available
data_files <- list.files("data", pattern = "\\.csv$|\\.tsv$", full.names = TRUE)
cat("Available data files:\n")
cat(paste("  ", data_files), sep = "\n")

# Define standard columns (non-social metadata columns to keep in cleaned version)
standard_columns <- c("feature_id", "feature_title", "feature_lexeme", "feature_description", 
                      "collected", "compiled", "updated_day", "updated_month", "updated_year", 
                      "domain", "settlement", "value", "stimuli", "answer")

# Function to identify social metadata columns for the internal use only
is_social_metadata <- function(col_name) {
  # Exact column names for social metadata
  exact_social_cols <- c("speaker_code", "name", "patronym", "l_name", "gender", 
                         "year_of_birth", "birth_place", "schooling", "residence_place",
                         "long_periods_outside", "mothers_birthplace")
  
  # Check for exact matches (case-insensitive)
  if (tolower(col_name) %in% tolower(exact_social_cols)) {
    return(TRUE)
  }
  
# Initialize empty database (will preserve all columns including social metadata)
database_full <- data.frame()

# Initialize tracking list for processing summary
processing_summary <- list()

# 1. Process noun features (Asya and Kostya)
if (file.exists("data/noun_features_coding.csv")) {
  df <- read_csv("data/noun_features_coding.csv", show_col_types = FALSE)
  if ("to_map" %in% colnames(df)) {
    df_filtered <- df %>%
      filter(to_map == 1) %>%
      mutate(feature_id = as.double(factor(feature_title)),
             compiled = "Asya Alekseeva") %>%
      arrange(feature_id)
    
    database_full <- bind_rows(database_full, df_filtered)
    processing_summary[["Noun features (Asya & Kostya)"]] <- nrow(df_filtered)
  } else {
    processing_summary[["Noun features (Asya & Kostya)"]] <- "Error: 'to_map' column not found"
  }
} else {
  processing_summary[["Noun features (Asya & Kostya)"]] <- "File not found"
}

# 2. Process Nikita's phonetics
if (file.exists("data/nikita_phonology_3.csv")) {
  max_id_in_db <- if (nrow(database_full) > 0) max(database_full$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/nikita_phonology_3.csv", show_col_types = FALSE)
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    arrange(feature_id)
  
  database_full <- bind_rows(database_full, df_filtered)
  processing_summary[["Nikita's phonetics"]] <- nrow(df_filtered)
} else {
  processing_summary[["Nikita's phonetics"]] <- "File not found"
}

# 3. Process Ilya's features
if (file.exists("data/rutul_dialectology_ilya.csv")) {
  max_id_in_db <- if (nrow(database_full) > 0) max(database_full$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/rutul_dialectology_ilya.csv", show_col_types = FALSE)
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    arrange(feature_id)
  
  database_full <- bind_rows(database_full, df_filtered)
  processing_summary[["Ilya's features"]] <- nrow(df_filtered)
} else {
  processing_summary[["Ilya's features"]] <- "File not found"
}

# 4. Process Garik's lexicon
if (file.exists("data/lexicon_moroz_full_ready.csv")) {
  max_id_in_db <- if (nrow(database_full) > 0) max(database_full$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/lexicon_moroz_full_ready.csv", show_col_types = FALSE) %>%
    filter(!is.na(value), value != "boring")
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    arrange(feature_id)
  
  database_full <- bind_rows(database_full, df_filtered)
  processing_summary[["Garik's lexicon"]] <- nrow(df_filtered)
} else {
  processing_summary[["Garik's lexicon"]] <- "File not found"
}

# 5. Process Nastya's verb features
if (file.exists("data/verb_2025-23-08.csv")) {
  max_id_in_db <- if (nrow(database_full) > 0) max(database_full$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/verb_2025-23-08.csv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    arrange(feature_id)
  
  database_full <- bind_rows(database_full, df_filtered)
  processing_summary[["Nastya's verb features"]] <- nrow(df_filtered)
} else {
  processing_summary[["Nastya's verb features"]] <- "File not found"
}

# 6. Process Nina's features
if (file.exists("data/NINA rutul_dialectology_merged_raw_data.csv")) {
  max_id_in_db <- if (nrow(database_full) > 0) max(database_full$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/NINA rutul_dialectology_merged_raw_data.csv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    arrange(feature_id)
  
  database_full <- bind_rows(database_full, df_filtered)
  processing_summary[["Nina's features"]] <- nrow(df_filtered)
} else {
  processing_summary[["Nina's features"]] <- "File not found"
}

# 7. Process Maxim's features
if (file.exists("data/other_features_Maks.csv")) {
  max_id_in_db <- if (nrow(database_full) > 0) max(database_full$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/other_features_Maks.csv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    arrange(feature_id)
  
  database_full <- bind_rows(database_full, df_filtered)
  processing_summary[["Maxim's features"]] <- nrow(df_filtered)
} else {
  processing_summary[["Maxim's features"]] <- "File not found"
}

# 8. Process Vanya's features
if (file.exists("data/netkachev_Rutul_data.csv")) {
  max_id_in_db <- if (nrow(database_full) > 0) max(database_full$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/netkachev_Rutul_data.csv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    arrange(feature_id)
  
  database_full <- bind_rows(database_full, df_filtered)
  processing_summary[["Vanya's features"]] <- nrow(df_filtered)
} else {
  processing_summary[["Vanya's features"]] <- "File not found"
}

# 9. Process 200-words-lists
if (file.exists("data/rutul_dialects_200.tsv")) {
  max_id_in_db <- if (nrow(database_full) > 0) max(database_full$feature_id, na.rm = TRUE) else 0
  
  df <- read_tsv("data/rutul_dialects_200.tsv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    arrange(feature_id)
  
  database_full <- bind_rows(database_full, df_filtered)
  processing_summary[["200-words-lists"]] <- nrow(df_filtered)
} else {
  processing_summary[["200-words-lists"]] <- "File not found"
}

# Print processing summary
cat("\nProcessing summary:\n")
for (source_name in names(processing_summary)) {
  result <- processing_summary[[source_name]]
  if (is.numeric(result)) {
    cat(sprintf("  %s: %d features\n", source_name, result))
  } else {
    cat(sprintf("  %s: %s\n", source_name, result))
  }
}

# Final data cleaning
cat("\nFinal data cleaning...\n")
if (nrow(database_full) > 0) {
  # Apply cleaning transformations to full database
  database_full_cleaned <- database_full %>%
    mutate_at(c("collected", "compiled", "domain", "settlement"), str_to_title) %>%
    mutate(feature_title = str_replace_all(feature_title, "'", "'"),
           feature_lexeme = str_replace_all(feature_lexeme, "'", "'"),
           value = str_replace_all(value, "\\s{2,}", " "))
  
  # Identify social metadata columns
  all_cols <- colnames(database_full_cleaned)
  social_metadata_cols <- all_cols[sapply(all_cols, is_social_metadata)]
  
  # Create cleaned version without social metadata
  # Keep all columns except social metadata columns
  cols_to_keep <- setdiff(all_cols, social_metadata_cols)
  
  database_cleaned <- database_full_cleaned %>%
    select(all_of(cols_to_keep))
  
  # Save full database with all metadata
  write_csv(database_full_cleaned, "data/database_full_with_metadata.csv", na = "")
  cat("Saved full database with metadata to data/database_full_with_metadata.csv\n")
  
  write_xlsx(database_full_cleaned, "data/database_full_with_metadata.xlsx")
  cat("Saved full database with metadata to data/database_full_with_metadata.xlsx\n")
  
  if (length(social_metadata_cols) > 0) {
    cat("Social metadata columns excluded from cleaned version:", paste(social_metadata_cols, collapse = ", "), "\n")
  }
  
  # Save cleaned database without social metadata (safe for GitHub)
  write_csv(database_cleaned, "data/database_cleaned.csv", na = "")
  cat("Saved cleaned database (without social metadata) to data/database_cleaned.csv\n")
  
  write_xlsx(database_cleaned, "data/database_cleaned.xlsx")
  cat("Saved cleaned database (without social metadata) to data/database_cleaned.xlsx\n")
  
  cat("\nFinal database summary:\n")
  cat("  Total features:", nrow(database_full_cleaned), "\n")
  cat("  Unique feature titles:", length(unique(database_full_cleaned$feature_title)), "\n")
  cat("  Unique settlements:", length(unique(database_full_cleaned$settlement)), "\n")
  cat("  Total columns in full database:", ncol(database_full_cleaned), "\n")
  cat("  Columns in cleaned database (without social metadata):", ncol(database_cleaned), "\n")
  
} else {
  cat("  ✗ No data was processed successfully\n")
}

cat("\nDatabase cleaning process completed!\n")
cat("NOTE: Files with '_full_with_metadata' contain social data and should NOT be posted on GitHub.\n")
cat("      Only 'database_cleaned.csv' and 'database_cleaned.xlsx' should be shared publicly.\n")
