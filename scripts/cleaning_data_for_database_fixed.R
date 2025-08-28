# Fixed version of the cleaning script for Rutul dialectology database
# This script works with the available packages and data files

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

# Initialize empty database
database <- data.frame()

# 1. Process noun features (Asya and Kostya)
cat("\n1. Processing noun features...\n")
if (file.exists("data/noun_features_coding.csv")) {
  df <- read_csv("data/noun_features_coding.csv", show_col_types = FALSE)
  if ("to_map" %in% colnames(df)) {
    df_filtered <- df %>%
      filter(to_map == 1) %>%
      mutate(feature_id = as.double(factor(feature_title)),
             compiled = "Asya Alekseeva") %>%
      select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
             updated_month, updated_year, domain, settlement, value, stimuli, answer) %>%
      arrange(feature_id)
    
    database <- rbind(database, df_filtered)
    cat("  ✓ Added", nrow(df_filtered), "noun features\n")
  } else {
    cat("  ✗ 'to_map' column not found in noun_features_coding.csv\n")
  }
} else {
  cat("  ✗ noun_features_coding.csv not found\n")
}

# 2. Process Nikita's phonetics
cat("\n2. Processing Nikita's phonetics...\n")
if (file.exists("data/nikita_phonology_3.csv")) {
  max_id_in_db <- if (nrow(database) > 0) max(database$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/nikita_phonology_3.csv", show_col_types = FALSE)
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
           updated_month, updated_year, domain, settlement, value, stimuli, answer) %>%
    arrange(feature_id)
  
  database <- rbind(database, df_filtered)
  cat("  ✓ Added", nrow(df_filtered), "phonetic features\n")
} else {
  cat("  ✗ nikita_phonology_3.csv not found\n")
}

# 3. Process Ilya's features
cat("\n3. Processing Ilya's features...\n")
if (file.exists("data/rutul_dialectology_ilya.csv")) {
  max_id_in_db <- if (nrow(database) > 0) max(database$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/rutul_dialectology_ilya.csv", show_col_types = FALSE)
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
           updated_month, updated_year, domain, settlement, value, stimuli, answer) %>%
    arrange(feature_id)
  
  database <- rbind(database, df_filtered)
  cat("  ✓ Added", nrow(df_filtered), "features from Ilya\n")
} else {
  cat("  ✗ rutul_dialectology_ilya.csv not found\n")
}

# 4. Process Garik's lexicon
cat("\n4. Processing Garik's lexicon...\n")
if (file.exists("data/lexicon_moroz_full_ready.csv")) {
  max_id_in_db <- if (nrow(database) > 0) max(database$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/lexicon_moroz_full_ready.csv", show_col_types = FALSE) %>%
    filter(!is.na(value), value != "boring")
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
           updated_month, updated_year, domain, settlement, value, stimuli, answer) %>%
    arrange(feature_id)
  
  database <- rbind(database, df_filtered)
  cat("  ✓ Added", nrow(df_filtered), "lexical features\n")
} else {
  cat("  ✗ lexicon_moroz_full_ready.csv not found\n")
}

# 5. Process Nastya's verb features
cat("\n5. Processing Nastya's verb features...\n")
if (file.exists("data/verb_2025-23-08.csv")) {
  max_id_in_db <- if (nrow(database) > 0) max(database$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/verb_2025-23-08.csv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
           updated_month, updated_year, domain, settlement, value, stimuli, answer) %>%
    arrange(feature_id)
  
  database <- rbind(database, df_filtered)
  cat("  ✓ Added", nrow(df_filtered), "verb features\n")
} else {
  cat("  ✗ verb_2025-23-08.csv not found\n")
}

# 6. Process Nina's features
cat("\n6. Processing Nina's features...\n")
if (file.exists("data/NINA rutul_dialectology_merged_raw_data.csv")) {
  max_id_in_db <- if (nrow(database) > 0) max(database$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/NINA rutul_dialectology_merged_raw_data.csv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
           updated_month, updated_year, domain, settlement, value, stimuli, answer) %>%
    arrange(feature_id)
  
  database <- rbind(database, df_filtered)
  cat("  ✓ Added", nrow(df_filtered), "features from Nina\n")
} else {
  cat("  ✗ NINA rutul_dialectology_merged_raw_data.csv not found\n")
}

# 7. Process Maxim's features
cat("\n7. Processing Maxim's features...\n")
if (file.exists("data/other_features_Maks.csv")) {
  max_id_in_db <- if (nrow(database) > 0) max(database$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/other_features_Maks.csv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    select(feature_id, feature_title, feature_lexeme, feature_description, 
           collected, compiled, updated_day, updated_month, updated_year, domain, 
           settlement, value, stimuli, answer) %>%
    arrange(feature_id)
  
  database <- rbind(database, df_filtered)
  cat("  ✓ Added", nrow(df_filtered), "features from Maxim\n")
} else {
  cat("  ✗ other_features_Maks.csv not found\n")
}

# 8. Process Vanya's features
cat("\n8. Processing Vanya's features...\n")
if (file.exists("data/netkachev_Rutul_data.csv")) {
  max_id_in_db <- if (nrow(database) > 0) max(database$feature_id, na.rm = TRUE) else 0
  
  df <- read_csv("data/netkachev_Rutul_data.csv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    select(feature_id, feature_title, feature_lexeme, feature_description, 
           collected, compiled, updated_day, updated_month, updated_year, domain, 
           settlement, value, stimuli, answer) %>%
    arrange(feature_id)
  
  database <- rbind(database, df_filtered)
  cat("  ✓ Added", nrow(df_filtered), "features from Vanya\n")
} else {
  cat("  ✗ netkachev_Rutul_data.csv not found\n")
}

# 9. Process 200-words-lists
cat("\n9. Processing 200-words-lists...\n")
if (file.exists("data/rutul_dialects_200.tsv")) {
  max_id_in_db <- if (nrow(database) > 0) max(database$feature_id, na.rm = TRUE) else 0
  
  df <- read_tsv("data/rutul_dialects_200.tsv", show_col_types = FALSE) %>%
    filter(!is.na(value))
  
  df_filtered <- df %>%
    mutate(feature_id = as.double(factor(feature_title)) + max_id_in_db) %>%
    select(feature_id, feature_title, feature_lexeme, feature_description, 
           collected, compiled, updated_day, updated_month, updated_year, domain, 
           settlement, value, stimuli, answer) %>%
    arrange(feature_id)
  
  database <- rbind(database, df_filtered)
  cat("  ✓ Added", nrow(df_filtered), "200-words-list features\n")
} else {
  cat("  ✗ rutul_dialects_200.tsv not found\n")
}

# Final data cleaning
cat("\n10. Final data cleaning...\n")
if (nrow(database) > 0) {
  database_cleaned <- database %>%
    mutate_at(c("collected", "compiled", "domain", "settlement"), str_to_title) %>%
    mutate(feature_title = str_replace_all(feature_title, "'", "'"),
           feature_lexeme = str_replace_all(feature_lexeme, "'", "'"),
           value = str_replace_all(value, "\\s{2,}", " "))
  
  # Save to CSV
  write_csv(database_cleaned, "data/database_cleaned.csv", na = "")
  cat("  ✓ Saved cleaned database to data/database_cleaned.csv\n")
  
  # Save to Excel
  write_xlsx(database_cleaned, "data/database_cleaned.xlsx")
  cat("  ✓ Saved cleaned database to data/database_cleaned.xlsx\n")
  
  cat("\nFinal database summary:\n")
  cat("  Total features:", nrow(database_cleaned), "\n")
  cat("  Unique feature titles:", length(unique(database_cleaned$feature_title)), "\n")
  cat("  Unique settlements:", length(unique(database_cleaned$settlement)), "\n")
  
} else {
  cat("  ✗ No data was processed successfully\n")
}

cat("\nDatabase cleaning process completed!\n")
