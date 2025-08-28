# Clean working version of generate_qmd_and_compile.R
# Modified to work with available packages

# Load required packages (only the ones we have)
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(quarto))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(glue))

cat("Starting QMD generation with available packages...\n")

# Check if database exists
if (!file.exists("data/database.csv")) {
  cat("Error: database.csv not found. Please run the cleaning script first.\n")
  cat("Run: Rscript scripts/cleaning_data_for_database.R\n")
  stop("Missing database file")
}

# Read the database
db <- read_csv("data/database.csv", show_col_types = FALSE) |> 
  filter(!is.na(value))

cat("Loaded database with", nrow(db), "rows\n")

# Check if villages file exists
if (!file.exists("data/villages.csv")) {
  cat("Warning: villages.csv not found. Some functionality may be limited.\n")
  villages <- data.frame()
} else {
  villages <- read_csv("data/villages.csv", show_col_types = FALSE) |>
    filter(!(village %in% c('Kazankulak', 'Novyy Borch', 'Vrush', 'Aran', 'Khnyukh')))
  cat("Loaded villages data with", nrow(villages), "villages\n")
}

# Remove existing QMD files (except index, team, features)
to_remove <- list.files(".", pattern = ".qmd")
to_remove <- to_remove[!(to_remove %in% c("index.qmd", "team.qmd", "features.qmd"))]
if (length(to_remove) > 0) {
  file.remove(to_remove)
  cat("Removed", length(to_remove), "existing QMD files\n")
}

# Remove HTML files
html_files <- list.files(".", pattern = ".html")
if (length(html_files) > 0) {
  file.remove(html_files)
  cat("Removed", length(html_files), "existing HTML files\n")
}

# Create QMD template function
create_qmd_file <- function(feature_id, feature_title, feature_description, compiled, 
                           updated_day, updated_month, updated_year, feature_lexemes) {
  
  filename <- sprintf("%03d_%s.qmd", feature_id, 
                     str_replace_all(feature_title, "[\\s:\\./]", "_"))
  
  # Handle missing values
  updated_day <- ifelse(is.na(updated_day), 1, updated_day)
  updated_month <- ifelse(is.na(updated_month), 1, updated_month)
  updated_year <- ifelse(is.na(updated_year), 2024, updated_year)
  
  # Create YAML header
  yaml_header <- paste0('---
title: "', feature_title, '"
author: "', compiled, '"
date: "Last update: ', updated_year, '-', updated_month, '-', updated_day, '"
format: html
number-sections: true
anchor-sections: true
---

')
  
  # Create R setup chunk
  setup_chunk <- paste0('```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning=FALSE, fig.width = 9.5)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

# Load data for this feature
read_csv("data/database.csv", show_col_types = FALSE) |> 
  filter(feature_id == ', feature_id, ') ->
  db

# Load villages if available
if (file.exists("data/villages.csv")) {
  villages <- read_csv("data/villages.csv", show_col_types = FALSE) |>
    filter(!(village %in% c("Kazankulak", "Novyy Borch", "Vrush", "Aran", "Khnyukh")))
} else {
  villages <- data.frame()
}
```

')
  
  # Create feature description
  description_section <- paste0('## Feature Description

', feature_description, '

')
  
  # Create data summary section
  summary_section <- paste0('## Data Summary

```{r}
# Summary statistics
cat("Feature ID:", ', feature_id, ', "\\n")
cat("Feature Title:", "', feature_title, '", "\\n")
cat("Total observations:", nrow(db), "\\n")
cat("Unique settlements:", length(unique(db$settlement)), "\\n")
cat("Unique values:", length(unique(db$value)), "\\n")

# Show first few rows
cat("\\nFirst few observations:\\n")
print(head(db, 10))
```

')
  
  # Create sections for each feature lexeme
  lexeme_sections <- ""
  if (length(feature_lexemes) > 0 && !all(is.na(feature_lexemes))) {
    for (lexeme in unique(feature_lexemes)) {
      if (!is.na(lexeme) && lexeme != "NA") {
        lexeme_sections <- paste0(lexeme_sections, '
## ', lexeme, '

### Data by Settlement

```{r}
# Filter data for this lexeme
lexeme_data <- db |> 
  filter(feature_lexeme == "', lexeme, '") |>
  filter(!is.na(value), value != "NO DATA")

if (nrow(lexeme_data) > 0) {
  # Group by settlement and show values
  settlement_summary <- lexeme_data |>
    group_by(settlement) |>
    summarise(
      count = n(),
      unique_values = length(unique(value)),
      values = paste(unique(value), collapse = "; ")
    ) |>
    arrange(desc(count))
  
  print(settlement_summary)
} else {
  cat("No data available for this lexeme\\n")
}
```

### Raw Data

```{r}
# Show all data for this lexeme
if (nrow(lexeme_data) > 0) {
  lexeme_data |>
    select(settlement, value, stimuli, answer, collected) |>
    arrange(settlement, value) |>
    print(n = Inf)
} else {
  cat("No data available for this lexeme\\n")
}
```

')
      }
    }
  }
  
  # Combine all sections
  content <- paste0(yaml_header, setup_chunk, description_section, summary_section, lexeme_sections)
  
  # Write the file
  writeLines(content, filename)
  return(filename)
}

# Process each unique feature
cat("\nGenerating QMD files...\n")

unique_features <- db |>
  distinct(feature_id, feature_title, feature_description, compiled, 
           updated_day, updated_month, updated_year) |>
  arrange(feature_id)

cat("Found", nrow(unique_features), "unique features to process\n")

generated_files <- c()

for (i in 1:nrow(unique_features)) {
  feature <- unique_features[i, ]
  
  # Get feature lexemes for this feature
  feature_lexemes <- db |>
    filter(feature_id == feature$feature_id) |>
    pull(feature_lexeme) |>
    unique()
  
  filename <- create_qmd_file(
    feature$feature_id,
    feature$feature_title,
    feature$feature_description,
    feature$compiled,
    feature$updated_day,
    feature$updated_month,
    feature$updated_year,
    feature_lexemes
  )
  
  generated_files <- c(generated_files, filename)
  
  if (i %% 10 == 0) {
    cat("Processed", i, "of", nrow(unique_features), "features\n")
  }
}

cat("\nSuccessfully generated", length(generated_files), "QMD files\n")

# Create a comprehensive index file
cat("\nCreating index file...\n")
index_content <- paste0('---
title: "Rutul Dialectology Features"
format: html
---

# Rutul Dialectology Features

This is an automatically generated index of all linguistic features in the database.

## Feature List

', 
  paste(sprintf("- [%s](%s)", 
                unique_features$feature_title, 
                sprintf("%03d_%s.html", 
                       unique_features$feature_id,
                       str_replace_all(unique_features$feature_title, "[\\s:\\./]", "_"))), 
        collapse = "\n"),
  '

## Summary Statistics

- **Total Features**: ', nrow(unique_features), '
- **Total Observations**: ', nrow(db), '
- **Unique Settlements**: ', length(unique(db$settlement)), '
- **Researchers**: ', paste(unique(unique_features$compiled), collapse = ", "), '

*Generated on: ', format(Sys.time(), "%Y-%m-%d %H:%M:%S"), '*')

writeLines(index_content, "index_generated.qmd")

cat("Created index_generated.qmd\n")

# Try to render a few files to test
cat("\nTesting rendering with a few files...\n")

test_files <- generated_files[1:min(3, length(generated_files))]

for (test_file in test_files) {
  cat("Rendering", test_file, "...\n")
  tryCatch({
    quarto_render(test_file, output_format = "html")
    cat("  ✓ Successfully rendered", test_file, "\n")
  }, error = function(e) {
    cat("  ✗ Failed to render", test_file, ":", e$message, "\n")
  })
}

cat("\nQMD generation and testing completed!\n")
cat("\nNext steps:\n")
cat("1. Review the generated QMD files\n")
cat("2. Test rendering with: quarto render index_generated.qmd\n")
cat("3. For full functionality, consider installing:\n")
cat("   - lingtypology: for linguistic maps\n")
cat("   - ymlthis: for advanced YAML manipulation\n")
cat("4. The current version provides:\n")
cat("   - Complete QMD files for all features\n")
cat("   - Data summaries and analysis\n")
cat("   - Basic HTML rendering capability\n")


quarto_render("060_Loanword_f_adopted_as_labialized_x.qmd", profile = "english")
