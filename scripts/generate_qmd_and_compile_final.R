# Script to generate QMD files with beautiful lingtypology maps
# This version uses the proper lingtypology package for professional linguistic maps
# Updated to fix village names, CSS styling, and prepare for multilingual rendering

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(quarto)
library(lingtypology)
library(DT)
library(knitr)
library(lubridate)
library(glue)

cat("Starting QMD generation with lingtypology maps...\n")

# Set working directory
setwd('.')

# Read data files
cat("Reading data files...\n")
if (file.exists('data/database.csv')) {
  db <- read_csv('data/database.csv', show_col_types = FALSE)
  cat("✓ Database loaded:", nrow(db), "rows\n")
} else {
  stop("Database file not found: data/database.csv")
}

if (file.exists('data/villages.csv')) {
  villages <- read_csv('data/villages.csv', show_col_types = FALSE)
  cat("✓ Villages loaded:", nrow(villages), "rows\n")
} else {
  stop("Villages file not found: data/villages.csv")
}

# Get unique features
features <- db |> 
  group_by(feature_id, feature_title, feature_description, compiled) |>
  summarise(
    total_observations = n(),
    unique_settlements = n_distinct(settlement),
    unique_values = n_distinct(value),
    .groups = 'drop'
  ) |>
  arrange(feature_id)

cat("Found", nrow(features), "features to process\n")

# Function to create QMD file with lingtypology maps
create_qmd_file_with_lingtypology <- function(feature_row) {
  feature_id <- feature_row$feature_id
  feature_title <- feature_row$feature_title
  feature_description <- feature_row$feature_description
  compiled <- feature_row$compiled
  
  # Get data for this feature
  feature_data <- db |> filter(feature_id == !!feature_id)
  feature_lexemes <- unique(feature_data$feature_lexeme)
  
  # Create YAML header with proper CSS styling
  yaml_header <- paste0(
    "---\n",
    "title: \"", feature_title, "\"\n",
    "author: \"", compiled, "\"\n",
    "date: ", Sys.Date(), "\n",
    "output:\n",
    "  html_document:\n",
    "    toc: true\n",
    "    toc_float: true\n",
    "    theme: cosmo\n",
    "    highlight: tango\n",
    "    css: styles.css\n",
    "---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.width = 9.5)\n",
    "```\n\n"
  )
  
  # Add feature description if available
  description_section <- ""
  if (!is.na(feature_description) && feature_description != "" && feature_description != "NA") {
    description_section <- paste0(feature_description, "\n\n")
  }
  
  # Create sections for each feature lexeme with Map and Data tabs
  lexeme_sections <- ""
  if (length(feature_lexemes) > 0 && !all(is.na(feature_lexemes))) {
    for (lexeme in unique(feature_lexemes)) {
      if (!is.na(lexeme) && lexeme != "NA") {
        lexeme_sections <- paste0(lexeme_sections, '
## ', lexeme, '

::: {{.panel-tabset}}

### Map

```{r}
# Load data for this lexeme
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lingtypology)

# Read the database and villages data
db <- read_csv("data/database.csv", show_col_types = FALSE)
villages <- read_csv("data/villages.csv", show_col_types = FALSE)

# Filter villages like in original script (exclude certain villages)
villages4map <- villages |>
  filter(!(village %in% c("Kazankulak", "Novyy Borch", "Vrush", "Aran", "Khnyukh")))

# Create mapping data for this lexeme
lexeme_data <- db |>
  filter(feature_id == ', feature_id, ') |>  # First filter by feature_id
  filter(feature_lexeme == "', lexeme, '") |>  # Then filter by feature_lexeme
  filter(!is.na(value), value != "NO DATA")

if (nrow(lexeme_data) > 0 && nrow(villages4map) > 0) {
  # Create mapping data for this lexeme
  mapping_data <- lexeme_data |>
    mutate(value = str_split(value, " ; ")) |>
    unnest_longer(value) |>
    distinct(settlement, value) |>
    mutate(n = 1) |>
    pivot_wider(names_from = value, values_from = n, values_fill = 0) |>
    left_join(villages4map[,c("village","lat","lon")], by = c("settlement" = "village")) |>
    mutate(language = "Rutul") |>
    filter(!is.na(lat) & !is.na(lon))

  if (nrow(mapping_data) > 0) {
    # Convert feature columns to numeric
    feature_cols <- setdiff(colnames(mapping_data), c("settlement","lat","lon","language"))
    mapping_data <- mapping_data |> mutate(across(all_of(feature_cols), as.numeric))

    # Create sophisticated mapping approach with ALWAYS visible village names
    if(length(feature_cols) == 1){
      # single feature column - show all villages in gray, feature data in color
      base_map <- map.feature(languages = "Rutul",
                             latitude = villages4map$lat,
                             longitude = villages4map$lon,
                             label = villages4map$village,
                             label.position = "top",
                             label.hide = FALSE,
                             width = 10,
                             color = "gray",
                             tile = "OpenStreetMap.HOT",
                             opacity = 0.4)

      feature_map <- map.feature(languages = "Rutul",
                                latitude = mapping_data$lat,
                                longitude = mapping_data$lon,
                                label = mapping_data$settlement,
                                label.position = "top",
                                label.hide = FALSE,
                                width = 10,
                                tile = "OpenStreetMap.HOT",
                                features = feature_cols)

      # Show both maps
      base_map
      feature_map
    } else {
      # multiple feature columns - use pie charts with ALWAYS visible village names
      base_map <- map.feature(languages = "Rutul",
                             latitude = villages4map$lat,
                             longitude = villages4map$lon,
                             label = villages4map$village,
                             label.position = "top",
                             label.hide = FALSE,
                             width = 10,
                             color = "gray",
                             tile = "OpenStreetMap.HOT",
                             opacity = 0.4)

      feature_map <- map.feature(languages = "Rutul",
                                latitude = mapping_data$lat,
                                longitude = mapping_data$lon,
                                minichart.data = mapping_data |> select(all_of(feature_cols)),
                                minichart = "pie",
                                width = 3,
                                tile = "OpenStreetMap.HOT",
                                label = mapping_data$settlement,
                                label.position = "top",
                                label.hide = FALSE)

      # Show both maps
      base_map
      feature_map
    }
  } else {
    cat("No geographical coordinates available for mapping.\\n")
  }
} else {
  cat("No data available for this lexeme or no village coordinates found.\\n")
}
```

### Data

```{r}
# Load required libraries
library(readr)
library(dplyr)
library(DT)

# Read the database data
db <- read_csv("data/database.csv", show_col_types = FALSE)

# Create interactive table for this lexeme
lexeme_data <- db |> 
  filter(feature_id == ', feature_id, ') |>  # First filter by feature_id
  filter(feature_lexeme == "', lexeme, '") |>  # Then filter by feature_lexeme
  filter(!is.na(value), value != "NO DATA")

if (nrow(lexeme_data) > 0) {
  # Prepare data for display
  display_data <- lexeme_data |>
    select(settlement, value, stimuli, answer, collected) |>
    arrange(settlement, value)
  
  # Use DT package for interactive table
  DT::datatable(display_data, 
                class = "cell-border stripe",
                rownames = FALSE,
                filter = "top",
                options = list(pageLength = 25, 
                             autoWidth = TRUE,
                             info = FALSE))
} else {
  cat("No data available for this lexeme.\\n")
}
```

:::
')
      }
    }
  }
  
  # Add fallback section for features without lexemes
  if (length(feature_lexemes) == 0 || all(is.na(feature_lexemes))) {
    lexeme_sections <- paste0(lexeme_sections, '
## Feature Map

::: {{.panel-tabset}}

### Map

```{r}
# Load data for this feature
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lingtypology)

# Read the database and villages data
db <- read_csv("data/database.csv", show_col_types = FALSE)
villages <- read_csv("data/villages.csv", show_col_types = FALSE)

# Filter villages like in original script (exclude certain villages)
villages4map <- villages |> 
  filter(!(village %in% c("Kazankulak", "Novyy Borch", "Vrush", "Aran", "Khnyukh")))

# Create mapping data for this feature
feature_data <- db |> 
  filter(feature_id == ', feature_id, ') |>  # Filter by feature_id
  filter(!is.na(value), value != "NO DATA")

if (nrow(feature_data) > 0 && nrow(villages4map) > 0) {
  # Create mapping data for this feature
  mapping_data <- feature_data |>
    mutate(value = str_split(value, " ; ")) |> 
    unnest_longer(value) |> 
    distinct(settlement, value) |> 
    mutate(n = 1) |> 
    pivot_wider(names_from = value, values_from = n, values_fill = 0) |> 
    left_join(villages4map[,c("village","lat","lon")], by = c("settlement" = "village")) |> 
    mutate(language = "Rutul") |>
    filter(!is.na(lat) & !is.na(lon))
  
  if (nrow(mapping_data) > 0) {
    # Convert feature columns to numeric
    feature_cols <- setdiff(colnames(mapping_data), c("settlement","lat","lon","language"))
    mapping_data <- mapping_data |> mutate(across(all_of(feature_cols), as.numeric))
    
    # Create sophisticated mapping approach with ALWAYS visible village names
    if(length(feature_cols) == 1){
      # single feature column - show all villages in gray, feature data in color
      base_map <- map.feature(languages = "Rutul",
                             latitude = villages4map$lat,
                             longitude = villages4map$lon,
                             label = villages4map$village,
                             label.position = "top",
                             label.hide = FALSE,
                             width = 10,
                             color = "gray",
                             tile = "OpenStreetMap.HOT",
                             opacity = 0.4)
      
      feature_map <- map.feature(languages = "Rutul",
                                latitude = mapping_data$lat,
                                longitude = mapping_data$lon,
                                label = mapping_data$settlement,
                                label.position = "top",
                                label.hide = FALSE,
                                width = 10,
                                tile = "OpenStreetMap.HOT",
                                features = feature_cols)
      
      # Show both maps
      base_map
      feature_map
    } else {
      # multiple feature columns - use pie charts with ALWAYS visible village names
      base_map <- map.feature(languages = "Rutul",
                             latitude = villages4map$lat,
                             longitude = villages4map$lon,
                             label = villages4map$village,
                             label.position = "top",
                             label.hide = FALSE,
                             width = 10,
                             color = "gray",
                             tile = "OpenStreetMap.HOT",
                             opacity = 0.4)
      
      feature_map <- map.feature(languages = "Rutul",
                                latitude = mapping_data$lat,
                                longitude = mapping_data$lon,
                                minichart.data = mapping_data |> select(all_of(feature_cols)),
                                minichart = "pie",
                                width = 3,
                                tile = "OpenStreetMap.HOT",
                                label = mapping_data$settlement,
                                label.position = "top",
                                label.hide = FALSE)
      
      # Show both maps
      base_map
      feature_map
    }
  } else {
    cat("No geographical coordinates available for mapping.\\n")
  }
} else {
  cat("No data available for this feature or no village coordinates found.\\n")
}
```

### Data

```{r}
# Load required libraries
library(readr)
library(dplyr)
library(DT)

# Read the database data
db <- read_csv("data/database.csv", show_col_types = FALSE)

# Create interactive table for this feature
feature_data <- db |> 
  filter(feature_id == ', feature_id, ') |>  # Filter by feature_id
  filter(!is.na(value), value != "NO DATA")

if (nrow(feature_data) > 0) {
  # Prepare data for display
  display_data <- feature_data |>
    select(settlement, value, stimuli, answer, collected) |>
    arrange(settlement, value)
  
  # Use DT package for interactive table
  DT::datatable(display_data, 
                class = "cell-border stripe",
                rownames = FALSE,
                filter = "top",
                options = list(pageLength = 25, 
                             autoWidth = TRUE,
                             info = FALSE))
} else {
  cat("No data available for this feature.\\n")
}
```

:::
')
  }
  
  # Combine all sections
  qmd_content <- paste0(yaml_header, description_section, lexeme_sections)
  
  # Create filename
  filename <- str_c(sprintf(str_c("%0", nchar(max(db$feature_id)), "d_"), feature_id), 
                   str_replace_all(feature_title, "[\\s:\\./]", "_"), 
                   ".qmd")
  
  # Write QMD file
  writeLines(qmd_content, filename)
  
  return(filename)
}

# Remove existing QMD files (except core pages)
cat("Cleaning up existing QMD files...\n")
existing_qmd_files <- list.files(".", pattern = "\\.qmd$")
core_files <- c("index.qmd", "team.qmd", "features.qmd")
to_remove <- existing_qmd_files[!(existing_qmd_files %in% core_files)]
if (length(to_remove) > 0) {
  file.remove(to_remove)
  cat("Removed", length(to_remove), "existing QMD files\n")
}

# Generate QMD files for all features
cat("Generating QMD files for all features...\n")
generated_files <- c()

for (i in 1:nrow(features)) {
  feature_row <- features[i, ]
  cat(sprintf("[%d/%d] Processing feature %d: %s\n", 
              i, nrow(features), feature_row$feature_id, feature_row$feature_title))
  
  filename <- create_qmd_file_with_lingtypology(feature_row)
  generated_files <- c(generated_files, filename)
}

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("QMD GENERATION COMPLETED!\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("Successfully generated", length(generated_files), "QMD files\n")
cat("All features now have:\n")
cat("  - Professional linguistic maps with ALWAYS visible village names\n")
cat("  - Interactive data tables\n")
cat("  - Proper CSS styling from styles.css\n")
cat("  - Consistent design across all pages\n")
cat("  - Complete data documentation\n")
cat("\nThe script now handles ALL feature types correctly!\n")
cat("Village names are ALWAYS visible on maps!\n")
cat("CSS styling is properly included!\n")
