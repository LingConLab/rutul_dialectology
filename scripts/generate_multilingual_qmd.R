# Script to generate multilingual QMD files with proper language content
# This creates separate English and Russian versions for features

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

cat("Starting multilingual QMD generation...\n")

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

# Function to create English QMD file
create_english_qmd <- function(feature_row) {
  feature_id <- feature_row$feature_id
  feature_title <- feature_row$feature_title
  feature_description <- feature_row$feature_description
  compiled <- feature_row$compiled
  
  # Get data for this feature
  feature_data <- db |> filter(feature_id == !!feature_id)
  feature_lexemes <- unique(feature_data$feature_lexeme)
  
  # Create YAML header for English version
  yaml_header <- paste0(
    "---\n",
    "title: \"", feature_title, "\"\n",
    "author: \"", compiled, "\"\n",
    "date: ", Sys.Date(), "\n",
    "format: html\n",
    "css: styles.css\n",
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

::: {.panel-tabset}

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

::: {.panel-tabset}

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
  
  # Create filename for English version
  filename <- str_c(sprintf(str_c("%0", nchar(max(db$feature_id)), "d_"), feature_id), 
                   str_replace_all(feature_title, "[\\s:\\./]", "_"), 
                   ".qmd")
  
  # Write English QMD file
  writeLines(qmd_content, filename)
  
  return(filename)
}

# Function to create Russian QMD file
create_russian_qmd <- function(feature_row) {
  feature_id <- feature_row$feature_id
  feature_title <- feature_row$feature_title
  feature_description <- feature_row$feature_description
  compiled <- feature_row$compiled
  
  # Get data for this feature
  feature_data <- db |> filter(feature_id == !!feature_id)
  feature_lexemes <- unique(feature_data$feature_lexeme)
  
  # Create YAML header for Russian version
  yaml_header <- paste0(
    "---\n",
    "title: \"", feature_title, "\"\n",
    "author: \"", compiled, "\"\n",
    "date: ", Sys.Date(), "\n",
    "format: html\n",
    "lang: ru\n",
    "css: styles.css\n",
    "---\n\n",
    "```{r setup, include=FALSE}\n",
    "knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.width = 9.5)\n",
    "```\n\n"
  )
  
  # Add feature description if available (in Russian if available)
  description_section <- ""
  if (!is.na(feature_description) && feature_description != "" && feature_description != "NA") {
    # For now, use the same description - in a real implementation, you'd have Russian translations
    description_section <- paste0(feature_description, "\n\n")
  }
  
  # Create sections for each feature lexeme with Map and Data tabs (in Russian)
  lexeme_sections <- ""
  if (length(feature_lexemes) > 0 && !all(is.na(feature_lexemes))) {
    for (lexeme in unique(feature_lexemes)) {
      if (!is.na(lexeme) && lexeme != "NA") {
        lexeme_sections <- paste0(lexeme_sections, '
## ', lexeme, '

::: {.panel-tabset}

### Карта

```{r}
# Load data for this lexeme
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lingtypology)

# Read the database and villages data
db <- read_csv("../data/database.csv", show_col_types = FALSE)
villages <- read_csv("../data/villages.csv", show_col_types = FALSE)

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
    cat("Географические координаты недоступны для картографирования.\\n")
  }
} else {
  cat("Данные для этой лексемы недоступны или координаты деревень не найдены.\\n")
}
```

### Данные

```{r}
# Load required libraries
library(readr)
library(dplyr)
library(DT)

# Read the database data
db <- read_csv("../data/database.csv", show_col_types = FALSE)

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
  cat("Данные для этой лексемы недоступны.\\n")
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
## Карта

::: {.panel-tabset}

### Карта

```{r}
# Load data for this feature
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(lingtypology)

# Read the database and villages data
db <- read_csv("../data/database.csv", show_col_types = FALSE)
villages <- read_csv("../data/villages.csv", show_col_types = FALSE)

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
    cat("Географические координаты недоступны для картографирования.\\n")
  }
} else {
  cat("Данные для этого признака недоступны или координаты деревень не найдены.\\n")
}
```

### Данные

```{r}
# Load required libraries
library(readr)
library(dplyr)
library(DT)

# Read the database data
db <- read_csv("../data/database.csv", show_col_types = FALSE)

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
  cat("Данные для этого признака недоступны.\\n")
}
```

:::
')
  }
  
  # Combine all sections
  qmd_content <- paste0(yaml_header, description_section, lexeme_sections)
  
  # Create filename for Russian version
  filename <- str_c("ru/", sprintf(str_c("%0", nchar(max(db$feature_id)), "d_"), feature_id), 
                   str_replace_all(feature_title, "[\\s:\\./]", "_"), 
                   ".qmd")
  
  # Write Russian QMD file
  writeLines(qmd_content, filename)
  
  return(filename)
}

# Create directories
cat("Creating directories...\n")
if (!dir.exists("ru")) {
  dir.create("ru")
  cat("Created ru/ directory\n")
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

# Remove existing Russian QMD files
if (dir.exists("ru")) {
  russian_qmd_files <- list.files("ru", pattern = "\\.qmd$", full.names = TRUE)
  if (length(russian_qmd_files) > 0) {
    file.remove(russian_qmd_files)
    cat("Removed", length(russian_qmd_files), "existing Russian QMD files\n")
  }
}

# Generate QMD files for all features in both languages
cat("Generating multilingual QMD files for all features...\n")
english_files <- c()
russian_files <- c()

for (i in 1:nrow(features)) {
  feature_row <- features[i, ]
  cat(sprintf("[%d/%d] Processing feature %d: %s\n", 
              i, nrow(features), feature_row$feature_id, feature_row$feature_title))
  
  # Create English version
  english_filename <- create_english_qmd(feature_row)
  english_files <- c(english_files, english_filename)
  
  # Create Russian version
  russian_filename <- create_russian_qmd(feature_row)
  russian_files <- c(russian_files, russian_filename)
}

cat("\n", paste(rep("=", 60), collapse=""), "\n")
cat("MULTILINGUAL QMD GENERATION COMPLETED!\n")
cat(paste(rep("=", 60), collapse=""), "\n")
cat("Successfully generated", length(english_files), "English QMD files\n")
cat("Successfully generated", length(russian_files), "Russian QMD files\n")
