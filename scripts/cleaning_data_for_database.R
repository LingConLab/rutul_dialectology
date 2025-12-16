#setwd('rutul_dialectology-master')
setwd('.')

library(conflicted)
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(writexl)

# asya and kostya -----------------------------------------------------------
df <- read_csv("data/noun_features_coding.csv", show_col_types = FALSE)
df |>
  filter(to_map == 1) |>
  mutate(feature_id = as.double(factor(feature_title)),
         compiled = "Asya Alekseeva") |>
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |>
  arrange(feature_id) |>
  write_csv("data/database.csv", na = "")

# Nikita's phonetics ------------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id", show_col_types = FALSE) |>
  distinct() |>
  filter(feature_id == max(feature_id)) |>
  pull(feature_id) ->
  max_id_in_db

df <- read_csv("data/nikita_phonology_3.csv", show_col_types = FALSE)
df |>
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |>
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |>
  arrange(feature_id) |>
  write_csv("data/database.csv", na = "", append = TRUE)

# Ilya's other ------------------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id", show_col_types = FALSE) |>
  distinct() |>
  filter(feature_id == max(feature_id)) |>
  pull(feature_id) ->
  max_id_in_db

df <- read_csv("data/rutul_dialectology_ilya.csv", show_col_types = FALSE)
df |>
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |>
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |>
  arrange(feature_id) |>
  write_csv("data/database.csv", na = "", append = TRUE)

# add Garik's lexicon -----------------------------------------------------
read_csv("data/database.csv", col_select = "feature_id", show_col_types = FALSE) |>
  distinct() |>
  filter(feature_id == max(feature_id)) |>
  pull(feature_id) ->
  max_id_in_db

read_csv("data/lexicon_moroz_full_ready.csv", show_col_types = FALSE) |>
  filter(!is.na(value),
         value != "boring") ->
  df

df |>
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |>
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |>
  arrange(feature_id) |>
  write_csv("data/database.csv", na = "", append = TRUE)


# add Kostya's oblique ----------------------------------------------------
#read_csv("data/database.csv", col_select = "feature_id") |>
#  distinct() |>
#  filter(feature_id == max(feature_id)) |>
#  pull(feature_id) ->
#  max_id_in_db

#read_csv("data/kostya_features.csv") |>
#  filter(!is.na(value)) ->
#  df

#df |>
#  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |>
#  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
#         updated_month, updated_year, domain, settlement, value, stimuli, answer) |>
#  arrange(feature_id) |>
#  write_csv("data/database.csv", na = "", append = TRUE)

# add Nastya's verb -------------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id", show_col_types = FALSE) |>
  distinct() |>
  filter(feature_id == max(feature_id)) |>
  pull(feature_id) ->
  max_id_in_db

read_csv("data/verb_2025-23-08.csv", show_col_types = FALSE) |>
  filter(!is.na(value)) ->
  df

df |>
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |> 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |> 
  arrange(feature_id) |> 
  write_csv("data/database.csv", na = "", append = TRUE)

# add Nina's verb ---------------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id", show_col_types = FALSE) |>
  distinct() |>
  filter(feature_id == max(feature_id)) |>
  pull(feature_id) ->
  max_id_in_db

read_csv("data/NINA rutul_dialectology_merged_raw_data.csv", show_col_types = FALSE) |>
  filter(!is.na(value)) ->
  df

df |>
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |> 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |> 
  arrange(feature_id) |> 
  write_csv("data/database.csv", na = "", append = TRUE)

# add Maxim's demonstratives ----------------------------------------------
read_csv("data/database.csv", col_select = "feature_id", show_col_types = FALSE) |>
  distinct() |>
  filter(feature_id == max(feature_id)) |>
  pull(feature_id) ->
  max_id_in_db

read_csv("data/other_features_Maks.csv", show_col_types = FALSE) |>
  filter(!is.na(value)) ->
  df

df |>
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |> 
  select(feature_id, feature_title, feature_lexeme, feature_description, 
         collected, compiled, updated_day, updated_month, updated_year, domain, 
         settlement, value, stimuli, answer) |> 
  arrange(feature_id) |> 
  write_csv("data/database.csv", na = "", append = TRUE)


# add Vanya's -------------------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id", show_col_types = FALSE) |>
  distinct() |>
  filter(feature_id == max(feature_id)) |>
  pull(feature_id) ->
  max_id_in_db

read_csv("data/netkachev_Rutul_data.csv", show_col_types = FALSE) |>
  filter(!is.na(value)) ->
  df

df |>
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |> 
  select(feature_id, feature_title, feature_lexeme, feature_description, 
         collected, compiled, updated_day, updated_month, updated_year, domain, 
         settlement, value, stimuli, answer) |> 
  arrange(feature_id)  |> 
  write_csv("data/database.csv", na = "", append = TRUE)


# add 200-words-lists -----------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id", show_col_types = FALSE) |>
  distinct() |>
  filter(feature_id == max(feature_id)) |>
  pull(feature_id) ->
  max_id_in_db

read_tsv("data/rutul_dialects_200.tsv", show_col_types = FALSE) |>
  filter(!is.na(value)) ->
  df

df |>
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |> 
  select(feature_id, feature_title, feature_lexeme, feature_description, 
         collected, compiled, updated_day, updated_month, updated_year, domain, 
         settlement, value, stimuli, answer) |> 
  arrange(feature_id)  |> 
  write_csv("data/database.csv", na = "", append = TRUE)

# after_merge_fix ---------------------------------------------------------

read_csv("data/database.csv", show_col_types = FALSE) |> 
  mutate_at(c("collected", "compiled", "domain", "settlement"), str_to_title) |> 
  mutate(feature_title = str_replace_all(feature_title, "'", "'"),
         feature_lexeme = str_replace_all(feature_lexeme, "'", "'"),
         value = str_replace_all(value, "\\s{2,}", " ")) |> 
  write_csv("data/database.csv", na = "")

read_csv("data/database.csv", show_col_types = FALSE) |> 
  write_xlsx("data/database.xlsx")

cat("Database processing completed successfully!\n")

