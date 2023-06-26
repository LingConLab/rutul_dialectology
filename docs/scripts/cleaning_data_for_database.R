# asya ergative -----------------------------------------------------------

library(tidyverse)
df <- read_csv("data/ergative_asya.csv")
df %>% 
  filter(to_map == 1) %>% 
  mutate(updated_day = 21,
         updated_month = 3,
         updated_year = 2023,
         feature_id = as.double(factor(feature_title)),
         compiled = "Asya Alekseeva") %>% 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, raw_data) %>% 
  rename(answer = raw_data) %>% 
  arrange(feature_id) %>% 
  write_csv("data/database.csv", na = "")

# Nikita's phonetics ------------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id") %>% 
  distinct() %>% 
  filter(feature_id == max(feature_id)) %>% 
  pull(feature_id) ->
  max_id_in_db

df <- read_csv("data/phonetics_nikita.csv")
df %>% 
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) %>% 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) %>% 
  arrange(feature_id) %>% 
  write_csv("data/database.csv", na = "", append = TRUE)

# Ilya's other ------------------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id") %>% 
  distinct() %>% 
  filter(feature_id == max(feature_id)) %>% 
  pull(feature_id) ->
  max_id_in_db

df <- read_csv("data/other_ilya.csv")
df %>% 
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) %>% 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) %>% 
  arrange(feature_id) %>% 
  write_csv("data/database.csv", na = "", append = TRUE)

# add Garik's lexicon -----------------------------------------------------
read_csv("data/database.csv", col_select = "feature_id") %>% 
  distinct() %>% 
  filter(feature_id == max(feature_id)) %>% 
  pull(feature_id) ->
  max_id_in_db

read_csv("data/lexicon_moroz_full.csv") %>% 
  filter(!is.na(value),
         value != "boring") ->
  df

df %>% 
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) %>% 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) %>% 
  arrange(feature_id) %>% 
  write_csv("data/database.csv", na = "", append = TRUE)


# add Kostya's oblique ----------------------------------------------------
read_csv("data/database.csv", col_select = "feature_id") %>% 
  distinct() %>% 
  filter(feature_id == max(feature_id)) %>% 
  pull(feature_id) ->
  max_id_in_db

read_csv("data/sonorant_oblique_in_r_kostya.csv") %>% 
  filter(!is.na(value)) ->
  df

df %>% 
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) %>% 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) %>% 
  arrange(feature_id) %>% 
  write_csv("data/database.csv", na = "", append = TRUE)

# add Nastya's verb -------------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id") %>% 
  distinct() %>% 
  filter(feature_id == max(feature_id)) %>% 
  pull(feature_id) ->
  max_id_in_db

readxl::read_xlsx("data/nastya_verb_review-2.xlsx") %>% 
  filter(!is.na(value)) ->
  df

df %>% 
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |> 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |> 
  arrange(feature_id) |> 
  write_csv("data/database.csv", na = "", append = TRUE)

# add Nina's verb ---------------------------------------------------------

read_csv("data/database.csv", col_select = "feature_id") %>% 
  distinct() %>% 
  filter(feature_id == max(feature_id)) %>% 
  pull(feature_id) ->
  max_id_in_db

readxl::read_xlsx("data/NINA rutul_dialectology_merged_raw_data.xlsx") %>% 
  filter(!is.na(value)) ->
  df

df %>% 
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |> 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |> 
  arrange(feature_id) |> 
  write_csv("data/database.csv", na = "", append = TRUE)

# add Kostya's and Asya's nouns -------------------------------------------
read_csv("data/database.csv", col_select = "feature_id") %>% 
  distinct() %>% 
  filter(feature_id == max(feature_id)) %>% 
  pull(feature_id) ->
  max_id_in_db

read_csv("data/noun_features_2023-05-25.csv") %>% 
  filter(!is.na(value)) ->
  df

df %>% 
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |> 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |> 
  arrange(feature_id) |> 
  write_csv("data/database.csv", na = "", append = TRUE)


# add Maxim's demonstratives ----------------------------------------------
read_csv("data/database.csv", col_select = "feature_id") %>% 
  distinct() %>% 
  filter(feature_id == max(feature_id)) %>% 
  pull(feature_id) ->
  max_id_in_db

readxl::read_xlsx("data/rutul_dialectology_Maks.xlsx") %>% 
  filter(!is.na(value)) ->
  df

df %>% 
  mutate(feature_id = as.double(factor(feature_title))+max_id_in_db) |> 
  select(feature_id, feature_title, feature_lexeme, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, answer) |> 
  arrange(feature_id) |> 
  write_csv("data/database.csv", na = "", append = TRUE)

