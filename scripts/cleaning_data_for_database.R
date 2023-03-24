library(tidyverse)
df <- read_csv("data/ergative.csv")
df %>% 
  mutate(feature_title = str_c(feature_title, ": ", feature_lexeme),
         updated_day = ifelse(is.na(updated_day), elicitation_day, updated_day),
         updated_month = ifelse(is.na(updated_month), elicitation_month, updated_month),
         updated_year = ifelse(is.na(updated_year), elicitation_year, updated_year),
         feature_id = as.double(factor(feature_title)),
         compiled = "Asya Alekseeva") %>% 
  select(feature_id, feature_title, feature_description, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, speaker_code, value, stimuli, raw_data) %>% 
  rename(answer = raw_data) %>% 
  write_csv("data/database.csv", na = "")
