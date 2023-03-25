# asya ergative -----------------------------------------------------------

library(tidyverse)
df <- read_csv("data/ergative.csv")
df %>% 
  filter(to_map == 1) %>% 
  mutate(feature_title = str_c(feature_title, ": ", feature_lexeme),
         updated_day = 21,
         updated_month = 3,
         updated_year = 2023,
         feature_id = as.double(factor(feature_title)),
         compiled = "Asya Alekseeva") %>% 
  select(feature_id, feature_title, feature_description, collected, compiled, updated_day, 
         updated_month, updated_year, domain, settlement, value, stimuli, raw_data) %>% 
  rename(answer = raw_data) %>% 
  write_csv("data/database.csv", na = "")
