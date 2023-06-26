library(tidyverse)
library(widyr)
df <- read_csv("https://raw.githubusercontent.com/LingConLab/rutul_dialectology/master/data/database.csv")

df |> 
  select(feature_title, feature_lexeme, value, settlement, value) |> 
  filter(!is.na(value),
         value != "NO DATA",
         value != "OTHER",
         value != "\\?",
         !(settlement %in% c("Tsudik", "Borch"))) |> 
  mutate(value = str_split(value, ";")) |> 
  unnest_longer(value) |> 
  mutate(value = str_squish(value)) |> 
  group_by(feature_title, feature_lexeme, settlement) |> 
  ungroup() |> 
  arrange(feature_title, feature_lexeme, settlement)  |> 
  distinct(settlement, value, feature_title, feature_lexeme) |> 
  mutate(feature_lexeme = ifelse(is.na(feature_lexeme), "", feature_lexeme),
         merged_value = str_c(feature_title, feature_lexeme, value)) |> 
  select(settlement, merged_value) |> 
  pairwise_count(settlement, merged_value) ->
  df_pairwise_total

df |> 
  select(feature_title, feature_lexeme, value, settlement, value) |> 
  filter(!is.na(value),
         !(settlement %in% c("Tsudik", "Borch"))) |> 
  group_by(feature_title, feature_lexeme, settlement) |> 
  ungroup() |> 
  arrange(feature_title, feature_lexeme, settlement)  |> 
  distinct(settlement, value, feature_title, feature_lexeme) |> 
  mutate(feature_lexeme = ifelse(is.na(feature_lexeme), "", feature_lexeme),
         merged_value = str_c(feature_title, feature_lexeme)) |> 
  select(settlement, merged_value) |> 
  pairwise_count(settlement, merged_value) |> 
  rename(total = n) ->
  df_pairwise_within_construction

df_pairwise_total |> 
  left_join(df_pairwise_within_construction) |> 
  mutate(n = ifelse(n > total, total, n)) |> 
  mutate(percentage = round(n/total*100, 2)) |> 
  select(item1, item2, percentage) |> 
  ggplot(aes(item1, item2))+
  geom_tile(aes(fill = percentage), colour = "white") +
  geom_text(aes(label = str_c(percentage, "%")), colour = "white") +
  scale_fill_gradient(low = "lightblue", high = "navy")+
  coord_fixed()+
  labs(x = "", y = "", title = "Heatmap with all stimuli") +
  theme(legend.position = "bottom")


df_pairwise_total |> 
  left_join(df_pairwise_within_construction) |>
  mutate(n = ifelse(n > total, total, n)) |> 
  mutate(percentage = 100-round(n/total*100, 2)) |> 
  select(item1, item2, percentage) |> 
  pivot_wider(names_from = item2, values_from = percentage) |> 
  arrange(item1) |> 
  select(-item1) |> 
  as.dist() ->
  dist_gold_standard

library("ape")
dist_gold_standard |>   
  hclust() |> 
  as.phylo() %>% 
  plot(main = "Clusterization with all stimuli",
       cex = 1.5,
       font = 2)

library(phangorn)
dist_gold_standard |> 
  neighborNet() |> 
  plot()
title(main = "neighborNet for all stimuli")
