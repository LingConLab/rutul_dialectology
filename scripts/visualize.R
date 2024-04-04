library(tidyverse)
library(widyr)

# all
read_csv("https://raw.githubusercontent.com/LingConLab/rutul_dialectology/master/data/database.csv") ->
  df

for_plot_title <- "with all stimuli"

# without 200
# read_csv("https://raw.githubusercontent.com/LingConLab/rutul_dialectology/master/data/database.csv") |>
#   filter(domain != "Basic Lexicon") ->
#   df
# 
# for_plot_title <- "without 200 lists"

# just 200
# read_csv("https://raw.githubusercontent.com/LingConLab/rutul_dialectology/master/data/database.csv") |>
#   filter(domain == "Basic Lexicon") ->
#   df
# 
# for_plot_title <- "based on 200 lists"

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
  mutate(percentage = round(n/total*100, 1)) |> 
  select(item1, item2, percentage) |> 
  group_by(item1) |>
  mutate(sum_i1 = sum(percentage)) |> 
  ungroup() |>
  group_by(item2) |>
  mutate(sum_i2 = sum(percentage)) |> 
  ungroup() |>
  mutate(item1 = fct_reorder(item1, sum_i1),
         item2 = fct_reorder(item2, sum_i2)) |> 
  ggplot(aes(item1, item2))+
  geom_tile(aes(fill = percentage), colour = "white") +
  geom_text(aes(label = str_c(percentage, "%")), colour = "white") +
  scale_fill_gradient(low = "lightblue", high = "navy")+
  coord_fixed()+
  labs(x = "", y = "", title = str_c("Heatmap ", for_plot_title)) +
  theme(legend.position = "bottom")

df_pairwise_total |> 
  left_join(df_pairwise_within_construction) |>
  mutate(n = ifelse(n > total, total, n)) |> 
  mutate(percentage = 100-n/total*100) |> 
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
  plot(main = str_c("Clusterization ", for_plot_title),
       cex = 1.5,
       font = 2)

library(phangorn)
dist_gold_standard |> 
  neighborNet() |> 
  plot()
title(main = str_c("neighborNet ", for_plot_title))

dist_gold_standard |>
  cmdscale(k = 3) |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  rename(dim1 = V1,
         dim2 = V2,
         dim3 = V3,
         settlement = rowname) |>
  mutate(dim1 = scale(dim1, center = min(dim1), scale = max(dim1) - min(dim1)),
         dim2 = scale(dim2, center = min(dim2), scale = max(dim2) - min(dim2)),
         dim3 = scale(dim3, center = min(dim3), scale = max(dim3) - min(dim3)))  |> 
  mutate(rgb = rgb(dim1, dim2, dim3)) |> 
  select(settlement, rgb) |> 
  arrange(settlement)->
  settlement_rgb
  
dist_gold_standard |>
  cmdscale(k = 2) |> 
  as.data.frame() |> 
  rownames_to_column() |> 
  rename(dim1 = V1,
         dim2 = V2,
         settlement = rowname) |> 
  left_join(settlement_rgb) |> 
  ggplot(aes(dim1, dim2, label = settlement, fill = settlement))+
  geom_point()+
  ggrepel::geom_label_repel(color = "white")+
  scale_fill_manual(values=settlement_rgb$rgb)+
  labs(title = str_c("MDS ", for_plot_title))+
  theme(legend.position = "none")

villages <- read_csv("data/villages.csv")
villages |> 
  left_join(settlement_rgb, by = c("village" = "settlement")) |> 
  filter(!is.na(rgb)) ->
  for_rgb_map

library(lingtypology)
map.feature(languages = "Rutul",
            features = for_rgb_map$village,
            latitude = for_rgb_map$lat,
            longitude = for_rgb_map$lon,
            color = for_rgb_map$rgb, 
            legend = FALSE,
            label = for_rgb_map$village,
            label.hide = FALSE,
            tile = 'Esri.WorldGrayCanvas', 
            minimap = TRUE)

# CA ----------------------------------------------------------------------

df |> 
  filter(value != "NO DATA",
         value != "OTHER") |> 
  count(feature_id, feature_title, feature_lexeme, settlement, value) |> 
  mutate(feature_lexeme = ifelse(is.na(feature_lexeme), "", feature_lexeme),
         feature = str_c(feature_id, "_", feature_title, "_", feature_lexeme, ": ", value)) |> 
  select(settlement, feature, n) |> 
  pivot_wider(names_from = feature, values_from = n, values_fill = 0) |> 
  column_to_rownames("settlement") |> 
  ca::ca() ->
  ca

ca$rowcoord |> 
  as.data.frame() |> 
  rownames_to_column("settlement") |> 
  ggplot(aes(Dim1, Dim2, label = settlement))+
  geom_point()+
  ggrepel::geom_label_repel()+
  theme_minimal()+
  labs(title = str_c("CA ", for_plot_title))

# entropy_of_features -----------------------------------------------------

df |> 
  filter(value != "NO DATA",
         value != "OTHER") |> 
  count(feature_id, feature_title, feature_lexeme, compiled, value) |> 
  group_by(feature_id, feature_title, feature_lexeme, compiled) |> 
  mutate(ratio = n/sum(n)) |> 
  summarise(entropy = -sum(ratio*log2(ratio)),
            number_of_values = n()) |> 
  arrange(-entropy) |> # writexl::write_xlsx("~/Desktop/features_by_entropy.xlsx")
  ggplot(aes(entropy, number_of_values))+
  geom_point()+
  theme_minimal()+
  labs(y = "number of values")
  
df |> 
  filter(feature_title == "Non-specific indefinite pronouns") |> 
  View()

# table with common value in settlement pair ------------------------------
library(tidyverse)
df <- read_csv("https://github.com/LingConLab/rutul_dialectology/raw/master/data/database.csv")

combn(unique(df$settlement), 2) |> 
  t() |> 
  as.data.frame() ->
  villages

map_dfr(1:nrow(villages), function(i){
  village_pair <- unlist(villages[i,])
  
  df |> 
    filter(!(value %in% c("NO DATA", "OTHER")),
           settlement %in% village_pair) |> 
    distinct(feature_title, feature_lexeme, value, settlement) |> 
    count(feature_title, feature_lexeme, value) |> 
    filter(n == 2) |> 
    select(-n) |> 
    mutate(village_pair = str_c(village_pair, collapse = " - "))
}) |> 
  writexl::write_xlsx("common_values_in_language_pair.xlsx")
