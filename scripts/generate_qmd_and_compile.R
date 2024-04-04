# install packages ---------------------------------------------------------
packages <- c("tidyverse", "quarto", "lingtypology", "DT", "knitr", "ymlthis", 
              "lubridate", "stringr")

to_install <- packages[!(packages %in% installed.packages())]

if(length(to_install) > 0){
  install.packages(to_install, dependencies = TRUE)
}

rm(packages, to_install)

# RUN TESTS ----------------------------------------------------------------
# testthat::test_dir("tests")

# GENERATE RMD ------------------------------------------------------------
suppressPackageStartupMessages(library(tidyverse))

db <- read_csv("data/database.csv", show_col_types = FALSE) |> filter(!is.na(value))

# create variable with leading 0 -------------------------------------------
db$filename <- str_c(sprintf(str_c("%0", nchar(max(db$feature_id)), "d_"), 
                             db$feature_id),
                     str_replace_all(db$feature_title, "[\\s:\\./]", "_"),
                     ".qmd")

to_remove <- list.files(".", pattern = ".qmd")
to_remove <- to_remove[!(to_remove %in% c("about.qmd", "index.qmd", "team.qmd", "features.qmd"))]
file.remove(c(to_remove, list.files(".", pattern = ".html")))

options(ymlthis.rmd_body = "
```{r, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning=FALSE, fig.width = 9.5)
library(tidyverse)
library(lingtypology)
read_csv('data/database.csv', show_col_types = FALSE) |> 
  filter(feature_id == PUT_FEATURE_ID_HERE) ->
  db
read_csv('data/villages.csv') |> 
  filter(!(village %in% c('Kazankulak', 'Novyy Borch', 'Vrush', 'Aran', 'Khnyukh'))) ->
  villages
  
villages |> 
  filter(!(village %in% c('Tsudik', 'Borch'))) ->
  villages4map
  
```

PUT_FEATURE_DESCRIPTION_HERE

")

make_section <- function(section_title){
  glue::glue("
## {section_title}

::: {{.panel-tabset}}

### Map

```{{r}}
db |> 
  filter(feature_lexeme == '{section_title}')  |>
  filter(!is.na(value),
         value != 'NO DATA') |>
  mutate(value = str_split(value, ' ; ')) |> 
  unnest_longer(value) |> 
  distinct(settlement, value) |> 
  mutate(n = 1) |> 
  pivot_wider(names_from = value, values_from = n, values_fill = 0) |> 
  left_join(villages[,c('village', 'lat', 'lon')], c('settlement' = 'village')) |> 
  mutate(language = 'Rutul') ->
  for_map
  
if(length(for_map) == 5){{
map.feature(languages = 'Rutul',
            latitude = villages4map$lat,
            longitude = villages4map$lon,
            label = villages4map$village,
            label.position = 'top',
            label.hide = FALSE,
            width = 10,
            color = 'gray',
            tile = 'OpenStreetMap.HOT',
            opacity = 0.4) |> 
  map.feature(languages = for_map$language,
              latitude = for_map$lat,
              longitude = for_map$lon,
              label = for_map$settlement,
              label.position = 'top',
              label.hide = FALSE,
              width = 10,
              tile = 'OpenStreetMap.HOT',
              features = colnames(for_map)[2],
              pipe.data = _)  
}} else {{
map.feature(languages = 'Rutul',
            latitude = villages4map$lat,
            longitude = villages4map$lon,
            label = villages4map$village,
            label.position = 'top',
            label.hide = FALSE,
            width = 10,
            color = 'gray',
            tile = 'OpenStreetMap.HOT',
            opacity = 0.4) |> 
  map.feature(languages = for_map$language,
            latitude = for_map$lat,
            longitude = for_map$lon,
            minichart.data = for_map |> select(-settlement, -lat, -lon, -language),
            minichart = 'pie', 
            width = 3,
            tile = 'OpenStreetMap.HOT',
            pipe.data = _)
}}
```

### Data

```{{r}}
db |> 
  filter(feature_lexeme == '{section_title}') |> 
  select(settlement, value, stimuli, answer, collected) |> 
  DT::datatable(class = 'cell-border stripe', 
    rownames = FALSE, 
    filter = 'top', 
    extensions = 'Buttons',
    options = list(pageLength = 42, 
                   autoWidth = TRUE, 
                   info = FALSE,
                   dom = 'fBltp',
                   buttons = list(list(extend = 'collection',
                                       buttons = c('csv', 'excel', 'pdf'),
                                       text = '<i class=\"fas fa-download\"></i>')),
                   paginate = TRUE))
```
:::

")
}

db |> 
  distinct(feature_id, feature_title, feature_description, compiled, 
           updated_day, updated_month, updated_year, filename, feature_lexeme) |> 
  count(feature_id, feature_title, feature_description, compiled, 
        updated_day, updated_month, updated_year, filename) |> 
  mutate(number_section = n > 1) ->
  rmd_creation

# If there are problems, check rmd_creation dataset!

library(ymlthis)

walk(rmd_creation$feature_id, function(i){
  yml_empty() |> 
    yml_title(rmd_creation$feature_title[i]) |> 
    yml_author(rmd_creation$compiled[i]) |> 
    yml_date(str_c('Last update: ', 
                   '`r lubridate::make_datetime(year = ',
                   rmd_creation$updated_year[i],
                   ', month = ',
                   rmd_creation$updated_month[i],
                   ', day = ',
                   rmd_creation$updated_day[i],
                   ')`')) |> 
    yml_output(html_document(number_sections = TRUE,
                             anchor_sections = TRUE,
                             pandoc_args = "--shift-heading-level-by=-1")) |> 
    use_rmarkdown(path = rmd_creation$filename[i], 
                  open_doc = FALSE, 
                  quiet = TRUE,
                  include_body = FALSE,
                  body = NULL)
  
  db |> 
    filter(feature_id == i) |> 
    pull(feature_lexeme) |> 
    unique() |> 
    map(make_section) |> 
    write_lines(rmd_creation$filename[i], append = TRUE)
  
  t <- read_lines(rmd_creation$filename[i])
  # change id
  t[str_which(t, "PUT_FEATURE_ID_HERE")] <- 
    str_replace(t[str_which(t, "PUT_FEATURE_ID_HERE")], 
                "PUT_FEATURE_ID_HERE", 
                as.character(rmd_creation$feature_id[i]))
  # change text
  t[str_which(t, "PUT_FEATURE_DESCRIPTION_HERE")] <- 
    str_replace(t[str_which(t, "PUT_FEATURE_DESCRIPTION_HERE")], 
                "PUT_FEATURE_DESCRIPTION_HERE", 
                as.character(rmd_creation$feature_description[i]))
  
  # fix in case there is no feature_lexeme
  t[str_which(t, "\\#\\# NA")] <- 
    str_replace(t[str_which(t, "\\#\\# NA")], "\\#\\# NA", "##")
  
  # fix section enumiration
  if(!rmd_creation$number_section[i]){
    t[str_which(t, "    number_sections: true")] <- 
      str_replace(t[str_which(t, "    number_sections: true")], 
                  "    number_sections: true", 
                  "    number_sections: false")
  }
  
  t <- t[str_which(t, "feature_lexeme == 'NA'", negate = TRUE)]
  
  write_lines(t, rmd_creation$filename[i])
})

# RENDER ------------------------------------------------------------------

library(quarto)
quarto_render(profile = "english")
quarto_render(profile = "russian")
