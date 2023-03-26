# install packages ---------------------------------------------------------
packages <- c("tidyverse", "testthat", "lingtypology", "DT", "knitr", 
              "ymlthis", "rmarkdown", "lubridate", "stringr")

to_install <- packages[!(packages %in% installed.packages())]

if(length(to_install) > 0){
  install.packages(to_install, dependencies = TRUE)
}

# RUN TESTS ----------------------------------------------------------------
# testthat::test_dir("tests")

# GENERATE RMD ------------------------------------------------------------
library(tidyverse)

db <- read_csv("data/database.csv") %>% filter(!is.na(value))

# create variable with leading 0 -------------------------------------------
# remove +1 when we will have more then 100 topics
db$filename <- str_c(sprintf(str_c("%0", nchar(max(db$feature_id))+1, "d_"), 
                           db$feature_id),
                     str_replace_all(db$feature_title, "[\\s:\\./]", "_"),
                     ".Rmd")

file.remove(unique(db$filename))

options(ymlthis.rmd_body = "
```{r, include=FALSE}
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning=FALSE, fig.width = 9.5)
library(tidyverse)
library(lingtypology)
db <- read_csv('data/database.csv')  %>% filter(feature_id == PUT_FEATURE_ID_HERE)
villages <- read_csv('data/villages.csv')
db %>% 
  distinct(settlement, value) %>% 
  mutate(n = 1) %>% 
  pivot_wider(names_from = value, values_from = n, values_fill = 0) %>% 
  left_join(villages[,c('village', 'lat', 'lon')], c('settlement' = 'village')) %>% 
  mutate(language = 'Rutul') ->
  for_map
```

PUT_FEATURE_DESCRIPTION_HERE

## Map

```{r}
map.feature(languages = for_map$language,
            latitude = for_map$lat,
            longitude = for_map$lon,
            label = for_map$settlement,
            label.position = 'top',
            label.hide = FALSE,
            minichart.data = for_map %>% select(-settlement, -lat, -lon, -language),
            minichart = 'pie', 
            width = 3)
```

## Data

```{r}
db %>% 
  select(settlement, value, stimuli, answer, collected) %>% 
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
")

db %>% 
  distinct(feature_id, feature_title, feature_description, compiled, 
           updated_day, updated_month, updated_year, filename) ->
  rmd_creation

map(rmd_creation$feature_id, function(i){
  ymlthis::yml_empty() %>% 
    ymlthis::yml_title(rmd_creation$feature_title[i]) %>% 
    ymlthis::yml_author(rmd_creation$compiled[i]) %>% 
    ymlthis::yml_date(str_c('Last update: ', 
                            '`r lubridate::make_datetime(year = ',
                            rmd_creation$updated_year[i],
                            ', month = ',
                            rmd_creation$updated_month[i],
                            ', day = ',
                            rmd_creation$updated_day[i],
                            ')`')) %>% 
    ymlthis::yml_output(html_document(number_sections = TRUE,
                                      anchor_sections = TRUE,
                                      pandoc_args = "--shift-heading-level-by=-1")) %>% 
    ymlthis::use_rmarkdown(path = rmd_creation$filename[i], 
                           open_doc = FALSE, 
                           quiet = TRUE,
                           include_body = FALSE,
                           body = NULL) 
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

  write_lines(t, rmd_creation$filename[i])
})

# RENDER ------------------------------------------------------------------
rmarkdown::render_site()
