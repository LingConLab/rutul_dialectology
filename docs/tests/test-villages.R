library(tidyverse)
library(testthat)

test_that("Test that there are correct village names", {
  
  read_csv("../data/database.csv") %>%  
    pull(settlement) %>% 
    unique() ->
    settlements
  
  read_csv("../data/villages.csv") %>% 
    pull(village) ->
    village
  
  bad_villages <- settlements[!(settlements %in% village)]
  
  if(length(bad_villages) != 0){
    observed <- str_c("There are wrong village names: ", bad_villages)
  } else {
    observed <- ""
  }
  expect_equal(observed, "")
})
