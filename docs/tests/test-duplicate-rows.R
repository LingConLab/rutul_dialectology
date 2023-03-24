library(tidyverse)
library(testthat)

test_that("Test that there are correct village names", {
  
  read_csv("../data/database.csv")  %>% 
    filter(duplicated(.[,-1])) %>% 
    pull(id) ->
    duplicated_ids
  
  if(length(duplicated_ids) != 0){
    observed <- str_c("There are duplicated rows, see ", duplicated_ids)
  } else {
    observed <- ""
  }
  expect_equal(observed, "")
})
