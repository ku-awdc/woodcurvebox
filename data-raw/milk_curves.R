## code to prepare `milk_curves` dataset goes here

library(tidyverse)

tibble(lactation = "first",
       a = 13.546, b = 19.575 / 1e2, c = 3.388 / 1e3) %>%
  add_row(lactation = "second",
          a = 18.820, b = 23.576 / 1e2, c = 5.441 / 1e3) %>%
  add_row(lactation = "third and more",
          a = 15.956, b = 30.004 / 1e2, c = 6.067 / 1e3) %>%
  mutate(lactation = fct_inorder(lactation)) ->
  milk_curves

usethis::use_data(milk_curves, overwrite = TRUE)
