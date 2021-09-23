## code to prepare `mastitis_scc_inflation_parameters` dataset goes here

devtools::load_all()

mastitis_scc_inflation_parameters <-
  find_mastititis_shapes(c(1,1))

#' Note that you *should* check the appearance of this using
#'
mastitis_scc_inflation_parameters %>%
  autoplot()

usethis::use_data(mastitis_scc_inflation_parameters, overwrite = TRUE)
