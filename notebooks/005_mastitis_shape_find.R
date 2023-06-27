
source("notebooks/eda_startup.r")
devtools::load_all()

#'
#'
expand_grid(

  shape = c(1, 2, 3, 5, 9, 7.5, 0., 5),
  scale = c(2, 2, 2, 1, 0.5, 1., 1.)
) %>%
  distinct(shape, scale) %>%
  #' find all the points based on these:
  mutate(mastitis = asplit(cbind(shape, scale), MARGIN = 1) %>%
           map(find_mastititis_shapes)) %>%
  unnest_wider(mastitis, names_sep = "_") ->
  found_parameters_df
#'
#'
#'
found_parameters_df %>%
  pivot_longer(cols = starts_with("mastitis_")) %>%
  identity() %>% {
    ggplot(.) +

      aes(value, group = name) +

      geom_density() +
      facet_wrap(~name, scales = "free")
  }
#'
#'
found_parameters_df %>%
  rowid_to_column("conf_id") %>%

  mutate(lost_density = pgamma(21,
                               shape = mastitis_shape, scale = mastitis_scale,
                               lower.tail = FALSE)) ->
output_df
#'
#'
#'
#'
output_df %>%
  identity() %>% {
    ggplot(.) +
      aes(lost_density) +
      geom_histogram() +
      geom_density() +
      geom_rug()
  }
#'
#' the above should result in something around zero, but unfortunately it doesn't.
#'
#'
#'
output_df %>%

  mutate(x = list(seq.default(0, 21, length.out = 100))) %>%
  unnest(x) %>%
  mutate(y = dgamma(x, shape = mastitis_shape, scale = mastitis_scale)) %>%

  identity() %>% {
    ggplot(.) +
      aes(x,y, group = conf_id, color = conf_id) +
      geom_line() +

      scale_colour_viridis_c()
  }

#'
#'
#' Finally, we decide what the true parameters should be and we store them
#' within this package.
#'
output_df %>%
  summarise(scale = median(mastitis_scale),
            shape = median(mastitis_shape)) ->

  mastitis_shape_parameters
#'
usethis::use_data(mastitis_shape_parameters)


