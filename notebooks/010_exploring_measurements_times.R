#'
#'
#' This used data loaded from `nlme_4`.
#'
good_herds <-
  df2 %>%
  ungroup() %>%
  count(BES_ID) %>%
  slice_max(n, n = 10)
#'
#'
df2 %>%
  ungroup() %>%
  semi_join(good_herds %>% select(-n), by = "BES_ID") ->
  good_herds_df_model
#'
#'
good_herds_df_model %>%
  group_by(BES_ID, DYR_ID, PARITY) %>%
  # filter(PARITY == 2) %>%
  summarise(all_dims = list(DIM),
            all_dims_length = all_dims %>% lengths) %>%
  arrange(BES_ID, desc(all_dims_length)) %>%
  filter(all_dims_length > 6) %>%

  mutate(mean_measure_distance = all_dims %>%
           map_dbl(. %>% diff.default %>% mean)) %>%
  identity() -> df_par2
#'
#'
df_par2 %>%
  mutate(
    dim1 = all_dims %>% map_dbl(~.x[1]),
    dim2 = all_dims %>% map_dbl(~.x[2]),
         # dim_last = all_dims %>% map_dbl(~tail(.x, 1))) %>%
         dim_10 = all_dims %>% map_dbl(~.x[10] %||% NA),
         dim_9 = all_dims %>% map_dbl(~.x[9] %||% NA)
    ) %>%

  # pivot_longer(c(dim1, dim_last)) %>%
  pivot_longer(c(dim1, dim2, dim_9, dim_10)) %>%

  identity() %>% {
    ggplot(.) +
      aes(value, group = name, color = name) +
      geom_freqpoly(aes(y = after_stat(density))) +
      geom_density(aes(y = after_stat(density))) +

      # facet_wrap(~name) +
      NULL
  }


