#' Title
#'
#' @param variables
#'
#' @return
#' @export
#'
#'
#' @examples
sample_scc_curves <- function(parameters_df, herd_size) {
  #' Generate individual cow parameters from the first lactation:
  parameters_df %>%
    mutate(herd_size = herd_size) %>%
    identity() %>% {
      bind_cols(
        rename(., herd_a = a, herd_b = b, herd_c = c),
        tibble(
          a = truncnorm::rtruncnorm(.$herd_size,
                                    # a = 0,
                                    # b = 0,
                                    mean = .$a,
                                    sd = 1),
          b = truncnorm::rtruncnorm(
            .$herd_size,
            # a = 0,
            # a = 0.01,
            b = 0,
            mean = .$b,
            # sd = 0.1
            # sd = 0.04
            sd = 0.2
          ),
          c = truncnorm::rtruncnorm(
            .$herd_size,
            # a = .$c,
            a = -1,
            b = 0,
            mean = .$c,
            sd = 0.00001
            # sd = 0.0001
            # sd = 0.005
          )
        ) %>%
          rename_with(~str_c("cow_", .x)) %>%
          rowid_to_column("cow_id") %>%
          mutate(cow_id = as.character(cow_id) %>%
                   # mutate(cow_id = str_c("cow_", cow_id) %>%
                   fct_inorder())
      )
    } %>%
    return()

  #'
  #'   #'
  #'   herd_df %>%
  #'     pivot_longer(cols = c(cow_a, cow_b, cow_c)) %>%
  #'
  #'     identity() %>% {
  #'       ggplot(.) +
  #'         aes(value, group = interaction(name)) +
  #'         geom_density() +
  #'         geom_rug(
  #'           data = . %>%
  #'             group_by(name, lactation, herd) %>%
  #'             slice(1) %>%
  #'             pivot_longer(cols = c(herd_a, herd_b, herd_c),
  #'                          names_to = "herd_name",
  #'                          values_to = "herd_value"),
  #'           aes(herd_value)) +
  #'         facet_wrap(~name, ncol = 1, scales = "free") +
  #'         NULL
  #'     }
  #'   #'
  #'
  #' herd_full_df <- herd_df %>%
  #'   expand_grid(
  #'     DIM = seq.default(5, 305)
  #'   ) %>%
  #'   mutate(daily_lactation_f  =
  #'            pmap_dbl(select(., n = DIM, a = cow_a, b = cow_b, c = cow_c),
  #'                     daily_lactation_f))
  #' #'
  #' herd_level_curve_df <- herd_df %>%
  #'   slice(1) %>%
  #'   select(-cow_a, -cow_b, -cow_c, -cow_id) %>%
  #'   expand_grid(
  #'     DIM = seq.default(5, 305)
  #'   ) %>%
  #'   mutate(daily_lactation_f  =
  #'            pmap_dbl(select(., n = DIM, a = herd_a, b = herd_b, c = herd_c),
  #'                     daily_lactation_f))
  #' #'
  #' #'
  #' herd_full_df %>%
  #'
  #'   identity() %>% {
  #'     ggplot(.) +
  #'       aes(DIM, daily_lactation_f) +
  #'       geom_line(aes(color = cow_id,
  #'                     group = interaction(cow_id)),
  #'                 alpha = .5,
  #'                 show.legend = FALSE) +
  #'
  #'       geom_line(aes(color = "herd-level"),
  #'                 data = herd_level_curve_df,
  #'                 linetype = "dashed",
  #'                 size = 2,
  #'                 show.legend = FALSE) +
  #'       scale_color_viridis_d() +
  #'       NULL
  #'   }
}
