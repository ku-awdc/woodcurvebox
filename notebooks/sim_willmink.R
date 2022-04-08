#' ---
#'
#' output: html_document
#' ---
#'
source("notebooks/eda_startup.r")
devtools::load_all(path = ".")
#TODO: This is outdated as `f_wilmink` has been changed, etc.
#'
#'
#' Original Lactation curves per herd
#'
#' $y_t = a + b \times t + c \times \exp(-0.05 \times t)$
#'
#'
#' Stratifying the response based on a farm-level happens
#' through the $\alpha_{ij}^{M}$
#'
#'
sample_parameters(1000) %>%
  transpose() %>%
  enframe("conf","params") %>%
  mutate(conf = factor(conf)) %>%
  unnest_wider(params) %>%
  identity() %>%
  #' add `DIM` values
  expand_grid(DIM = seq_len(305)) %>%
  identity() %>%
  #' apply wilmink
  mutate(f_value = pmap_dbl(select(., a, b, c, d, DIM), f_wilmink)) %>%
  identity() %>%

  identity() %>% {
    ggplot(.) +
      aes(DIM, f_value,
          color = interaction(conf),
          groups = interaction(conf)) +
      # geom_point() + geom_line()
      geom_line(show.legend = FALSE) +
      # geom_point(shape = 20, show.legend = FALSE) +

      # scale_x_log10() +
      # lims(x = c(0,50)) +

      NULL
  }
#'
#' ### References
#'
#' - [Milking curves article](https://www.frontiersin.org/articles/10.3389/fvets.2016.00115/full)
