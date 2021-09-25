#' ---
#' subtitle: Exploring incomplete gamma function
#' output: html_document
#' ---
#'
source("notebooks/eda_startup.r")
devtools::load_all(path = ".")
#'
#'
#' Incomplete gamma-function
#'
#' @param n day `n`
daily_lactation_f <- function(n, a, b, c) {
  a * n ** b * exp(-c * n)
}
#' @inheritParams daily_lactation_f
daily_lactation_df <- function(n, a, b, c) {
  #' alternative: `a exp(-c n) (b n^(-1 + b) - c n^b)`
  a * exp(-c * n) * n^(-1 + b) * (b - c * n)
}
#'
#'
#' Source: [Application of the Incomplete Gamma Function to
#' Predict Cumulative Milk Production](https://www.sciencedirect.com/science/article/pii/S0022030280828955)
#'
tibble(lactation = "first",
       a = 13.546, b = 19.575 / 1e2, c = 3.388 / 1e3) %>%
  add_row(lactation = "second",
          a = 18.820, b = 23.576 / 1e2, c = 5.441 / 1e3) %>%
  add_row(lactation = "third and more",
          a = 15.956, b = 30.004 / 1e2, c = 6.067 / 1e3) %>%
  mutate(lactation = fct_inorder(lactation)) %>%

  #' Change `a` to opposite sign just to get the curve that we are exploring
  mutate(a = -a) %>%

  #' Add suggested peaks
  mutate(peak_location = b/c) %>%
  # mutate(peak_level = ???) %>%

  #' Evaluate lactation-curve
  expand_grid(n = seq_len(305)) %>%
  mutate(daily_lactation_f  =
           pmap_dbl(select(., n, a, b, c), daily_lactation_f)) %>%
  mutate(daily_lactation_df  =
           pmap_dbl(select(., n, a, b, c), daily_lactation_df)) %>%
  pivot_longer(starts_with("daily_lactation")) %>%
  identity() %>% {
    ggplot(.) +
      # aes(n, daily_lactation_f,
      aes(n, value,
          groups = interaction(name, lactation),
          color = lactation) +
      geom_line() +

      geom_vline(aes(xintercept = peak_location,
                     color = lactation),
                 data = . %>% distinct(peak_location, lactation)) +
      facet_wrap(~name, ncol = 1, scales = "free_y") +
      NULL
  }
#'
#'
#'
