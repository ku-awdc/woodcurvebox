#'
#'
#'
source("../asf_phd/r/eda_startup.r")
#'
library(tidylog)
library(haven)
#'
#'
#' Data source: [Link](https://data.mendeley.com/datasets/b9rs4d5dsn/2)
#'
#'
haven::read_sav(r"(C:\Users\tpb398\Downloads\b9rs4d5dsn-1\data set for 10 regular herds.sav)", n_max = Inf) ->
  regular_herds_df
haven::read_sav(r"(C:\Users\tpb398\Downloads\b9rs4d5dsn-1\data set for 2 mastitis controlled herds.sav)", n_max = Inf) ->
  mastitis_controlled_herds_df
#'
mastitis_controlled_herds_df %>%
  arrange(herd, cow, parity, par_year, par_mont, rec_num) %>%
  View()
  naniar::vis_miss()
#'
mastitis_controlled_herds_df %>%
  glimpse() %>%
  identity() %>% {
    ggplot(.) +
      aes(DIM, )
  }
#'
#' These dataset have dates corresponding to Iranian calendar, and should be
#' converted if they are to be compared to other datasets.
#'
#'
regular_herds_df %>%
  glimpse() %>% print() %>%
  count(herd_cod, body_cod, name = "n_observations_pr_body") %>%
  identity()
#'
regular_herds_df %>%
  mutate(record_date = lubridate:::update.Date(
    #' ensure this has no time, as it is invalid..
    lubridate::today(),
    years = year, months = month, days = day)) %>%
  mutate(parity_m = as.Date(parity_m)) %>%
  #' FIXME: `parity_m` is `NA` many more places than `record_date`.
  #' Which is not ideal.
  filter(!is.na(record_date)) %>%
  identity() ->
  regular_herds_clean_df
#'
regular_herds_clean_df %>%
  add_count(herd_cod, body_cod, name = "n_body_count") %>%
  add_count(herd_cod, parity, name = "n_body_parity") %>%
  distinct(herd_cod, body_cod, parity, .keep_all = TRUE) %>%
  select(herd_cod, body_cod, parity, n_body_count, n_body_parity) %>%
  arrange(desc(n_body_parity), desc(n_body_count)) %>%
  View()
#'
#' A lot can be concluded from this but apparently herd `293` is
#' rich with measurements;
#'
#' `lactatio` is probably useful to "create" days-in-milk.
#' But first, the relation between lactation and parity needs to be investigated.
#'
regular_herds_clean_df %>%
  filter(herd_cod == 293) %>%
  select(herd_cod, body_cod, parity, lactatio, record_date) %>%
  arrange(body_cod, record_date)
  pivot_wider()
#'
#' Conclusion is that `lactatio` is probably `days-in-milk`.
#'
#'
regular_herds_clean_df %>%

  # glimpse()
  filter(herd_cod == 293) %>%

  # pivot_longer(c(milk_yie, logscc)) %>%
  pivot_longer(c(logscc)) %>%

  identity() %>% {
    ggplot(.) +
      # aes(record_date, value,
      aes(lactatio, value,
          color = interaction(name,
                              herd_cod, body_cod,
                              parity,
                              lactatio),
          group = interaction(name,
                              herd_cod, body_cod,
                              parity,
                              lactatio)) +
      geom_line(show.legend = FALSE) +
      geom_point(show.legend = FALSE) +

      facet_wrap(name + body_cod ~parity,
                 labeller = label_both,
                 scales = "free") +
      NULL
  }
#'
#'
#'
