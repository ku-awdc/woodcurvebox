#'
#' 
#'  
#' The infamous peak in milk yield, should correspond to a minima in SCC curves.
#' 
#' GAM does not capture that the minima could vary in days-in-milk within a
#' lactation period, thus, we try to find this point in the data itself.
#'  
#' Only interested in the raw SCC counts
#' 
one_herd <- one_herd %>% 
  # mutate(SCC = log(SCC)) %>% # cheat to see the analysis with log(SCC) 
  select(DYR_ID, SCC, PARITY, DIM)
#' 
one_herd %>%
  group_by(DYR_ID, PARITY) %>% 
  slice_min(
    order_by = SCC,
    n = 1
  ) %>% 
  ungroup() %>% 
  mutate(global_min_dim = DIM,
         global_min_value = SCC) %>% 
  select(-DIM, -SCC) ->
  one_herd_global_min
#' 
#' 
#' 
one_herd_global_min %>% 
  filter(PARITY <= 4) %>% 
  identity() %>% {
    ggplot(.) +
      # aes(global_min_dim) +
      aes(global_min_dim, color = factor(PARITY)) +
      geom_density(linetype = "dashed") + 
      geom_freqpoly(aes(y=after_stat(density)))
  }


#TODO: Find the first minima and include it 

one_herd %>% 
  arrange(DYR_ID, PARITY, DIM) %>% 
  group_by(DYR_ID, PARITY) %>% 
  mutate(diff_predict = diff(SCC) %>% c(NA)) %>% 
  # summarise(first_peak = which(diff_predict > 0)[[1]])
  # slice(first_peak = which(diff_predict > 0)[[1]]) %>% 
  slice(first_peak = which(diff_predict > 0) %>% {
    if (is_empty(.)) {
      NA
    } else {
      .[[1]]
    }
  }) %>% 
  ungroup() %>%
  mutate(first_min_dim = DIM,
         first_min_value = SCC) %>% 
  select(-DIM, -SCC, -diff_predict) -> 
  one_herd_first_min


one_herd_first_min %>% 
  filter(PARITY <= 4) %>%
  identity() %>% {
    ggplot(.) +
      aes(first_min_dim, color = factor(PARITY)) +
      geom_density(linetype = "dashed") + 
      geom_freqpoly(aes(y=after_stat(density)))
  }


one_herd_global_min %>% 
  nrow()
one_herd_first_min %>% 
  nrow()
#'
#'
#' ## Other statistics about the lactation period
#' 
#' To complete the analysis, here we find the `slope` as well as the `avg` 
#' and provide the statistics overall (wrt. parity) of the lactation period.
#' 
#' 

one_herd %>% 
  group_by(DYR_ID, PARITY) %>% 
  summarise(average = mean(SCC)) %>% 
  ungroup() -> 
  one_herd_global_avg

one_herd %>% 
  arrange(DYR_ID, PARITY, DIM) %>% 
  group_by(DYR_ID, PARITY) %>%
  # slice(which(DIM >= 200) %>% {c(head(.,1), tail(., 1))}) %>% 
  # filter(DYR_ID == 1012661209) %>% 
  slice(which(between(DIM, 150, 250)) %>% {c(head(.,1), tail(., 1))}) %>%
  summarise(slope = diff(SCC) / diff(DIM)) %>% 
  # print()
  ungroup() -> 
  one_herd_end_of_lactation_slope
#'
#'
#' One could explore why these are not producing a meaningful slope...
one_herd_end_of_lactation_slope %>%
  filter(is.nan(slope)) %>% 
  # filter(PARITY <= 4) %>% 
  print(n=Inf)

one_herd %>% 
  semi_join(
    one_herd_end_of_lactation_slope %>%
      filter(is.nan(slope))
  ) %>% 
  print(n=Inf)


one_herd_end_of_lactation_slope %>%
  nrow()

one_herd_curve_stats <- 
  one_herd_global_avg %>% 
  full_join(one_herd_global_min, by = c("DYR_ID", "PARITY")) %>%
  full_join(one_herd_first_min, by = c("DYR_ID", "PARITY")) %>% 
  full_join(one_herd_end_of_lactation_slope, by = c("DYR_ID", "PARITY")) %>% 
  
  # naniar::miss_var_summary() %>%
  identity()
#'
#'
one_herd_curve_stats %>% 
  naniar::miss_var_summary()
#'

one_herd_curve_stats %>% 
  group_by(PARITY) %>% 
  summarise(
    across(average:slope, list("min" = . %>% min(na.rm = TRUE), 
                               "max" = . %>% max(na.rm = TRUE),
                               "lower" = . %>% quantile(probs = 0.025, na.rm = TRUE), 
                               "upper" = . %>% quantile(probs = 0.975, na.rm = TRUE)),
           .names = "{.col}.fn{.fn}")
  ) %>% 
  ungroup() %>%
  pivot_longer(-PARITY) %>% 
  separate(name, c("var", "stat"), sep = ".fn") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  # filter(PARITY == 1) %>% 
  
  print(n=Inf) %>% 
  identity() 

