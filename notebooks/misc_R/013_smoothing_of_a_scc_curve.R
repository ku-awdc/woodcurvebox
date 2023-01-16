
library(mgcv)

one_herd_limited <- 
  one_herd %>% 
  #' not enough data for parity above 4
  filter(PARITY <= 4) %>% 
  mutate(logSCC = log(SCC)) %>% 
  #' too many cows in each parity, choose at most 100 in each
  semi_join(
    distinct(., DYR_ID, PARITY) %>%
      group_by(PARITY) %>%
      sample_n(100, replace = FALSE) %>%
      ungroup(), by = "DYR_ID"
  )

one_herd_limited_pars <- one_herd_limited %>%
  filter(PARITY == 1) %>% 
  group_by(PARITY) %>%
  nest() %>% 
  ungroup() %>% 
  #' ensure that `DYR_ID` does not contain redundant levels
  mutate(data = data %>% 
           map(. %>% mutate(DYR_ID = factor(DYR_ID))))

one_herd_outputs <- one_herd_limited_pars %>% 
  mutate(output = data %>% 
           map(~
                 gam(logSCC ~ 
                       s(DIM, k = 20) +
                       # s(DIM, by = DYR_ID),
                       s(DYR_ID, bs='re'),
                     data = .x, 
                     method = 'REML')))
#' 
one_herd_outputs %>% 
  mutate(
    newdata = data %>% map(function(data) expand_grid(
      DIM = seq_len(305),
      DYR_ID = unique(data$DYR_ID) %>% sample(size = 1)
    )),
    predict = map2(output, newdata, ~ predict(.x, newdata = .y))
  ) -> one_herd_curves
#' 
#' 
one_herd_curves %>% 
  select(-data, -output) %>% 
  unnest(c(newdata, predict)) %>% 
  
  select(DYR_ID, PARITY, DIM, predict) -> 
  one_herd_curves_df
#'
#'
# library(smooth)
# es(one_herd_curves_df %>% select(DIM, predict) %>% as.matrix)
# es(ts(one_herd_curves_df$predict, start = 1, end = 305))
# es(ts(one_herd_curves_df$predict, start = 1, end = 305, frequency = 1))

#'
one_herd_curves_df %>%
  group_by(DIM > 55) %>% 
  group_modify(function(data, key) {
    if (pull(key)) {
      mutate(data, 
             movavg_e = pracma::movavg(predict, n = 60, type = "e"))
    } else {
      mutate(data, 
             movavg_e = predict)
    }
  }) %>%
  
  # naniar::vis_miss()
  # mutate(ema = TTR::EMA(predict, n = 40),
  #        movavg_e = pracma::movavg(predict, n = 40, type = "e")) %>%
  # pivot_longer(c(predict, ema, movavg_e)) %>% 
  pivot_longer(c(predict, movavg_e)) %>% 
# one_herd_curves_df %>% 
  identity() %>% {
    ggplot(.) + 
      aes(DIM, value, group = name, color = name) + 
      # geom_line(position = position_dodge(width = 10)) + 
      geom_line() + 
      ylim(2.5, 6.4) + 
      # facet_wrap(name~., ncol = 1) + 
      NULL
  }
