
source("../asf_phd/R/eda_startup.r")
#'
peak_loc <- 50
peak_value <- 60

first <- TRUE
plot.new()
for (conf in
     expand_grid(a = 0, b = .01,
                 #' `c` maximal attainable `log(SCC)`-count
                 c = log(10e3),
                 #' `d` must be non-negative
                 #' doesn't influence maximum
                 d = c(1, log(10)),
                 k = .000000000001) %>%
     transpose()) {
  with(conf, {
    print(c * exp(-(peak_loc / 1000) * 100 * d))
    curve(a + b * x + c * exp(-(peak_loc / 1000) * x * d),
          xlim = c(0, 305),
          add = !first)
    abline(a, b, col = 20)

  })
  first <- FALSE
}
#'
#'
#'

expand_grid(a = seq.default(1.8, 2.4, length.out = 3),
            b = tan(pracma::deg2rad(seq.default(0, .6, length.out = 10))),
            # b = log(50),
            #' `c` maximal attainable `log(SCC)`-count
            # c = log(10e3),
            c = exp(-6:2),
            d = seq.default(-0.5, 1.5, length.out = 10),
            #' `d` must be non-negative
            #' doesn't influence maximum
            # d = c(1, log(10)),
            # d = seq.default(1, log(10), length.out = 4),
            k = .000000000001) %>%
  rowid_to_column("conf") %>%
  mutate(conf = factor(conf)) %>%
  expand_grid(
    DIM = seq.default(0, 305)) %>%
  mutate(f_value = pmap_dbl(select(., a, b, c, d, k, DIM), function(...) {
    with(list(...), {
      # a + b * DIM + c * exp(-(peak_loc / 1000) * DIM * d)
      # a + b * DIM + d * exp(-exp(k) * DIM)
      a + b * DIM + d * exp(-0.05 * DIM * c)
    })
  })) %>%
  group_by(conf) %>%
  mutate(df_value = diff.default(f_value) %>% c(NA)) %>%
  pivot_longer(c(f_value, df_value)) %>%
  #' DEBUG
  # pivot_longer(c(df_value)) %>%
  identity() %>% {
    ggplot(.) +
      aes(DIM, value, color = conf, group = interaction(conf, name)) +
      geom_line(show.legend = FALSE, alpha = .25) +

      # geom_abline(aes(intercept = a, slope = b),
      #             data = . %>% distinct(conf, a, b),
      #             show.legend = FALSE) +

      geom_vline(aes(xintercept = 50, color = "peak"),
                 show.legend = FALSE) +
      # coord_equal() +
      # lims(x = c(0, 70)) +
      lims(x = c(0, 305)) +

      facet_wrap(~name, ncol = 1, scales = "free_y") +
      # scale_y_log10() +
      NULL
  }

