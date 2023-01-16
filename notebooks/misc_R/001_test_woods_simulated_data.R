
#'
#'Test curves, nls.multistart with simulated DIM and SCC data points
#'


f_wilmink <- function(DIM, a, b, k, d){
  a + b * DIM + exp(-exp(k) * DIM)*d
  # pmax(0.001, a + b * DIM + exp(-exp(k) * DIM)*d)
  # pmax(runif(1, min = 1e-3, 0.1), a + b * DIM + exp(-exp(k) * DIM)*d)
}
#'
#'
#'
shift_point <- 25
shift_point_scc <- 33.115
tribble(
  ~DIM,  ~SCC,
  # 1,      0, # not needed!
  2,      1,   # e.g. log(SCC) = 0, log1p(SCC) = log(1) = 0.6931472
  5,     500,
  7,     500,
  9,     500,
  16,     500,
  shift_point, shift_point_scc,
) %>% 
  bind_rows(
    tibble(DIM = seq.default(shift_point, 150, by = 1),
    SCC = seq_along(DIM) * 5 + shift_point_scc)
  ) %>%
  mutate(logSCC = log(SCC), 
         log1pSCC = log1p(SCC)) %>% 
  identity() %>% {
    ggplot(.) +
      aes(DIM, SCC) +
      geom_point() + geom_line();
    .
  } %>% 
  print() %>% 
  identity() %>% {
    list(
      # null_p = nls(logSCC   ~  f_wilmink(DIM, a, b, k, d),
      #              data = filter(., SCC != 0),
      #              start = c(a = 1, b = 1, k = 100, d = 1),
      #              upper = c(a = 20, b = 20, k = 20, d = 20)
      # ),
      # lower=c(a=0, b=0, k=-5, d=0),
      # upper=c(a=9, b=1.5, k=0, d=5),
      # start_lower = c(a=0, b=0, k=-5, d=0),
      # start_lower = c(a=2, b=2, k=10, d = 2),
      # start_upper = c(a=8, b=1, k=-0.01, d=4)),
      # one_p  = nls(log1pSCC ~  f_wilmink(DIM, a, b, k, d),
      #              lower=c(a=0, b=0, k=-5, d=0),
      #              # start = c(a = 1, b = 1, k = 100, d = 1),
      #              start = c(a=0, b=0, k=-5, d=0),
      #              # upper = c(a = 20, b = 20, k = 20, d = 20),
      #              upper = c(a=8, b=1, k=-0.01, d=4),
      #              data = .))
      
      nls_multstart_log1pSCC = 
        nls.multstart::nls_multstart(log1pSCC ~ f_wilmink(DIM, a, b, k, d),
                                     data = .,
                                     lower=c(a=0, b=0, k=-5, d=0),
                                     upper=c(a=9, b=1.5, k=0, d=5),
                                     start_lower = c(a=0, b=0, k=-5, d=0),
                                     start_upper = c(a=8, b=1, k=-0.01, d=4),
                                     iter = 500,
                                     supp_errors = "Y"),
      nls_multstart_logSCC = 
        nls.multstart::nls_multstart(logSCC ~ f_wilmink(DIM, a, b, k, d),
                                     data = filter(., SCC > 1),
                                     lower=c(a=0, b=0, k=-5, d=0),
                                     upper=c(a=9, b=1.5, k=0, d=5),
                                     start_lower = c(a=0, b=0, k=-5, d=0),
                                     start_upper = c(a=8, b=1, k=-0.01, d=4),
                                     iter = 500,
                                     supp_errors = "Y")
    )
  } %>% 
  enframe() %>% 
  mutate(coef = value %>% map(broom::tidy)) %>% 
  unnest(coef)

