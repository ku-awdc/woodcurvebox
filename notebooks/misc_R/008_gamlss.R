#' assessing gamlss
#'
#'
library(gamlss)
library(tidyverse)
herd_df %>%
  dplyr::select(where(. %>% is.na %>% any %>% `!`)) %>% 
  mutate(SCC = exp(logSCC)) ->
  herd_df

gamlss::gamlss(
  logSCC ~ pb(DIM) +  PARITY,
  sigma.formula = ~PARITY,
  nu.formula = ~PARITY,
  family = GG(mu.link = "identity", 
              # sigma.link = "log", 
              sigma.link = "log", 
              nu.link = "identity"),
  data = herd_df %>%
    dplyr::select(where(. %>% is.na %>% any %>% `!`))
) -> gamlss_model
warnings()

gamlss_model %>% 
  plot()


gamlss_model %>% 
  summary()

gamlss_model %>% 
  broom::glance()

gamlss_model %>% 
  broom::augment()


gamlss_model %>% 
  broom::augment() %>% {
    ggplot(.) + 
      geom_point(aes(`pb(DIM)`, logSCC), shape = 20, alpha = 0.1) + 
      # geom_line(aes(DIM, exp(.fitted))) +
      geom_line(aes(`pb(DIM)`, .fitted)) +
      facet_wrap(~PARITY, scales = "free") +
      NULL
  }

