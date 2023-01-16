#'
#'

load("M:/PCR_data/curve6_nlme_wilmink.RData")

#'
#'
library(tidyverse)
theme_set(ggpubr::theme_pubclean())

#TODO: Order PARITY according to the numeric value

df_model <- df_model %>% 
  ungroup() %>% 
  mutate(PARITY = fct_inseq(PARITY))
#'
#'
#'
#'
df_model %>% 
  ungroup() %>% 
  # count(BES_ID, DYR_ID, sort = FALSE) %>% 
  count(BES_ID, sort = FALSE) %>%
  slice_max(n, n = 10) %>%
  identity()
#'
#'
#' Chosen `4522612` as our favorite `BES_ID`
#' 
herd_df <- df_model %>% 
  ungroup() %>% 
  filter(BES_ID == 4522612)
#'
#'
#' Figure out if they were moved to another `BES_ID`, and that's why they are
#' missing.
df_model %>% 
  semi_join(
    herd_df %>% 
      distinct(DYR_ID)
  ) -> data_on_selected_cows

data_on_selected_cows %>% 
  count(DYR_ID, PARITY) %>% 
  pivot_wider(names_from = PARITY, values_from = n)  %>% 
  naniar::vis_miss(cluster = TRUE)
#' 
#' Unfortunately, this only explains 0.1% of the missing data...
#' This should be included somewhere in the analysis.
#'
#'
data_on_selected_cows %>% 
  identity() %>% {
    ggplot(.) + 
      aes(DIM, logSCC) +
      geom_point(shape = 20) + 
      geom_line(color = "grey20", alpha = 0.1) + 
      
      geom_smooth(orientation = "x") + 
      
      facet_wrap(~PARITY)
  }
#' For now, we ignore `data_on_selected_cows` and just stick with the information
#' that's available on the cows, from the selected herd. 
#' 

herd_df %>% 
  mutate(SCC = exp(logSCC)) %>% 
  filter(SCC <= 5e2) %>% 
  pivot_longer(c(SCC, logSCC)) %>% 
  # print()
  identity() %>% {
    ggplot(.) + 
      aes(DIM, value, group = name) +
      geom_point(shape = 20) + 
      geom_line(color = "grey20", alpha = 0.1) + 
      
      geom_smooth(orientation = "x") + 
      
      facet_wrap(name~PARITY, scales = "free")
  }
#' 
#' 
herd_df %>% 
  mutate(SCC = exp(logSCC)) %>% 
  group_by(PARITY) %>%
  nest() %>% 
  arrange(PARITY) %>% 
  ungroup() %>% 
  mutate(
    model_output = data %>% 
      map(., 
          function(data) {
            # browser()
            
            loglogSCC_nls_model_coef <- 
              nls(log(logSCC) ~ a * (DIM**b) * exp(-c * DIM), 
                  start = c(
                    # a = 3,
                    a = 4,
                    b = -0.0001,
                    c = -0.003
                  ), data = data) %>% 
              coef()
            logSCC_nls_model_coef <- 
              nls(logSCC ~ a * (DIM**b) * exp(-c * DIM), 
                  start = c(
                    # a = 3,
                    a = 4,
                    b = -0.0001,
                    c = -0.003
                  ), data = data) %>% 
              coef()
            SCC_nls_model_coef <- 
              nls(SCC ~ a * (DIM**b) * exp(-c * DIM), 
                  start = c(
                    # a = 3,
                    a = 4,
                    b = -0.0001,
                    c = -0.003
                  ), data = data) %>% 
              coef()
            
            list(
              model_with_loglogSCC = 
                nlme::nlme(
                  log(logSCC) ~ a * (DIM**b) * exp(-c * DIM),
                  data = data,
                  fixed = a + b + c ~ 1,
                  random = a + b + c ~ 1,
                  # groups = ~cow_id,
                  groups = ~BES_ID,
                  start = loglogSCC_nls_model_coef,
                  # na.action = na.exclude,
                  control = list(returnObject = TRUE, opt = "nlm", 
                                 msMaxIter = 250)
                  # control = list(returnObject = TRUE)
                ),
              model_with_logSCC = 
                nlme::nlme(
                  logSCC ~ a * (DIM**b) * exp(-c * DIM),
                  data = data,
                  fixed = a + b + c ~ 1,
                  random = a + b + c ~ 1,
                  # groups = ~cow_id,
                  groups = ~BES_ID,
                  start = logSCC_nls_model_coef,
                  # na.action = na.exclude,
                  control = list(returnObject = TRUE, opt = "nlm", 
                                 msMaxIter = 250)
                ),
              model_with_SCC = 
                nlme::nlme(
                  SCC ~ a * (DIM**b) * exp(-c * DIM),
                  data = data,
                  fixed = a + b + c ~ 1,
                  random = a + b + c ~ 1,
                  # groups = ~cow_id,
                  groups = ~BES_ID,
                  start = SCC_nls_model_coef,
                  # start = c(
                  #   a =  150,
                  #   b = -0.1,
                  #   c = -0.003
                  # ),
                  # na.action = na.exclude,
                  control = list(returnObject = TRUE)
                )
            )
          }
      )
  ) -> herd_model_output_df
#'
#'
#'
herd_model_output_df %>% 
  select(-data) %>% 
  unnest_wider(model_output) %>% 
  pivot_longer(-PARITY) %>% 
  mutate(
    residuals = value %>% map(residuals),
    coef = value %>% map(coef),
    deviance = value %>% map_dbl(deviance),
    AIC = value %>% map(. %>% extractAIC %>% set_names(c("edf", "AIC"))),
    sigma = value %>% map_dbl(sigma)
  ) %>% 
  unnest_wider(coef) %>% 
  unnest_wider(AIC) %>%
  arrange(name, PARITY) %>% 
  print()
  select(-value) %>% 
  unnest_longer(residuals) %>% 
  #' ensure that only one herd is present
  # distinct(residuals_id)
  identity() %>% {
    ggplot(.) + 
      aes(residuals) +
      geom_density() + 
      
      facet_wrap(name ~ PARITY, scales = "free") + 
      
      NULL
  }
#'
#'
