#' GAM Parameters
#' 
library(tidyverse)
library(lubridate)
library(lme4)
library(broom)
#'
#'
#'
load("K:/paperII/004_kontrol.RData") 
#'
#'
#' Begin by removing the herd that is sampled every week;
top_herds <- top_herds %>%
  #'
  filter(BES_ID != 9986698)
#'
# glimpse(top_herds)
# top_herd_sizes_obs
# top_herds %>% 
#   filter(PARITY == 1) %>% 
#   group_by(BES_ID, DYR_ID) %>% 
#   summarise(dKontrolDato = KONTROLDATO %>% diff()) %>%
#   # summarise(min_dKontrolDato = dKontrolDato %>% min) %>% 
#   arrange(dKontrolDato)

k <- 0.11

nine_herds <- top_herds %>% 
  filter(PARITY == 1) %>% 
  mutate(logSCC = log(SCC)) %>% 
  mutate(exp_k = exp(-k * DIM))


nlme::nlme(
  ~ F
)
# lmer(logSCC ~ 1 + DIM + exp_k + (1|BES_ID), 
#      data = nine_herds) -> 
#   lmer_fixed_k

# nine_herds_sample <- 
#   nine_herds %>% 
#   sample_n(size = 10e3)

wilmink_pars <- list()

optim(par = 0.1, 
      function(k) {
        # if (k<0) {
        #   return(Inf)
        # }
        
        # lmer(logSCC ~ 1 + DIM + exp_k + (1|BES_ID), # alpha is random component
        # lmer(logSCC ~ (1|BES_ID) + (0+DIM|BES_ID) + exp_k, # alpha is random component
        lmer(logSCC ~ (1|BES_ID) + (0+DIM|BES_ID) + (0+exp_k|BES_ID), # alpha is random component
             data = 
               nine_herds %>% 
               # sample_n(10e3) %>%
               mutate(exp_k = exp(-k * DIM))) -> 
          lmer_fixed_k
        wilmink_pars <<- coef(lmer_fixed_k)
        -logLik(lmer_fixed_k)
      }, 
      method = "Brent",
      lower = -2,
      upper = 2,
      control = list(trace = 2, REPORT = 1)) ->
  wilmink_optim_output
wilmink_pars
wilmink_optim_output
#'
#'
#'
wilmink_pars %>% 
  unclass() %>% 
  as.data.frame() %>% 
  as_tibble(rownames = "BES_ID") %>% 
  rename(beta_DIM = BES_ID.DIM, beta_exp_k = BES_ID.exp_k, intercept = BES_ID..Intercept.) %>% 
  mutate(k = wilmink_optim_output$par) -> 
  wilmink_parameters_collated
#'
#'
#'
wilmink_parameters_collated %>% 
  expand_grid(DIM = seq_len(305)) %>% 
  mutate(
    .fitted = intercept + beta_DIM * DIM + beta_exp_k * exp(-k * DIM)
  ) %>% 
  identity() %>% {
    ggplot(.) + 
      aes(DIM, .fitted, group = BES_ID) + 
      geom_line() +
      
      NULL
  }
#'
#'
wilmink_parameters_collated %>%
  select(BES_ID, intercept, beta_DIM, beta_exp_k, k) %>% 
  pivot_longer(-BES_ID) %>% 
  mutate(name = fct_inorder(name)) %>% 
  identity() %>% {
    ggplot(.) + 
      aes(value, group = name) + 
      geom_density(aes(y = after_stat(density), color = "density")) +
      geom_freqpoly(aes(y = after_stat(density), color = "freqpoly")) +
      geom_rug() + 
      facet_wrap(~name, scales = "free") + 
      NULL
  }

