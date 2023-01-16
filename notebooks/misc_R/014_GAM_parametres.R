#'
#' GAM Parameters
#' 
library(tidyverse)
library(lubridate)
library(broom)
#'
#'
theme_set(ggpubr::theme_pubclean())
#'
#'
load("K:/paperII/004_kontrol.RData") 

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



# param <- predict(model_out, type = "terms")

one_herd_outputs %>% 
  mutate(
    augment = map2(output, data,
                   ~ broom:::augment.gam(x = .x, 
                                         newdata = expand_grid(
                                           DYR_ID = unique(.y$DYR_ID) %>% sample(size = 1),
                                           # DIM = seq_len(305)))
                                           DIM = seq.default(-50, 12 * 305, by = 1)))
    )) %>%
  select(-data, -output) -> 
  choosen_baseline_scc_curve

stretching_parameters_n <- 25

choosen_baseline_scc_curve %>% 
  expand_grid(
    # alpha = c(0, -12, 3),
    alpha = seq.default(-10, 3, length.out = stretching_parameters_n),
    # beta  = c(1, 1.4),
    beta  = seq.default(0.001, 1.4, length.out = stretching_parameters_n),
    # gamma = c(1, 0.1, 1.55),
    # gamma = c(1, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
    gamma = c(1, seq.default(0.1, 1, length.out = stretching_parameters_n)),
  ) %>% 
  rowid_to_column("new_cow_id") %>% 
  mutate(
    augment_new_cow = pmap(select(., augment, alpha, beta, gamma), 
                           function(augment, alpha, beta, gamma) {
                             mutate(augment, 
                                    # .fitted = pracma::movavg(.fitted, 50, type = "e"),
                                    .fitted = alpha + beta * .fitted,
                                    # .fitted = pmin(alpha + beta * .fitted, 7),
                                    DIM = gamma * DIM) %>% 
                               filter(between(DIM, 0, 305))
                             #'TODO: check if we have DIM being >305 before filtering,
                           })) %>% 
  #' remove all configurations that yields negative SCC curves
  filter(
    map_lgl(
      augment_new_cow, ~ 
        # filter(.x, between(DIM, 0, 305)) %>% 
        # summarise(all(.fitted>=0)) %>%
        summarise(.x, all(between(.fitted, 0, 7))) %>%
        pull()
    )
  ) %>% 
  # unnest(augment) %>%
  select(-augment) -> 
  artificial_scc_curves

artificial_scc_curves %>% 
  unnest(augment_new_cow) %>%
  # glimpse()
  # count(new_cow_id)
  mutate(x_flag = between(DIM, 0, 305),
         cow_dilation = case_when(
           new_cow_id == 1 ~ "neutral",
           TRUE ~ new_cow_id %>% as.character(),
         )) %>% {
           ggplot(.) + 
             aes(DIM, .fitted) + 
             # geom_line(aes(group = DYR_ID, color = x_flag)) + 
             # geom_line(aes(group = interaction(new_cow_id), color = x_flag)) + 
             # geom_line(aes(group = interaction(new_cow_id), 
             #               color = cow_dilation),
             #           show.legend = FALSE,
             #           size = 1.2) +
             
             #' this is a second plot...
             geom_point(aes(group = interaction(new_cow_id), 
                           color = cow_dilation),
                        data = . %>% group_by(new_cow_id) %>% sample_n(10),
                       show.legend = FALSE,
                       shape = 20) + 
             
             geom_hline(yintercept = c(3, 6.5), linetype = "dotted") +
             
             scale_color_viridis_d(direction = -1, alpha = 0.8) + 
             
             xlim(-10,  350) +
             
             # ylim(3, 6.5) + 
             NULL
         } %>% 
  # print() %>% 
  # plotly::ggplotly() %>% 
  identity()


artificial_scc_curves %>% 
  unnest(augment_new_cow) %>%
  # glimpse()
  # count(new_cow_id)
  mutate(x_flag = between(DIM, 0, 305),
         cow_dilation = case_when(
           new_cow_id == 1 ~ "neutral",
           TRUE ~ new_cow_id %>% as.character(),
         )) %>% {
           ggplot(.) + 
             aes(DIM, .fitted) + 
             # geom_line(aes(group = DYR_ID, color = x_flag)) + 
             # geom_line(aes(group = interaction(new_cow_id), color = x_flag)) + 
             geom_line(aes(group = interaction(new_cow_id),
                           color = cow_dilation),
                       show.legend = FALSE,
                       size = 1.2) +

             geom_hline(yintercept = c(3, 6.5), linetype = "dotted") +
             
             scale_color_viridis_d(direction = -1, alpha = 0.8) + 
             
             xlim(-10,  350) +
             
             # ylim(3, 6.5) + 
             NULL
         } %>% 
  # print() %>% 
  # plotly::ggplotly() %>% 
  identity()

#' The resulting parameters are definitely not going to occupy 
#' the same space, so let us look at them more closely:
#' 
#' 

expand_grid(
  # alpha = c(0, -12, 3),
  alpha = seq.default(-10, 3, length.out = stretching_parameters_n),
  # beta  = c(1, 1.4),
  beta  = seq.default(0.001, 1.4, length.out = stretching_parameters_n),
  # gamma = c(1, 0.1, 1.55),
  # gamma = c(1, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6),
  gamma = c(1, seq.default(0.1, 1, length.out = stretching_parameters_n)),
) %>% 
  mutate(`alpha x beta` = alpha * beta,
         `alpha x gamma` = alpha * gamma,
         `beta x gamma` = beta * gamma) %>%
  pivot_longer(alpha:`beta x gamma`) %>% 
  mutate(name = fct_inorder(name)) %>% 
  identity() %>% {
    ggplot(.) + 
      aes(value, group = name) + 
      geom_density(aes(color = name)) + 
      geom_freqpoly(aes(y = after_stat(density), 
                        color = name)) + 
      
      facet_wrap(~name, scales = "free_x") + 
      
      NULL
  } %>% 
  identity()
#'
#'
#'
#' 

artificial_scc_curves %>% 
  select(-augment_new_cow) %>% 
  mutate(`alpha x beta` = alpha * beta,
         `alpha x gamma` = alpha * gamma,
         `beta x gamma` = beta * gamma) %>%
  pivot_longer(alpha:`beta x gamma`) %>% 
  mutate(name = fct_inorder(name)) %>% 
  identity() %>% {
    ggplot(.) + 
      aes(value, group = name) + 
      geom_density(aes(color = name)) + 
      geom_freqpoly(aes(y = after_stat(density), 
                        color = name)) + 
      
      facet_wrap(~name, scales = "free_x") + 
      
      NULL
  } %>% 
  identity()
#'
#'
#'
#'
