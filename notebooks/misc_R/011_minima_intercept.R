
library(tidyverse)
library(lubridate)
library(magrittr)
#'
#'
#'
#'
load("L:/paper2_merge_temp6.RData")
rm(branch, dryoff, dryoff_treat, other_treat, pcr, teat_treat); gc()
# load("L:/004_kontrol.RData") 


all_herds <- df6 %>%
  ungroup() %>%
  filter(between(SCC, 1, 9998))


all_herds %>% 
  count(HERDTYPE) %>% 
  mutate(herd_type_cat = factor(HERDTYPE, labels = c("Organic", "Conventional")))
all_herds %>% 
  count(BREED)

all_herds <- all_herds %>% 
  #' Holstein, Jersey, Other
  filter(HERDTYPE == 1, BREED == 1)

# rm(other_treat, dryoff_treat, pcr, teat_treat)
# gc()
top_herd_sizes_obs <- all_herds %>%
  count(BES_ID) %>%
  slice_max(n, n = 15) %>%
  print(n = Inf)

#'
#' This choosen herd has time between controls around 30 days, as is expected
#' in the implemented surveillance programme.
choosen_herd <- top_herd_sizes_obs %>% 
  # slice(n()%/%2)
  # filter(BES_ID == 3075212)
  filter(BES_ID == 4350212)
#'
#'
#'
one_herd <- all_herds %>% 
  semi_join(choosen_herd %>% select(BES_ID), 
            by = "BES_ID")
#' Note that the cows in this herd, might have moved to another herds,
#' so if you want to follow the cows, then retrieve them from `all_herds`.
#' 
#' 
one_herd %>%
  group_by(PARITY) %>% 
  summarise(n_cows = n_distinct(DYR_ID),
            n_obs = n(),
            prop_obs_pr_cow = n_obs / n_cows)
#'
#
#' ## Add weights to the observations based on length to next obs
#' 
#' This is to try and devaluate cows that have been taken out of regular
#' milking (and testing) due to treatment.
one_herd %>% 
  arrange(BES_ID, PARITY, DYR_ID) %>% 
  group_by(BES_ID, PARITY, DYR_ID) %>% 
  mutate(
    dur_until_next = DIM %>% diff.default() %>% c(NA),
    weight = dur_until_next %>% replace_na(replace = 1) %>% {
      pmin(1, 1/log1p(pmax(30, .) - 30))
      # 1 / plogis(., scale = 20)
      # 1 / log(. - 7 + 1)
      # 1 / log1p(.)
      # 1 / log1p(.)
    }
  ) %>% 
  ungroup() -> one_herd

one_herd$weight %>% 
  density() %>% 
  plot()
#'
#'
#'
one_herd %>% 
  # VALIDATE THE WEIGHTS
  semi_join(
    filter(., dur_until_next >= 40) %>% 
      distinct(BES_ID, DYR_ID),
    by = c("BES_ID", "DYR_ID")
  ) %>% 
  arrange(BES_ID, DYR_ID, PARITY) %>% 
  # View()
  # View()
  # distinct(dur_until_next) %>%
  # print(n=Inf)
  identity() %>% {
    ggplot(.) + 
      aes(dur_until_next, group = PARITY) +
      
      geom_density(aes(y = after_stat(density), color = "density")) +
      geom_freqpoly(aes(y = after_stat(density), color = "freqpoly")) +
      geom_vline(xintercept = 30) +
      # facet_wrap(~PARITY)
      NULL
  }

# 
#' ## Use GAM to retrieve `logSCC`-curves
#'
#'
#'
library(mgcv)
#' 
#' 
#' GAM
#' 
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
  );
# one_herd
one_herd_limited_pars <- one_herd_limited %>% 
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
      DYR_ID = unique(data$DYR_ID)
    )),
    predict = map2(output, newdata, ~ predict(.x, newdata = .y))
  ) -> one_herd_curves
#' 
#' 
#' 
one_herd_curves %>% 
  select(-data, -output) %>% 
  unnest(c(newdata, predict)) %>% 
  
  select(DYR_ID, PARITY, DIM, predict) -> 
  one_herd_curves_df

one_herd_curves_df %>% {
  ggplot(.) + 
    aes(DIM, predict, group = interaction(DYR_ID, PARITY)) +
    # aes(DIM, exp(predict), group = interaction(DYR_ID, PARITY)) + 
    geom_line(color = "grey35") +
    geom_smooth(aes(group = PARITY, color = "smooth")) + 
    scale_color_manual(values = c("smooth" = "green")) + 
    facet_wrap(~PARITY) + 
    # ylim(0, 500) + 
    NULL
}

one_herd_curves_df %>% 
  group_by(DYR_ID, PARITY) %>% 
  summarise(average = mean(predict)) %>% 
  ungroup() -> 
  one_herd_global_avg

one_herd_curves_df %>% 
  group_by(DYR_ID, PARITY) %>% 
  slice_min(
    order_by = predict,
    n = 1
  ) %>% 
  ungroup() %>% 
  mutate(global_min_dim = DIM,
         global_min_value = predict) %>% 
  select(-DIM, -predict) ->
  one_herd_global_min

#TODO: Find the first minima and include it 

one_herd_curves_df %>% 
  arrange(DYR_ID, PARITY, DIM) %>% 
  group_by(DYR_ID, PARITY) %>% 
  mutate(diff_predict = diff(predict) %>% c(NA)) %>% 
  # summarise(first_peak = which(diff_predict > 0)[[1]])
  slice(first_peak = which(diff_predict > 0)[[1]]) %>% 
  ungroup() %>%
  mutate(first_min_dim = DIM,
         first_min_value = predict) %>% 
  select(-DIM, -predict, -diff_predict) -> 
  one_herd_first_min


one_herd_curves_df %>% 
  arrange(DYR_ID, PARITY, DIM) %>% 
  group_by(DYR_ID, PARITY) %>% 
  filter(DIM %in% c(200, 300)) %>%
  summarise(slope = diff(predict) / diff(DIM), browser()) %>% 
  ungroup() -> 
  one_herd_end_of_lactation_slope

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
  group_by(PARITY) %>% 
  summarise(
    across(average:slope, list("min" = min, "max" = max,
                               "lower" = . %>% quantile(probs = 0.025), 
                               "upper" = . %>% quantile(probs = 0.975)),
           .names = "{.col}.fn{.fn}")
  ) %>% 
  ungroup() %>%
  pivot_longer(-PARITY) %>% 
  # glimpse() %>% 
  separate(name, c("var", "stat"), sep = ".fn") %>% 
  pivot_wider(names_from = stat, values_from = value) %>% 
  # glimpse() %>%
  
  filter(PARITY == 1) %>% 
  
  print(n=Inf) %>% 
  identity() 
#'
#'


# identity() %>% {
#   ggplot(.) + 
#     aes(DIM, group = PARITY) + 
#     geom_density(aes(color = factor(PARITY))) + 
#     geom_freqpoly(aes(color = factor(PARITY), y=after_stat(density))) + 
#     xlim(0, 305) + 
#     geom_vline(xintercept = 100) +
#     facet_wrap(~PARITY, scales = "free") +
#     NULL
# } %>% 
# 
# identity()



