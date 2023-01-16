
library(tidyverse)
library(lubridate)
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

top_herds <- all_herds %>% 
  semi_join(top_herd_sizes_obs %>% 
              select(BES_ID),
            by = "BES_ID")

top_herds %>%
  semi_join(
    top_herds %>% 
      distinct(BES_ID, PARITY, DYR_ID) %>% 
      group_by(BES_ID, PARITY) %>%
      sample_n(replace = TRUE, size = 5) %>% 
      ungroup(),
    by = c("BES_ID", "PARITY", "DYR_ID")) %>% 
  group_by(BES_ID, PARITY, DYR_ID) %>%
  # n_groups() %>% 
  
  mutate(kontrol_duration = 
           KONTROLDATO %>% diff() %>% c(NA_real_)) %>% 
  summarise(
    summ_kontrol_duration = pmax(0, kontrol_duration) %>% 
      unique() %>% 
      sort() %>% 
      list()
  ) %>% 
  unnest_wider(summ_kontrol_duration) %>% 
  # View()
  identity()
#'
#'
#' This choosen herd has time between controls around 30 days, as is expected
#' in the implemented surveillance programme.
choosen_herd <- top_herd_sizes_obs %>% 
  # slice(n()%/%2)
  filter(BES_ID == 3075212)
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
# DEBUG
# one_herd %>% 
#   group_by(PARITY, DYR_ID) %>% 
#   summarise(all_dims = list(DIM)) %>% 
#   head(10) %>% 
#   View()
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
one_herd
library(mgcv)

one_herd %>% 
  glimpse()

one_herd %>% 
  count(DYR_ID) %>% 
  print(n=Inf)

one_herd %>% 
  distinct(BES_ID, PARITY, DYR_ID) %>% 
  count(BES_ID, PARITY)

one_herd_cows_all_three_parity <- one_herd %>% 
  distinct(BES_ID, DYR_ID, PARITY) %>% 
  pivot_wider(names_from = PARITY, values_from = PARITY) %>% 
  na.omit() %>%
  # nrow() %>% 
  identity()


one_herd <- one_herd %>% 
  # filter(PARITY <= 3) %>% 
  # mutate(DYR_ID = factor(DYR_ID)) %>% 
  identity()

one_herd$BES_ID %>% n_distinct()
one_herd$BES_ID %>% levels() %>% n_distinct()

one_herd$DYR_ID %>% n_distinct()
one_herd$DYR_ID %>% levels() %>% n_distinct()

# UNCOMMENT TO RE-CACHE
# save.image("L:/004_kontrol.RData") 

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
                       s(DIM) +
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
# mutate(predict = 
#          output %>% map(. %>% predict)
#' 
#' 
one_herd_limited
print() %>% 
  mutate(DYR_ID = as.character(DYR_ID)) %>% 
  group_by(PARITY) %>% 
  nest() %>% 
  ungroup() %>% 
  slice(1) %>% 
  unnest(data) -> one_herd_par_1

one_herd_par_1$DYR_ID %>% n_distinct()
one_herd_par_1$DYR_ID %>% levels() %>% n_distinct()
one_herd_par_1 %>% 
  glimpse()

# one_herd_par_1 <- one_herd_par_1 %>% 
#   mutate(DYR_ID = factor(DYR_ID)) %>% 
#   mutate(logSCC = log(SCC)) %>% 
#   select(logSCC, SCC, DIM, DYR_ID)

library(mgcv)

one_herd_par_1 <- one_herd_par_1 %>% 
  mutate(DYR_ID = factor(DYR_ID))

one_herd_gam <- gam(logSCC ~ 
                      s(DIM) +
                      # s(DIM, by = DYR_ID),
                      s(DYR_ID, bs='re'),
                    data = one_herd_par_1, method = 'REML')


one_herd_gam %>% 
  broom:::augment.gam(
    newdata = expand_grid(
      DYR_ID = one_herd_par_1$DYR_ID %>% unique(),
      DIM = seq_len(305))) %>% 
  identity() %>% {
    ggplot(.) + 
      aes(DIM, .fitted, group = DYR_ID) +
      # aes(DIM, .fitted) + 
      geom_line(color = "grey70") + 
      geom_smooth(aes(group = NULL, color = "smooth")) + 
      ylim(2.5, 8) + 
      NULL
  }

one_herd_par_1 %>% 
  naniar::miss_var_summary()
# gam(log(SCC) ~ s(DIM, by = factor(DYR_ID)),
# gam(log(SCC) ~ s(DIM) + s(DYR_ID, bs = "re"),
# gam(logSCC ~ s(DIM) + s(DYR_ID, bs = "re"),


mgcv::gamm(logSCC ~ s(DIM),
           random = list(DYR_ID = ~ 1),
           data = one_herd_par_1) -> 
  gamm_one_herd_par_1

gamm_one_herd_par_1$lme
gamm_one_herd_par_1$gam %>% 
  # predict(newdata = expand_grid(DIM = seq_len(305))) %>% 
  predict.gam(newdata = expand_grid(DIM = 5:305),
              type = "response") %>% 
  as_tibble() %>% 
  mutate(DIM = 5:305) %>% {
    plot(.$DIM, .$value)
  }

# class()
# broom:::augment.ga()





# DEBUG
# model.matrix(~ poly(DIM, 5) + DYR_ID - 1, data =one_herd_par_1) %>% 
#   object.size() %>% 
#   format(units = "Mb")

one_herd_gam <- gam(logSCC ~ DIM +
                      s(DYR_ID, bs='re'),
                    data = one_herd_par_1, method = 'REML')

one_herd_gam %>% 
  summary()
one_herd_gam %>% 
  mgcv:::plot.gam()

expand_grid(
  DIM = seq_len(305),
  PARITY = 1:7,
) %>%
  bind_cols(
    predict.gam(one_herd_gam,
                newdata = . ,
                se.fit = FALSE) %>%
      as_tibble() %>% 
      rename(.fitted = V1, .fitted2 = V2)) %>%
  identity() %>% {
    ggplot(.) + 
      # aes(DIM, fit, groups = interaction(DYR_ID, factor(PARITY))) +
      aes(DIM, .fitted, groups = factor(PARITY)) +
      geom_line(aes(color = factor(PARITY)))
  } %>% 
  plotly::ggplotly()
