
#'
#' Paper II - SCC curves
#'
#' nls and nlme curves with One herd
#'
#' Data: YKTR - raw data 
#'

library(tidyverse)
yktr          <- read_csv("M:/data/yktr.csv")
#'
#'
mini_yktr <- yktr %>%
  sample_n(1e6)
#'
#'
yktr_clean <- yktr %>% 
  # ONE-GO APPROACH
  filter(KGMAELK > 0, 
         KGMAELK < 100, 
         (KONTROLDATO > as.Date("2009-12-31"))) %>%
  identity()
#'
#'
max_scc <- yktr$CELLETAL %>% max(na.rm = TRUE)
max_scc
#' Gather counts on pathological SCC entries in `yktr`:
patho_scc_stats <- 
  yktr_clean %>%
  # mini_yktr %>%
  # yktr %>%
  select(SCC = CELLETAL) %>% 
  summarise(
    non_na_n = sum(!is.na(SCC)),
    na_n = n() - non_na_n,
    SCC0 = sum(SCC == 0, na.rm = TRUE),
    n_valid_scc = non_na_n - SCC0,
    SCC1 = sum(SCC == 1, na.rm = TRUE),
    SCC_2all = sum(SCC > 1, na.rm = TRUE),
    SCC_999 = sum(SCC == 9999, na.rm = TRUE)
  )
#'
#'
patho_scc_stats %>% 
  pivot_longer(SCC1:SCC_999, values_to = "counts") %>%
  mutate(props = counts / n_valid_scc) %>% 
  identity
#'
#'
#'

yktr %>%
  count(CELLETAL) ->
tally # indicating that SCC 9999, 0 and NAs should be removed

ggplot(tally %>% filter(CELLETAL > 0)) +
  aes(x=CELLETAL, y=n) +
  geom_col() +
  scale_x_continuous(trans="log10")

tally %>% arrange(desc(n))


dat <- tibble(x=rnorm(1e5), group=sample(letters, 1e5, TRUE))
ggplot(dat) +
  aes(x=x, col=group) +
#  geom_histogram() +
  stat_ecdf()

load("K:/paperII/df_model_paperI.RData") # to load df_model
rm(df_all, df1_neg, df2_neg, df3_neg, df4_neg, df1_pos, df2_pos, df3_pos, df4_pos); gc()

allbes <- unique(df_model$BES_ID)
one_herd <- df_model %>%
  filter(BES_ID == allbes[1]) %>%
  mutate(CELLETAL = exp(logSCC)) %>%
  filter(CELLETAL > 0.01, CELLETAL < 9998.9) %>%
  mutate(logSCC = log(CELLETAL)) %>%
  mutate(AnimalParity = interaction(DYR_ID, PARITY))

one_herd

f_wilmink <- function(DIM, a,b,k,d){
  a + b * DIM + exp(-(exp(k)) * DIM)*d
}


# nls multistart (not grouped herd level):
# repeat for all 6 diff. datasets
nls_oh <- nls.multstart::nls_multstart(logSCC ~ f_wilmink(DIM, a, b, k, d),
                                         data = one_herd,
#                                         lower=c(a=0, b=0, k=-5, d=0),
#                                         upper=c(a=9, b=1.5, k=0, d=5),
                                         start_lower = c(a=0, b=0, k=-5, d=0),
                                         start_upper = c(a=8, b=1, k=-0.01, d=4),
                                         iter = 500,
                                         supp_errors = "Y")

coef(nls_oh)

library(nlme)
nlme_oh <- nlme(logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
                  data=one_herd,
                  fixed=a+b+k+d~1,
                  random=a+b+k+d~1,
                  groups=~DYR_ID,
                  start=coef(nls_oh), # or start=c(a = 4.5, b = 0.004, k = -1.5, d = 3.2)
                  na.action=na.exclude,
                  control = list(maxIter = 1200, msMaxIter = 1200))

nlme_oh
ranef(nlme_oh)

fakedata <- expand_grid(AnimalParity = unique(one_herd$AnimalParity), DIM = 1:305)
fakedata$prediction <- predict(nlme_oh, newdata = fakedata, level = 0) # level = 0 will ignore the random component

animals <- sample(unique(one_herd$AnimalParity), 2)
ggplot() +
  geom_line(data=fakedata %>% filter(AnimalParity %in% animals), aes(x=DIM, y=prediction, col=AnimalParity)) +
  geom_point(data=one_herd %>% filter(AnimalParity %in% animals), aes(x=DIM, y=logSCC, col=AnimalParity)) +
  theme(legend.pos = "none")


# save cleaned data: --------------------------------------------

rm(yktr, yktr_clean); gc()
save.image("K:/paperII/yktr_summarise.RData")
