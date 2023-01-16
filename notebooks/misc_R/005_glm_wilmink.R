

# Maj Beldring Henningsen, majbh@sund.ku.dk


# Packages and settings ------------------------------------------------

library(tidyverse)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
library(nls.multstart)
library(nlme) # for nlslist

Sys.setlocale("LC_ALL","English") # date formatting
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters


theme_set(ggpubr::theme_pubclean())

# Loading data and preparing data -----------------------------------------------


load("M:/PCR_data/curve6_nlme_wilmink.RData")


# Summarize Data ------------------------------------------------------------------

# count SCC 0, 1 , 9999
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
tally 

ggplot(tally %>% filter(CELLETAL > 0)) +
  aes(x=CELLETAL, y=n) +
  geom_col() +
  scale_x_continuous(trans="log10")

tally %>% arrange(desc(n))




# ecdf plot -----------------------------------------------

dat <- tibble(x=rnorm(1e5), group=sample(letters, 1e5, TRUE))
ggplot(dat) +
  aes(x=x, col=group) +
#  geom_histogram() +
  stat_ecdf()




# nls and nlme single animal one herd ------------------------

# filter data
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


nlme_oh <- nlme(logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
                  data=one_herd,
                  fixed=a+b+k+d~1,
                  random=a+b+k+d~1,
                  groups=~DYR_ID,
                  start=c(a = 4.5, b = 0.004, k = -1.5, d = 3.2),
                  # start=coef(nls_oh),
                  na.action=na.exclude,
                  control = list(maxIter = 1200, msMaxIter = 1200))

nlme_oh <- nlme(logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
                  data=one_herd,
                  fixed=a+b+k+d~1,
                  random=a+b~1,
                  groups=~AnimalParity,
                  start=coef(nls_oh),
                  na.action=na.exclude,
                  control = list(maxIter = 1200, msMaxIter = 1200))

newdata_wilmink_lin <- expand_grid(BES_ID = one_herd$BES_ID[1], 
                                   AnimalParity = animals,
                                   PCR_TEST = c(0,1), 
                                   PARITY = c(2,3,4),
                                   DIM = 6:305) %>% 
  mutate(DYR_ID = str_extract(AnimalParity, "^(\\d+)")) %>% 
  filter(PARITY == as.numeric(str_extract(AnimalParity, "(\\d+)$")))


# lin_wilmink_lm <- glm(I(exp(logSCC)) ~ 1 + DIM + I(DIM**2) + I(DIM**3),
# lin_wilmink_lm <- glm(I(exp(logSCC)) ~ 1 + DIM + I(DIM**2) + I(DIM**3) + factor(DYR_ID),
# lin_wilmink_lm <- glm(I(exp(logSCC)) ~ 1 + DIM + I(DIM**2) + I(DIM**3) + 
                        # interaction(BES_ID, factor(PCR_TEST), factor(PARITY)),
# lin_wilmink_lm <- glm(I(exp(logSCC)) ~ 1 + DIM + I(DIM**2) + I(DIM**3) + I(DIM**4) + I(DIM**5) + I(DIM**6) + 
one_herd %>% 
  mutate(scc_weight = log1p(pmax(1, DIM - 25))) ->
  one_herd
lin_wilmink_lm <- glm(I(exp(logSCC)) ~ 1 + DIM +
                        # I(DIM**2) + I(DIM**3) +
                        # I(exp(-exp(k))),
                        # I(1/plogis(pmax(0, DIM), scale =  1)) +
                        # I(1/plogis(pmax(0, DIM), scale =  2)) +
                        # I(1/plogis(pmax(0, DIM), scale =  3)) +
                        # I(1/plogis(pmax(0, DIM), scale =  4)) +
                        # I(1/plogis(pmax(0, DIM), scale =  5)) +
                        I(1/plogis(pmax(0, DIM), scale =  6)) +
                        # I(1/plogis(pmax(0, DIM), scale =  7)) +
                        # factor(DYR_ID) +
                        # factor(PARITY) +
                        # factor(AnimalParity),
                        # factor(DYR_ID):factor(PARITY)
                        interaction(BES_ID, factor(PCR_TEST), factor(PARITY)),
                      # weights = one_herd$scc_weight,
                     data = one_herd, 
                     family = poisson(link = "log"))
lin_wilmink_lm %>% 
  summary()
newdata_wilmink_lin  

lin_wilmink_lm %>% 
  # broom::augment() %>% 
  broom::augment(newdata = newdata_wilmink_lin) %>% 
  identity() %>%  {
    ggplot(.) + 
      # aes(DIM, .fitted, groups = `factor(DYR_ID)`, color =  `factor(DYR_ID)`) + 
      # aes(DIM, .fitted, color =  `interaction(BES_ID, factor(PCR_TEST), factor(PARITY))`) + 
      # aes(DIM, .fitted, color =  interaction(BES_ID, factor(PCR_TEST), factor(PARITY))) + 
      # aes(DIM, .fitted, groups =  interaction(AnimalParity, BES_ID, factor(PCR_TEST)),
      aes(DIM, .fitted) + 
      geom_line(
        data = . %>% filter(AnimalParity %in% animals),
        aes(
        groups =  interaction(factor(DYR_ID), factor(PARITY), factor(PCR_TEST), factor(BES_ID)),
        color = AnimalParity),
        show.legend = FALSE) +
      geom_point(data = one_herd, aes(DIM, logSCC, color = AnimalParity), 
                 shape = 20,
                 show.legend = FALSE) + 
      # geom_point(aes(y = log(`I(exp(logSCC))`), color =  `factor(DYR_ID)`), show.legend = FALSE) + 
      # 
      geom_line(data=fakedata %>% filter(AnimalParity %in% animals), aes(x=DIM, y=prediction, col=AnimalParity, linetype = "nlme"),
                show.legend = FALSE) +
      facet_wrap(~factor(PCR_TEST), ncol = 1) + 
      
      # facet_wrap(~`interaction(BES_ID, factor(PCR_TEST), factor(PARITY))`, ncol = 1, labeller = label_both) + 
      scale_color_viridis_d() + 
      # expand_limits(y = 0) + 
      lims(y = c(2, 7)) +
      scale_linetype_manual(values = c(nlme = "dashed")) + 
      NULL
  }


nlme_oh
ranef(nlme_oh)


nlme_oh %>% 
  nlme:::predict.nlme(level = 0:1) %>% 
  as_tibble() %>% 
  bind_cols(usual_predict = nlme_oh %>% predict) %>% 
  nest(-AnimalParity)



# plot single curves ------------------------------------------------------
animals <- sample(unique(one_herd$AnimalParity), 5)


fakedata <- expand_grid(AnimalParity = unique(one_herd$AnimalParity), DIM = 1:305)
fakedata$prediction <- predict(nlme_oh, newdata = fakedata)

# animals <- sample(unique(one_herd$AnimalParity), 5)
ggplot() +
  geom_line(data=fakedata %>% filter(AnimalParity %in% animals), aes(x=DIM, y=prediction, col=AnimalParity)) +
  geom_point(data=one_herd %>% filter(AnimalParity %in% animals), aes(x=DIM, y=logSCC, col=AnimalParity)) +
  theme(legend.pos = "none")






