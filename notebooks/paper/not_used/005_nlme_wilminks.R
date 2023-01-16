

# Maj Beldring Henningsen, majbh@sund.ku.dk

# Paper woodcurvebox
# nlme with wilmink

# data:
## top 20 herds
## Parity 3
## conventional
## Holstein


# Packages and settings ----------------------------------------

library(tidyverse)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
library(nls.multstart)
library(nlme) # for nlslist


# Loading data and preparing data -------------------------------------

load("K:/woodcurvebox_data/paper/004_filtering.RData")

# only looking at parity 3
rm(df_all, df2, df4); gc()


# Only looking at top herd, parity 3
top <- df3 %>%
  ungroup() %>%
  count(BES_ID) %>%
  slice_max(n, n = 20) %>%
  ungroup()

df3_top <- df3 %>%
  ungroup() %>%
  semi_join(top %>% select(-n), by = "BES_ID") %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC) %>%
  ungroup()

rm(df3, top); gc()





# wilmink function -----------------------------------------

f_wilmink <- function(DIM, a,b,k,d){
  a + b * DIM + exp(-(exp(k)) * DIM)*d
}


# nls multistart wilmink (not grouped herd level) ------------------


# repeat for all 8 diff. datasets
nls_wilmink <- nls.multstart::nls_multstart(logSCC ~ f_wilmink(DIM, a, b, k, d),
                                         data = df3_top,
                                         lower=c(a=0, b=0, k=-5, d=0),
                                         upper=c(a=9, b=1.5, k=0, d=5),
                                         start_lower = c(a=0, b=0, k=-5, d=0),
                                         start_upper = c(a=8, b=1, k=-0.01, d=4),
                                         iter = 500,
                                         supp_errors = "Y")

nls_start <- coef(nls_wilmink) %>%
  as_tibble()


# nlme wilmink ----------------------------------------------


nlme_wilmink <- nlme(logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
                  data=df3_top,
                  fixed=a+b+k+d~1,
                  random=a+b+k+d~1,
                  groups=~BES_ID,
                  start = nls_start$value,
                  na.action=na.exclude,
                  control = list(maxIter = 1200, msMaxIter = 1200))

nlme_out_wilmink <- coef(nlme_wilmink) %>%
  as_tibble()










# Saving -----------------------------

save.image("K:/woodcurvebox_data/paper/005_nlme_wilmink.RData")
