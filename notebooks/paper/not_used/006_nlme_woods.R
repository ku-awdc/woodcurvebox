

# Maj Beldring Henningsen, majbh@sund.ku.dk

# Paper woodcurvebox
# nlme with Woods

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



# Woods function -----------------------------------------

f_woods <- function(DIM, a,b,k){
  log(a) + b * log(DIM) + (- k* DIM)
}



# nls multistart with woods (not grouped herd level) ------------------


# repeat for all 8 diff. datasets
nls_woods <- nls.multstart::nls_multstart(logSCC ~ f_woods(DIM, a, b, k),
                                         data = df3_top,
                                         lower=c(a=0, b=0, c=-5),
                                         upper=c(a=9, b=1.5, c=5),
                                         start_lower = c(a=0, b=0, c=-5),
                                         start_upper = c(a=8, b=1, c=5),
                                         iter = 500,
                                         supp_errors = "Y")

nls_start <- coef(nls_woods) %>%
  as_tibble()


# nlme woods ----------------------------------------------


nlme_woods <- nlme(logSCC ~ f_woods(DIM, a, b, k),
                  data=df3_top,
                  fixed=a+b+k~1,
                  random=a+b+k~1,
                  groups=~BES_ID,
                  start = nls_start$value,
                  na.action=na.exclude,
                  control = list(maxIter = 1200, msMaxIter = 1200))

nlme_out_woods <- coef(nlme_woods) %>%
  as_tibble()






# Saving -----------------------------

save.image("K:/woodcurvebox_data/paper/006_nlme_woods_wilmink.RData")
