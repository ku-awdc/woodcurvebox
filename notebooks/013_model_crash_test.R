#'
#' Maj, 11052022
#'
#' Testing when models will crash
#'
#' nls vs nlme
#'
#' Model assessment: msr. Compare individual animal with herds
#'
#'
#' Testing to do:
#' 1. Wilmink vs Woods, Observations per animal. use 10 top herds
#' 1a. Both for running and non running models: count 0.25, 0.75, mean, median for observations per unique animal
#'
#' NOTE: script in paper2_SCCmodel: 005_glm_wilmink... Here we have ran nlme with group ~DYR_ID
#'
#' FIX : Obtain herds residuals ?
#' FIX : Loop so 10 animals (?) removed until model crash
#'
#' Load data:
library(tidyverse)
load("K:/paperII/004_kontrol.RData") # using all_herds
rm(choosen_herd, one_herd_cows_all_three_parity, one_herd, top_herd_sizes_obs, top_herds)
gc()
#'
#'
#' Prepare data: top 10 herds in all data:
top_herds <- all_herds %>%
  ungroup() %>%
  count(BES_ID) %>%
  slice_max(n, n = 1) %>%
  ungroup()

data <- all_herds %>%
  ungroup() %>%
  semi_join(top_herds %>% select(-n), by = "BES_ID") %>%
  mutate(logSCC = log(SCC)) %>%
  #dplyr::filter(PARITY == 2) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC) %>%
  rename(herdID = BES_ID, cowID = DYR_ID) %>%
  ungroup()
#'
#'
#' Renaming variables to correspond to woodcurvebox::estimate_wilmink
#' "cowID","DIM","logSCC"
#'
woodcurvebox::estimate_wilmink(data = data)
woodcurvebox::estimate_woods(data = data)
#' test 1: Wilmink one herd: with all parities:  Not running - singularity issue. This is all parity.
#' test 1: woods one herd: with all parities:  Not running - singularity issue. This is all parity.
#' test 2: Wilmink one herd: Parity 2:  Not running - singularity issue. This is all parity.
#' test 2: Woods one herd: Parity 2:  Not running - singularity issue. This is all parity.
#'
#'  Conclusion 1:
#'  None of the models can run all parities from one herd (top herd..)
#'  Both fails at nlme. nls runs. Maybe to few DYR_ID observations.
#'  in nlme; herd level modelling can solve this
#'  Only parity 2: Both fails again
#'
#'
#'
#'  Testing package functions manually with the top herd:
#'
#' Wilmink:
f_wilmink <- function(DIM, a,b,k,d){
  a + b * DIM + exp(-(exp(k)) * DIM)*d
}

# stopifnot(is.data.frame(data), all(c("cowID","DIM","logSCC") %in% names(data)))

#'
#'
library(nls.multstart)
nls_wilmink <- nls_multstart(logSCC ~ f_wilmink(DIM, a, b, k, d),
                        data = data,
                        lower=c(a=0, b=0, k=-5, d=0),
                        upper=c(a=9, b=1.5, k=0, d=5),
                        start_lower = c(a=0, b=0, k=-5, d=0),
                        start_upper = c(a=8, b=1, k=-0.01, d=4),
                        iter = 500,
                        supp_errors = "Y")

if(length(unique(data[["cowID"]]))==1){
  cat("Returning single animal model\n")
  #return(coef(nls_oh))
  return(nls_wilmink)
}

wilmink_coef <- coef(nls_wilmink) %>%
  as_tibble()

library(nlme)
nlme_wilmink <- nlme(logSCC ~ f_wilmink(DIM, a, b, k, d),
                data=data,
                fixed=a+b+k+d~1,
                random=a+b+k+d~1 | cowID,
                #fixed = list(DIM + a + b + k + d ~ 1),
                #random = DIM + a + b + k + d ~ 1 | cowID,
                #groups=~cowID,
                start=wilmink_coef$value,
                na.action=na.exclude,
                control = list(maxIter = 1200, msMaxIter = 1200))

cat("Returning multiple animal model\n")
return(nlme_wilmink)
#'
#'
#'
#' woods:
f_woods <- function(DIM, a,b,c){
  a * DIM ** b * exp(-c * DIM)
}
library(nls.multstart)
nls_woods <- nls_multstart(logSCC ~ f_woods(DIM, a, b, c),
                        data = data,
                        lower=c(a=0, b=0, c=-5),
                        upper=c(a=300, b=1.5, c=5),
                        start_lower = c(a=0, b=-1, c=-1),         # Check nonlog: a = 150, b = -0.1, c = -0.003
                        start_upper = c(a=300, b=1, c=1),
                        iter = 500,
                        supp_errors = "Y")

if(length(unique(data[["cowID"]]))==1){
  cat("Returning single animal model\n")
  #return(coef(nls_oh))
  return(nls_wilmink)
}

woods_coef <- coef(nls_woods) %>%
  as_tibble()

#' FIX: try again with SCC. This could be non-log transformed
library(nlme)
nlme_woods <- nlme(logSCC ~ f_woods(DIM, a, b, c),
                data=data,
                #fixed=a+b+c~1,
                #random=a+b+c~1,
                fixed=a+b+c~1,
                random=a+b+c~1 | cowID,
                #groups=~cowID,
                start=woods_coef$value,
                na.action=na.exclude,
                control = list(maxIter = 1200, msMaxIter = 1200))

cat("Returning multiple animal model\n")
return(nlme_oh)



#'
#' RESULTS:
#'
#' 10 HERDS, PARITY 2:
#' Wilmink:
#' nls is running -> 1 set of parameter for all data
#' nlme not running; probably too few observations per cowID
#'
