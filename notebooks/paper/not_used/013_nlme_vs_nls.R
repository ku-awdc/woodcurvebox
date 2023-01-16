

# Maj, majbh@sund.ku.dk

# Paper woodcurvebox
# NLS vs NLME vs BRM
# (WOODS vs WILMINK done with residuals in 007)
# data: top 20 herds, Parity 3, conventional, Holstein

## GOAL:
# NLS curves for all 20 top herds
# NLME curves for all 20 top herds
# Combine NLS and NLME in one

# Approach with woods............

# Packages and settings ----------------------------------------

library(tidyverse)
library(pbapply)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
library(nls.multstart)
library(nlme) # for nlslist
library(brms) # for MCMC
library(purrr)
library(hrbrthemes)
library(broom)
library(viridis)


# Loading data and preparing data -------------------------------------

load("K:/woodcurvebox_data/paper/004_filtering.RData")

# only looking at parity 3
rm(df_all, df2, df4); gc()


# Only looking at top herd, parity 3
top <- df3 %>%
  ungroup() %>%
  count(BES_ID) %>%
  slice_max(n, n = 6) %>%
  ungroup()

df3_top <- df3 %>%
  ungroup() %>%
  semi_join(top %>% select(-n), by = "BES_ID") %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC) %>%
  ungroup()

rm(df3, top); gc()



# Define Woods  -----------------------------------------

f_woods <- function(DIM, loga, b, k){
  loga + b * log(DIM) + (- k* DIM)
}


# nls multistart wilmink (not grouped herd level) ------------------

# repeat for all 8 diff. datasets

df3_sep <- df3_top |>
  group_by(BES_ID) |>
  group_split() |>
  pblapply(function(x){

    # Only animals with exactly 9 obs covering a min of 250 DIM range
    x |>
      group_by(DYR_ID) |>
      mutate(Range = as.numeric(max(DIM) - min(DIM), units="days")) |>
      filter(Range >= 250) |>
      ungroup() |>
      count(DYR_ID) |>
      filter(n == 9) |>
      select(DYR_ID) |>
      left_join(x, by="DYR_ID") ->
      xred

    if(nrow(xred)==0) return(NULL)

    # Work with full data:
    # xred <- x

    nls_woods <- nls.multstart::nls_multstart(logSCC ~ f_woods(DIM, loga, b, k),
                                              data = xred,
                                              #lower=c(loga=-Inf, b=-Inf, c=-5),
                                              #upper=c(a=9, b=1.5, c=5),
                                              start_lower = c(loga=-100, b=-100, c=-100),
                                              start_upper = c(loga=100, b=100, c=100),
                                              iter = 500,
                                              supp_errors = "Y")

    xred$Prediction_nls <- predict(nls_woods)
    xred$Residuals_nls <- residuals(nls_woods)

    nls_start <- coef(nls_woods) |>
      as_tibble()

    nlme_woods <- nlme(logSCC ~ f_woods(DIM, loga, b, k),
                       data=xred,
                       fixed=loga+b+k ~ 1,
                       random=loga+b+k ~ 1,
                       groups=~DYR_ID,
                       start = nls_start$value,
                       na.action=na.exclude,
                       control = list(maxIter = 1200, msMaxIter = 1200))

    xred$Prediction_nlme <- predict(nlme_woods)
    xred$Residuals_nlme <- residuals(nlme_woods)

    xred

  }) |>
  bind_rows()

df3_sep |>
  pivot_longer(Prediction_nls:Residuals_nlme, names_to="Parameter", values_to="Value") |>
  filter(Parameter %in% c("Residuals_nls", "Residuals_nlme")) |>
  group_by(BES_ID, DYR_ID, Parameter) |>
  summarise(MSQ = mean(Value^2)) ->
  animal_data

# Redo with Wilmink. What parity... (Parity 1 vs older...)
ggplot(animal_data, aes(x=BES_ID, y=MSQ, fill=Parameter, col=Parameter)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10")

lm(log(MSQ) ~ BES_ID + Parity + PCR) #FIX: add parity in data.. Or use PCR data...
# If MSR is correlated with PCR status, then we can use MSR as appr. for disease....



# catch the outliers and create the curves... For visualizing the outliers...




