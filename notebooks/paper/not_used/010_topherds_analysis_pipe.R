

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
  slice_max(n, n = 20) %>%
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

    # Fewer animals!
    x |>
      ungroup() |>
      count(DYR_ID) |>
      arrange(desc(n)) |>
      slice(1:100) |>
      select(DYR_ID) |>
      left_join(x, by="DYR_ID") ->
      xred

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
      as_tibble() |>
      mutate(BES_ID = x$BES_ID[1])

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

# Redo with Wilmink. What parity... (Parity)
ggplot(animal_data, aes(x=BES_ID, y=MSQ, fill=Parameter, col=Parameter)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10")

lm(log(MSQ) ~ BES_ID + Parity + PCR) #FIX: add parity in data.. Or use PCR data...
# If MSR is correlated with PCR status, then we can use MSR as appr. for disease....

nls_start <- coef(nls_woods) %>%
  as_tibble()


# FIX: use purr to fit all the curves:
# https://www.granvillematheson.com/post/nonlinear-modelling-using-nls-nlme-and-brms/


# catch the outliers and create the curves... For visualizing the outliers...



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




# brm -----------------------------------------------------
# MCMC approach with STAN

# FIX: choose priors not based on data

woodsprior <- c(
  set_prior("normal(500, 0)", nlpar = "a", lb=0, ub=1),
  set_prior("normal(1, -1)", nlpar = "b", lb=1),
  set_prior("normal(-0.01, -0.001)", nlpar = "k", lb=0),
  set_prior("normal(0.05, 0.2)", class="sigma"))

woods_brm_def <- bf(logSCC ~ log(a) + b * log(DIM) + (- k* DIM),
                             # Nonlinear variables
                             a + b + k ~ 1,
                             # Nonlinear fit
                             nl = TRUE)

woods_brm_fit <- brm(
  woods_brm_def,
  family=gaussian(),
  data = df3_top,
  prior = woodsprior )



# curves ------------------------------
# FIX: plot nls, nlme and (brm) curves all in one plot...
# FIX: compare parameters with a density plot
# https://www.granvillematheson.com/post/nonlinear-modelling-using-nls-nlme-and-brms/



# Saving -----------------------------

#save.image("K:/woodcurvebox_data/paper/010_NLS_NLME_BRM.RData")
