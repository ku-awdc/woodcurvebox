

# Maj, majbh@sund.ku.dk

# Paper woodcurvebox

# comparing woods and Wilmink with Parity 1 and only 4 herds


# Packages and settings ----------------------------------------

library(tidyverse)
library(pbapply)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
library(nls.multstart)
library(nlme) # for nlslist
#library(brms) # for MCMC
#library(purrr)
#library(hrbrthemes)
#library(broom)
#library(viridis)



# Loading data and preparing data -------------------------------------

# data: 8 top herds overlapping parity 1,2,3 and > 3:
load("K:/woodcurvebox_data/paper/004_filtering.RData")
rm(df_top, df2_top, df3_top, df4_top, top); gc()

df_top <- df1_top

# group all parities >3 to parity 4:
# df_top |>
#   mutate(PARITY=replace(PARITY, PARITY > 3, 4)) ->
#   df_top

df_top |>
  mutate(BES_ID = factor(BES_ID)) |>
  #mutate(PARITY = factor(PARITY)) ->
  df_top


# anonymizing BES_ID and drop unused levels:
df_top$BES_ID <- recode_factor(df_top$BES_ID,
                                    '3012712' = "Herd_1",
                                    '3075212' = "Herd_2",
                                    '3282712' = "Herd_3",
                                    '3804812' = "Herd_4",
                                    '4073112' = "Herd_5",
                                    '4522612' = "Herd_6",
                                    '5760812' = "Herd_7",
                                    '9989138' = "Herd_8")

target <- c("Herd_1", "Herd_2", "Herd_3", "Herd_4")
df_top <- filter(df_top, BES_ID %in% target)

df_top$BES_ID <- fct_drop(df_top$BES_ID)



# Define Woods  -----------------------------------------

f_woods <- function(DIM, loga, b, k){
  loga + b * log(DIM) + (- k* DIM)
}

f_wilmink <- function(DIM, a, b, k){
  a + b * DIM + k*exp(-0.5 * DIM)
}

# Not running: Singularity issues
# f_wilmink <- function(DIM, a,b,k,d){
#   a + b * DIM + exp(-(exp(k)) * DIM)*d
# }


# nls multistart wilmink (not grouped herd level) ------------------

# Creating funciton with nls -> nlme, xred data (9 obs, DIM > 250)
df_sep <- df_top |>
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
                                              start_lower = c(loga=-100, b=-100, k=-100),
                                              start_upper = c(loga=100, b=100, k=100),
                                              iter = 500,
                                              supp_errors = "Y")

    nls_wilmink <- nls.multstart::nls_multstart(logSCC ~ f_wilmink(DIM, a, b, k),
                                              data = xred,
                                              #lower=c(loga=-Inf, b=-Inf, c=-5),
                                              #upper=c(a=9, b=1.5, c=5),
                                              start_lower = c(a=-100, b=-100, k=-100),
                                              start_upper = c(a=100, b=100, k=100),
                                              iter = 500,
                                              supp_errors = "Y")

    xred$Prediction_nls_wood <- predict(nls_woods)
    xred$Residuals_nls_wood <- residuals(nls_woods)
    xred$Prediction_nls_wil <- predict(nls_wilmink)
    xred$Residuals_nls_wil <- residuals(nls_wilmink)

    nls_start_woods <- coef(nls_woods) |>
      as_tibble()

    nls_start_wilmink <- coef(nls_wilmink) |>
      as_tibble()

    nlme_woods <- nlme(logSCC ~ f_woods(DIM, loga, b, k),
                       data=xred,
                       fixed=loga+b+k ~ 1,
                       random=loga+b+k ~ 1,
                       groups=~DYR_ID,
                       start = nls_start_woods$value,
                       na.action=na.exclude,
                       control = list(maxIter = 1200, msMaxIter = 1200))

    nlme_wilmink <- nlme(logSCC ~ f_wilmink(DIM, a, b, k),
                       data=xred,
                       fixed=a+b+k ~ 1,
                       random=a+b+k ~ 1,
                       groups=~DYR_ID,
                       start = nls_start_wilmink$value,
                       na.action=na.exclude,
                       control = list(maxIter = 1200, msMaxIter = 1200))

    xred$Prediction_woods <- predict(nlme_woods)
    xred$Residuals_woods <- residuals(nlme_woods)
    xred$Prediction_wilmink <- predict(nlme_wilmink)
    xred$Residuals_wilmink <- residuals(nlme_wilmink)

    xred

  }) |>
  bind_rows()


# create data
df_sep |>
  pivot_longer(Prediction_nls_wood:Residuals_wilmink, names_to="Parameter", values_to="Value") |>
  filter(Parameter %in% c("Residuals_woods", "Residuals_wilmink")) |>
  ungroup() |>
  group_by(BES_ID, DYR_ID, Parameter) |>
  summarise(MSQ = mean(Value^2))|>
  ungroup()->
  animal_data


# Boxplot MSQ, catch, here NLS vs NLME
ggplot(animal_data, aes(x=BES_ID, y=MSQ, fill=Parameter, col=Parameter)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10")
