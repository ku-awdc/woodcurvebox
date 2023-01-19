

# Maj, majbh@sund.ku.dk

# Paper woodcurvebox


# To do: anomymize herds

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

load("K:/woodcurvebox_data/paper/004_filtering.RData")

#rm(df_all); gc()


# NOTE: this is made without combining Animals and parities!
# create ani coloumn first..



# Define Woods  -----------------------------------------

f_woods <- function(DIM, loga, b, k){
  loga + b * log(DIM) + (- k* DIM)
}


# redo the following, including creating animal data for all four parities

# nls multistart wilmink (not grouped herd level) ------------------

# FIX: it would have been tidyer to add a Parity coloumn and then keep that and not give residuals coloumn different names

# Creating funciton with nls -> nlme, xred data (9 obs, DIM > 250)
df4_sep <- df4_top |>
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
                       groups=~DYR_ID, # unique for animal DYR_ID*PARITY -> run by this variable (str_c)
                       start = nls_start$value,
                       na.action=na.exclude,
                       control = list(maxIter = 1200, msMaxIter = 1200))

    xred$Prediction_4 <- predict(nlme_woods)
    xred$Residuals_4 <- residuals(nlme_woods)

    xred

  }) |>
  bind_rows()


# repeat the following with the output from each parity
# Create dataframe with MSQ (mean square residual)
df4_sep |>
  pivot_longer(Residuals_4, names_to="Parameter", values_to="Value") |>
  filter(Parameter %in% c("Residuals_4")) |>
  ungroup() |>
  group_by(BES_ID, DYR_ID, Parameter) |>
  summarise(MSQ = mean(Value^2))|>
  ungroup()->
  animal_data4


bind_rows(animal_data1, animal_data2, animal_data3, animal_data4) ->
  animal_data


# Boxplot MSQ, catch, here NLS vs NLME
ggplot(animal_data, aes(x=BES_ID, y=MSQ, fill=Parameter, col=Parameter)) +
  geom_boxplot() +
  scale_y_continuous(trans="log10")





# plot an outlier from each parity. (can also catch them )
## Abnormal:
# 4: 1011187350
# 3: 1011465089
# 2: 1017134708
# 1: 1011699936

df1_sep |>
  filter(DYR_ID == '1017255483')|>
  #mutate(SCC = exp(logSCC)) |>
  #filter(SCC <= 5e2) |>
  #pivot_longer(c(SCC, logSCC)) |>
  # print()
  identity() %>%
  {
    ggplot(.) +
      aes(DIM, logSCC) +
      geom_point(shape = 20, size = 5) +
      #geom_line(color = "grey20", alpha = 2) +
      geom_smooth(se = F, orientation = "x", colour="red") +
      ggtitle("Non-outlier high SCC") +
      theme(text = element_text(size = 22)) +
      #theme_grey(base_size = 22)
      #facet_wrap(name~PARITY, scales = "free") +
      NULL
  }



save.image("K:/woodcurvebox_data/paper/006_results_parity.RData")

# linear regression, MSQ vs selected variables
lm(log(MSQ) ~ BES_ID + Parameter, data = animal_data) #FIX: add PCR and parity in data.. Or use PCR data...
# If MSR is correlated with PCR status, then we can use MSR as appr. for disease....






# summary descriptives input variables -------------------

# sumamry of logSCC
bind_rows(df1_sep, df2_sep, df3_sep, df4_sep) ->
  df_sep

df_sep |>
  select(BES_ID, logSCC) |>
  group_by(BES_ID) |>
  summarize(min = exp(min(logSCC)),
            q1 = exp(quantile(logSCC, 0.25)),
            median = exp(median(logSCC)),
            mean = exp(mean(logSCC)),
            q3 = exp(quantile(logSCC, 0.75)),
            max = exp(max(logSCC)))

df1_sep |> # repeat for all df_sep files
  summarize(min = exp(min(logSCC)),
            q1 = exp(quantile(logSCC, 0.25)),
            median = exp(median(logSCC)),
            mean = exp(mean(logSCC)),
            q3 = exp(quantile(logSCC, 0.75)),
            max = exp(max(logSCC)))


# summary of residuals
df1_sep |> # repeat for all df_sep files
  summarize(min = min(Residuals_1),
            q1 = quantile(Residuals_1, 0.25),
            median = median(Residuals_1),
            mean = mean(Residuals_1),
            q3 = quantile(Residuals_1, 0.75),
            max = max(Residuals_1))


# Residuals on herd level across parities:
# wouldn't had needed to do this, if I have instead added a parity coloumn and not given residuals different names
df4_res <- df4_sep |>
  select(DYR_ID, BES_ID, Residuals_4) |>
  rename(Res_nlme = Residuals_4)

bind_rows(df1_res, df2_res, df3_res, df4_res) ->
  df_res
rm(df1_res, df2_res, df3_res, df4_res); gc()

df_res |>
  select(BES_ID, Res_nlme) |>
  drop_na() |>
  group_by(BES_ID) |>
  summarize(min = min(Res_nlme),
            q1 = quantile(Res_nlme, 0.25),
            median = median(Res_nlme),
            mean = mean(Res_nlme),
            q3 = quantile(Res_nlme, 0.75),
            max = max(Res_nlme))










# curves with mean or median from parity 2 ------------------------

nls_2 <- nls.multstart::nls_multstart(logSCC ~ f_woods(DIM, loga, b, k),
                                          data = df2_sep,
                                          #lower=c(loga=-Inf, b=-Inf, c=-5),
                                          #upper=c(a=9, b=1.5, c=5),
                                          start_lower = c(loga=-100, b=-100, c=-100),
                                          start_upper = c(loga=100, b=100, c=100),
                                          iter = 500,
                                          supp_errors = "Y")

nls_start <- coef(nls_2) |>
  as_tibble()


nlme_1 <- nlme(logSCC ~ f_woods(DIM, loga, b, k),
                   data=df1_sep,
                   fixed=loga+b+k ~ 1,
                   random=loga+b+k ~ 1,
                   groups=~DYR_ID,
                   start = nls_start$value,
                   na.action=na.exclude,
                   control = list(maxIter = 1200, msMaxIter = 1200))

nlme_out <- nlme_2
nlme_2 <- coef(nlme_2) %>%
  as_tibble()


nlme_mean <- nlme_2 %>%
  dplyr::summarise(across(everything(), mean))


curve_2 <- nlme_mean %>%
  #' join parameters with x-axis (`DIM`)
  crossing(DIM = seq_len(305)) %>%
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, loga, b, k), f_woods)) %>% {
    ggplot(., aes(DIM, logSCC)) +
      # aes(group = BES_ID) +
      #aes(group = pctile) +
      #aes(color = median) +
      #geom_line(size = rel(1.5), colour = "#E69F00") + # orange for POS
      geom_line(size = rel(1.5), colour = "#56B4E9") + # blue for NEG
      #labs(caption = "PCR negative, parity 2") +
      ggtitle("SCC curve for Parity 2") +
      #ylim(3.5, 10.5) +

      ggpubr::theme_classic2() +
      NULL
  }
curve_2



nlme_misc <- update(nlme_2,
                       corr=corAR1(0.311))




# compare with Anova ----------------------------------------

# can't compare parities, as we need same number of observations.
# can't compare different models..
# Not sure how to use this for this study
anova(nls_2, nlme_2)







# inter correlation for parameter assessment ----------------------------------

ggpairs(nlme_2,
        upper = list(continuous = wrap("cor", size = 8)))

# saving pictures for paper:
#ggsave("C:/Users/zjt234/PhD/PaperII_woodcurve/final_figures/name.tiff", width = 40, height = 20, units = "cm", dpi=300)







# residuals plot -----------------------------------------------------------

# can be plotted when nlme_2 is not a tibble
plot(fitted(nlme_out), resid(nlme_2), pch=20); abline(h=0, col="red", lwd = 2)

# gg residuals plot with a larger sample of data: need aug data
res2 <- ggplot(aug_nlme_neg4 %>% sample_n(25e3), aes(.fitted, .resid)) +
  ggpubr::theme_classic2() +
  #geom_point(size=1) +
  # geom_point(alpha = 1/10) +
  geom_point(shape=1, size=2, fill='white') +
  geom_hline(yintercept = 0, color = 'red', size=1) +
  #geom_smooth(se=FALSE, color = "red") +
  labs(x = "Fitted", y = "Residuals", title = "Parity > 3, negative") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)
        #,legend.position = "top"
  ) +
  NULL




# QQ plot ------------------------------------------------------------

ggqqplot(residuals(nlme_out), shape=1,
                xlab = "Theoretical Quantiles", ylab = "Sample Quantiles", title = "Parity 2",
                font.x = c(14), font.y = c(14),
         color = "#F8766D")






# if I want to compare woods vs wilmink, both 3 parameters: ------------------

# reqrite the following and rename a to loga

# compare to nls output:
nlme_coef_tidy <- nlme_coef %>%
  gather(Parameter, Estimate, -PET) %>%
  mutate(Model = "NLME")

nls_coef_tidy <- hill_nls_outcomes %>%
  select(PET, Parameter=term, Estimate=estimate) %>%
  mutate(Model = "NLS")

nls_nlme_comparison <- full_join(nls_coef_tidy, nlme_coef_tidy)

ggplot(nls_nlme_comparison, aes(x=Estimate, colour=Model, fill=Model)) +
  geom_density(alpha=0.3) +
  scale_colour_manual(values=colourpal) +
  scale_fill_manual(values=colourpal) +
  facet_wrap(~Parameter, scales="free")





save.image("K:/woodcurvebox_data/paper/006_modeleval.RData")
