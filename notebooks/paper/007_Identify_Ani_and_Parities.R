

# @majbeldring

# MAIN OUTPUT FOR PAPER - for Wilmink vs Woods, see script 8

# Paper woodcurvebox:
## fitting all data grouped by animal ID and Parity (random effects over these)
## Table 2+3 from here
## MSQ over parities found here
## retreieving single animals is also from this script.
## Residuals per DIM also in this script



# To do: anomymize herds

# Packages and settings ----------------------------------------

library(tidyverse)
library(pbapply)
library(knitr) # for kable tables
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

# ggsave("C:/Users/zjt234/PhD/PaperII_woodcurve/Figure.tiff", width = 40, height = 40, units = "cm", dpi=300)



# Loading data and preparing data -------------------------------------

# data: 8 top herds overlapping parity 1,2,3 and > 3:
load("K:/woodcurvebox_data/paper/004_filtering.RData")
rm(df1_top, df2_top, df3_top, df4_top, top); gc()


# group all parities >3 to parity 4:
df_top |>
  mutate(PARITY=replace(PARITY, PARITY > 3, 4)) ->
  df_top

df_top |>
  mutate(BES_ID = factor(BES_ID)) |>
  mutate(PARITY = factor(PARITY)) ->
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
df_top$BES_ID <- fct_drop(df_top$BES_ID)



# Define Woods  -----------------------------------------

f_woods <- function(DIM, loga, b, k){
  loga + b * log(DIM) + (- k* DIM)
}



# nls multistart wilmink (not grouped herd level) ------------------

# Creating funciton with nls -> nlme, xred data (9 obs, DIM > 250)
df_sep <- df_top |>
  group_by(BES_ID) |>
  group_split() |>
  pblapply(function(x){

    # Only animals with exactly 9 obs covering a min of 250 DIM range
    x |>
      group_by(Ani) |>
      mutate(Range = as.numeric(max(DIM) - min(DIM), units="days")) |>
      filter(Range >= 250) |>
      ungroup() |>
      count(Ani) |>
      filter(n == 9) |>
      select(Ani) |>
      left_join(x, by="Ani") ->
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
                       groups=~Ani,
                       start = nls_start$value,
                       na.action=na.exclude,
                       control = list(maxIter = 1200, msMaxIter = 1200))

    xred$Prediction_nlme <- predict(nlme_woods)
    xred$Residuals_nlme <- residuals(nlme_woods)

    xred

  }) |>
  bind_rows()


# repeat the following with the output from each parity
df_sep |>
  pivot_longer(Residuals_nlme, names_to="Parameter", values_to="Value") |>
  filter(Parameter %in% c("Residuals_nlme")) |>
  ungroup() |>
  group_by(BES_ID, DYR_ID, PARITY, Parameter) |>
  summarise(MSQ = mean(Value^2))|>
  ungroup()->
  animal_data

cbbPalette <- c("#E69F00", "#56B4E9", "#009E73","#CC79A7", "#F0E442", "#0072B2", "#D55E00")

# Boxplot MSQ, catch, here NLS vs NLME
ggplot(animal_data, aes(x=BES_ID, y=MSQ, fill=PARITY, col=PARITY)) +
  geom_boxplot() +
  labs(y= "MSR") +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        text = element_text(size = 18)) +
  scale_y_continuous(trans="log10") +
  scale_fill_manual(values=cbbPalette)

ggsave("C:/Users/zjt234/PhD/PaperII_woodcurve/Figure_Boxplot_all.tiff", width = 40, height = 20, units = "cm", dpi=300)
# width = 40, height = 20, units = "cm",


# OUTLIERS Single animals -----------------------------------------------

# make with predictions rather than LOESS..........

# plot an outlier from each parity. (can also catch them )
#1011699936_1
#1017134708_2
#1011465089_3
# 1013845877_4

# non-outlier DYR: 1017255483_1
# non-outlier: 	1014616772_1

# vosualizing the curves with GGPLOTS (standard method for observations > 1000=GAM) (not rpedicted)
df_sep |>
  filter(Ani == '1011699936_1')|>
  identity() %>%
  {
    ggplot(.) +
      aes(DIM, logSCC) +
      geom_point(shape = 20, size = 5) +
      #geom_line(color = "grey20", alpha = 2) +
      geom_smooth(se = F, orientation = "x", colour="red") +
      ggtitle("Parity 1") +
      ylim(-0.01, 9.0) +
      theme(text = element_text(size = 22)) +
      #theme_grey(base_size = 22)
      #facet_wrap(name~PARITY, scales = "free") +
      NULL
  }


# visualize predicted logSCC curves, parity 1
df_sep |>
  filter(Ani == '1011699936_1')|>
  ggplot(aes(x=DIM, y=logSCC)) +
  geom_point(size=5) +
  ylim(-0.01, 9.0) +
  geom_smooth(aes(y=Prediction_nlme), colour= "#E69F00", size = 2, se = FALSE) +
  geom_hline(yintercept = 5.3, linetype = "dashed", size = 1) +
    theme(text = element_text(size = 22)) +
    ggtitle("Parity 1") -> p_1

# visualize predicted logSCC curves, parity 2
df_sep |>
    filter(Ani == '1017134708_2')|>
    ggplot(aes(x=DIM, y=logSCC)) +
    geom_point(size=5) +
    ylim(-0.01, 9.0) +
    geom_smooth(aes(y=Prediction_nlme), colour= "#56B4E9", size = 2, se = FALSE) +
    geom_hline(yintercept = 5.3, linetype = "dashed", size = 1) +
    theme(text = element_text(size = 22)) +
    ggtitle("Parity 2") -> p_2

# visualize predicted logSCC curves, parity 3
df_sep |>
  filter(Ani == '1011465089_3')|>
  ggplot(aes(x=DIM, y=logSCC)) +
  geom_point(size=5) +
  ylim(-0.01, 9.0) +
  geom_smooth(aes(y=Prediction_nlme), colour= "#009E73", size = 2, se = FALSE) +
  geom_hline(yintercept = 5.3, linetype = "dashed", size = 1) +
  theme(text = element_text(size = 22)) +
  ggtitle("Parity 3") -> p_3

# visualize predicted logSCC curves, parity > 3
df_sep |>
  filter(Ani == '1013845877_4')|>
  ggplot(aes(x=DIM, y=logSCC)) +
  geom_point(size=5) +
  ylim(-0.01, 9.0) +
  geom_smooth(aes(y=Prediction_nlme), colour= "#CC79A7", size = 2, se = FALSE) +
  geom_hline(yintercept = 5.3, linetype = "dashed", size = 1) +
  theme(text = element_text(size = 22)) +
  ggtitle("Parity > 3") -> p_4


ggarrange(p_1, p_2, p_3, p_4,
          ncol=2, nrow=2,
          common.legend = TRUE, legend="right")

ggsave("C:/Users/zjt234/PhD/PaperII_woodcurve/001_Submission/Figure5_highMSR_Animals.tiff", width = 40, height = 20, units = "cm", dpi=300)


# plot all - Not working. Need to find the average mean at every DIM
df_sep |>
  ggplot(aes(x=DIM, y=logSCC)) +
  #geom_point(size=5) +
  geom_line(aes(y=Prediction_nlme), colour= "red", size = 2) +
  theme(text = element_text(size = 22)) +
  ggtitle("Mean")


# plot a good animal:
df_sep |>
  filter(Ani == '1014616772_1') |>
  ggplot(aes(x = DIM, y = logSCC)) +
  geom_point(size = 5) +
  geom_smooth(aes(y = Prediction_nlme), colour = "#0072B2", size = 2, se = FALSE) +
  theme(text = element_text(size = 22))
ggsave("C:/Users/zjt234/PhD/PaperII_woodcurve/001_Submission/Figure6_lowMSR_Animal.tiff", width = 40, height = 20, units = "cm", dpi=300)



# summary descriptive  ------------------------

# summary of SCC - independent variable
# herd level
df_sep |>
  select(BES_ID, logSCC) |>
  group_by(BES_ID) |>
  summarize(min = exp(min(logSCC)),
            q1 = exp(quantile(logSCC, 0.25)),
            median = exp(median(logSCC)),
            mean = exp(mean(logSCC)),
            q3 = exp(quantile(logSCC, 0.75)),
            max = exp(max(logSCC)))

# parity level
df_sep |>
  select(PARITY, logSCC) |>
  group_by(PARITY) |>
  summarize(min = exp(min(logSCC)),
            q1 = exp(quantile(logSCC, 0.25)),
            median = exp(median(logSCC)),
            mean = exp(mean(logSCC)),
            q3 = exp(quantile(logSCC, 0.75)),
            max = exp(max(logSCC)))




# Summary of MSR - herd level
animal_data |>
  select(BES_ID, MSQ) |>
  drop_na() |>
  group_by(BES_ID) |>
  summarize(min = min(MSQ),
            q1 = quantile(MSQ, 0.25),
            median = median(MSQ),
            mean = mean(MSQ),
            q3 = quantile(MSQ, 0.75),
            max = max(MSQ)) |>
  kable()


# Summary of MSR - Parity level
animal_data |>
  select(PARITY, MSQ) |>
  drop_na() |>
  group_by(PARITY) |>
  summarize(min = min(MSQ),
            q1 = quantile(MSQ, 0.25),
            median = median(MSQ),
            mean = mean(MSQ),
            q3 = quantile(MSQ, 0.75),
            max = max(MSQ)) |>
  kable()








# Evaluate Residuals over DIM --------------------------


# Boxplot, residuals NLME vs DIM
ggplot(df_sep, aes(x=DIM, y=Residuals_nlme, group=DIM)) +
  geom_boxplot(outlier.colour = "#0072B2", outlier.shape = 0.5, aes(group = cut_width(DIM, 5))) +
  labs(y= "Residuals") +
  theme(text = element_text(size = 12))
  # + ggtitle("Residuals per DIM")
ggsave("C:/Users/zjt234/PhD/PaperII_woodcurve/Figure_DIM.tiff", dpi=300)


ggplot(df_sep, aes(x=DIM, y=Residuals_nlme, group=DIM)) +
  geom_boxplot(aes(group = cut_width(DIM, 5))) +
  theme(text = element_text(size = 18)) +
  ggtitle("Residuals per DIM")




# count for summary tables in paper ----------------------------------

# if DYR_ID is wanted, just divide total SCC with 9
table1_count <- df_sep %>% count(BES_ID, sort = TRUE)


# Save ----------------------------------------------------------------------------

#save.image("K:/woodcurvebox_data/paper/007_modeleval.RData")

load("K:/woodcurvebox_data/paper/007_modeleval.RData")
