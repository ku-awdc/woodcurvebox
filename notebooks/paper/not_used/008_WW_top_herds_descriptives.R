

# Maj Beldring Henningsen, majbh@sund.ku.dk

# descriptives

# Packages and settings ----------------------------------------

library(tidyverse)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
library(nls.multstart)
library(nlme) # for nlslist

Sys.setlocale("LC_ALL","English") # date formatting
options(stringsAsFactors = FALSE) # prevent factorizing caracters
Sys.setenv(LANG = "en")



# Loading data and preparing data -------------------------------------

load("K:/woodcurvebox_data/paper/006_nlme_woods_wilmink.RData")



# Summary statistics of input variables -----------------------

# top 20 herds
df3_top %>%
  summarize(min = exp(min(logSCC)),
            q1 = exp(quantile(logSCC, 0.25)),
            median = exp(median(logSCC)),
            mean = exp(mean(logSCC)),
            q3 = exp(quantile(logSCC, 0.75)),
            max = exp(max(logSCC)))





# barplot ---------------------------------------------


# intercorrelation parameters with PCR-------------------

ggpairs(nlme_out_neg2,
        upper = list(continuous = wrap("cor", size = 8)))


df_all %>%
  ggpairs()
ggsave("K:/woodcurvebox_data/paper/gg_topherds.tiff", width = 40, height = 20, units = "cm", dpi=300)


