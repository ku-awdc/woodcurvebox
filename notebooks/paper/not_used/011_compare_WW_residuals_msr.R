

# Maj Beldring Henningsen, majbh@sund.ku.dk

# fitting SCC curves
# 20 top herds
# woods vs wilmink
# nlme fit

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


# defining wilmink and woods:
f_wilmink <- function(DIM, a,b,k,d){
  a + b * DIM + exp(-(exp(k)) * DIM)*d
}

f_woods <- function(DIM, a,b,k){
  log(a) + b * log(DIM) + (- k* DIM)
}




# Summary statistics of input variables -----------------------

# repeat for all parity groups, vacc and no vacc
df3_top %>%
  summarize(min = exp(min(logSCC)),
            q1 = exp(quantile(logSCC, 0.25)),
            median = exp(median(logSCC)),
            mean = exp(mean(logSCC)),
            q3 = exp(quantile(logSCC, 0.75)),
            max = exp(max(logSCC)))




# curves  woods and wilmink -------------------------------------------

# woods:
woods_mean <- nlme_out_woods %>%
  dplyr::summarise(across(everything(), mean))

# Colour: Repeat for all parities.
curve_woods <- list(woods_mean = woods_mean) %>%
  enframe() %>%
  unnest(value) %>%
  crossing(DIM = seq_len(305)) %>%
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, a, b, k), f_woods)) %>% {
    ggplot(., aes(DIM, logSCC)) +
      geom_line(size = rel(1.5)) +
      #ylim(3.5, 6.0) +
      ggpubr::theme_classic2() +
      labs(title = "Woods, Parity 3, top 20 herds") +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(hjust = 0.5),
            #legend.position="none"
            #,legend.position = "top"
      )  +
      #geom_hline(yintercept=5.3, linetype="dashed") + #200.000 threshold
      NULL
  }



# Wilmink:
wilmink_mean <- nlme_out_wilmink %>%
  dplyr::summarise(across(everything(), mean))

# Colour: Repeat for all parities.
curve_wilmink <- list(wilmink_mean = wilmink_mean) %>%
  enframe() %>%
  unnest(value) %>%
  crossing(DIM = seq_len(305)) %>%
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, a, b, k, d), f_wilmink)) %>% {
    ggplot(., aes(DIM, logSCC)) +
      geom_line(size = rel(1.5)) +
      #ylim(3.5, 6.0) +
      ggpubr::theme_classic2() +
      labs(title = "Wilmink, Parity 3, top 20 herds") +
      theme(axis.text = element_text(size = 14),
            axis.title = element_text(size = 14),
            plot.title = element_text(hjust = 0.5),
            #legend.position="none"
            #,legend.position = "top"
      )  +
      #geom_hline(yintercept=5.3, linetype="dashed") + #200.000 threshold
      NULL
  }


curves_ww <- ggarrange(curve_wilmink, curve_woods,
                     ncol=2, nrow=1,
                     common.legend = TRUE, legend="right")
curves_ww

# save in paper folder
ggsave("C:/Users/zjt234/PhD/PaperII_woodcurvebox/final_figures/curve_ww.tiff", width = 40, height = 40, units = "cm", dpi=300)





# 3. ecdf plot --------------------------------------

# here just shown with parameter a

woods_out_bes <- nlme_out_woods %>% rownames_to_column("BES_ID") # nyt BES_ID

ecdf_woods <- woods_out_bes %>%
  select(a) %>%
  ggplot(aes(a)) +
  stat_ecdf(size = rel(1.5)) +
  ggpubr::theme_classic2() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))


wilmink_out_bes <- nlme_out_wilmink %>% rownames_to_column("BES_ID") # nyt BES_ID

ecdf_wilmink <- wilmink_out_bes %>%
  select(a) %>%
  ggplot(aes(a)) +
  stat_ecdf(size = rel(1.5)) +
  ggpubr::theme_classic2() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14))


ecdf_ww <- ggarrange(ecdf_wilmink, ecdf_woods,
                   ncol=2, nrow=1,
                   common.legend = TRUE, legend="right")
ecdf_ww
ggsave("C:/Users/zjt234/PhD/PaperII_woodcurvebox/final_figures/ecdf_ww.tiff", width = 40, height = 40, units = "cm", dpi=300)








# mean, median, sd, se, CI of wilmink parameters  -------------------

nlme_out_wilmink %>%
  summarize(median = median(a),
            mean = mean(a),
            sd = sd(a))



# FIX for my models...
hill_nls_outcomes <- df3_top %>%
  mutate(outpars = map(nlme_out_wilmink, ~broom::tidy(.x))) %>%
  select(-pf, -nlme_out_wilmink) %>%
  unnest(cols="outpars")

nlme_wilmink_summary <- nlme_out_wilmink %>%
  group_by(term) %>%
  summarise(mean = mean(estimate),
            median = median(estimate),
            sd = sd(estimate)) %>%
  ungroup()

knitr::kable(hill_nls_outcomes_summary, digits = 3)





# logSCC values at MIN, day 100 and day 150 ---------------------

min_wilmink <-
  nlme_out_wilmink %>%
  summarise(across(everything(), mean)) %>%
  #' join parameters with x-axis (`DIM`)
  crossing(DIM = seq_len(305)) %>%
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, a,b,k,d), f_wilmink)) %>%

  # identity{} : min SCC, DIM at 100, DIM at 150
  identity() %>% {
    bind_rows(
      slice_min(., logSCC, n = 1),
      filter(., DIM == 100),
      filter(., DIM == 150)
    )
  } %>%
  mutate(SCC = exp(logSCC))

# minSCC, minlogSCC and DIM at minimum retrieved from table
# delta logSCC calculated using th 100 and 150 DIM logSCC value


min_woods <-
  nlme_out_woods %>%
  summarise(across(everything(), mean)) %>%
  #' join parameters with x-axis (`DIM`)
  crossing(DIM = seq_len(305)) %>%
  #' calculate the proper `logSCC`
  mutate(logSCC = pmap_dbl(select(., DIM, a,b,k), f_woods)) %>%

  # identity{} : min SCC, DIM at 100, DIM at 150
  identity() %>% {
    bind_rows(
      slice_min(., logSCC, n = 1),
      filter(., DIM == 100),
      filter(., DIM == 150)
    )
  } %>%
  mutate(SCC = exp(logSCC))






# Q-Q plot ----------------------------------------------------


qq_wilmink <- ggqqplot(residuals(nlme_wilmink), shape=1,
                xlab = "Theoretical Quantiles",
                ylab = "Sample Quantiles", title = "Wilmink, Parity 3, top herds",
                font.x = c(14), font.y = c(14),
                color = "#F8766D")
qq_wilmink$layers[[2]]$aes_params$colour <- "black"

qq_woods <- ggqqplot(residuals(nlme_woods), shape=1,
                       xlab = "Theoretical Quantiles",
                       ylab = "Sample Quantiles", title = "Wood's, Parity 3, top herds",
                       font.x = c(14), font.y = c(14),
                       color = "#00BFC4")
qq_woods$layers[[2]]$aes_params$colour <- "black"


qq_ww <- ggarrange(qq_wilmink, qq_woods,
                    ncol=2, nrow=1,
                    common.legend = TRUE, legend="right")
qq_ww

ggsave("C:/Users/zjt234/PhD/PaperII_woodcurvebox/final_figures/qq_ww.tiff", width = 40, height = 40, units = "cm", dpi=300)




# residuals plot ---------------------------------
library(broom.mixed)

broom::augment(nlme_wilmink, data = df3_top) ->
  aug_wilmink

res_wilmink <- ggplot(aug_wilmink, aes(.fitted, .resid)) +
  ggpubr::theme_classic2() +
  #geom_point(size=1) +
  # geom_point(alpha = 1/10) +
  geom_point(shape=1, size=1, fill='white') +
  geom_hline(yintercept = 0, color = "red", size=1) +
  # geom_smooth(se=FALSE, color = "red") +
  labs(x = "Fitted", y = "Residuals", title = "Wilmink, Parity 3, top herds") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)
        #,legend.position = "top"
  ) +
  NULL

broom::augment(nlme_woods, data = df3_top) ->
  aug_woods

res_woods <- ggplot(aug_woods, aes(.fitted, .resid)) +
  ggpubr::theme_classic2() +
  #geom_point(size=1) +
  # geom_point(alpha = 1/10) +
  geom_point(shape=1, size=1, fill='white') +
  geom_hline(yintercept = 0, color = "red", size=1) +
  # geom_smooth(se=FALSE, color = "red") +
  labs(x = "Fitted", y = "Residuals", title = "Woods, Parity 3, top herds") +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size = 14)
        #,legend.position = "top"
  ) +
  NULL

resid_ww <- ggarrange(res_wilmink, res_woods,
                       ncol=2, nrow=1,
                       common.legend = TRUE, legend="right")

resid_ww
ggsave("C:/Users/zjt234/PhD/PaperII_woodcurvebox/final_figures/resid_ww.tiff", width = 40, height = 40, units = "cm", dpi=300)











