
library(tidyverse)
library(ggpubr) # p values in boxplot
library(gridExtra) # gridarrange
ggplot2::theme_set(ggplot2::theme_bw())  # globally sets ggplot2 theme to theme_bw
library(GGally) # for ggpairs
library(nls.multstart)
library(nlme) # for nlslist

Sys.setlocale("LC_ALL","English") # date formatting
memory.size()            # Checking your memory size
memory.limit()           # Checking the set limit
memory.limit(size=56000) # suggest for 64 bit
options(stringsAsFactors = FALSE) # prevent factorizing caracters


## Quick look at Mossa's curve

glimpse(df_curve) #df_curve comes from which data set?

all_herds <- unique(df_curve$BES_ID)
h <- all_herds[1]

one_herd <- df_curve %>%
  ungroup() %>%
  filter(BES_ID %in% h) %>%
  filter(!is.na(DIM), !is.na(SCC), SCC > 0) %>%
  mutate(LactSet = str_c(DYR_ID, PARITY, sep="_"), logSCC = log(SCC)) %>%
  select(LactSet, DIM, SCC, MILK, logSCC, Outcome=logSCC) %>%
  filter(LactSet %in% unique(LactSet)[sample.int(50)]) %>%
  identity()


ggplot(one_herd, aes(x=DIM, y=Outcome, col=LactSet)) +
  geom_line() +
  geom_point() +
  facet_wrap(~LactSet)


startpars <- nls(
  Outcome  ~ a * (DIM ** b) * exp(-c * DIM),
  one_herd,
  start = c(
    a = 30,
    b = 0.02,
    c = 0.006
  )
)

model_oh <- nlme::nlme(
  # logSCC ~ a + b * DIM + exp(-(exp(k)) * DIM)*d,
  # logSCC  ~ a * (DIM ** b) * exp(-c * DIM),
  Outcome  ~ a * (DIM ** b) * exp(-c * DIM),
  data = one_herd,
  fixed = a + b + c ~ 1,
  random = a + b + c ~ 1,
  groups =  ~ LactSet,
  start = coef(startpars),
  na.action = na.exclude,
   verbose=TRUE,
  control = list(maxIter = 1200, msMaxIter = 1200)
)
coef(model_oh) %>%
  as.data.frame() %>%
  rownames_to_column("LactSet") ->
  results

summary(model_oh)

daily_lactation_f <- function(n, a, b, c) {
  a * n ** b * exp(-c * n)
}

dim <- 1:305
results %>%
  split(.$LactSet) %>%
  lapply(function(x) tibble(DIM=dim, PredOutcome = daily_lactation_f(dim, x$a, x$b, x$c))) %>%
  bind_rows(.id="LactSet") ->
  predictions

ggplot(predictions, aes(x=DIM, y=PredOutcome, col=LactSet)) +
  geom_line() +
  geom_point(data=one_herd, mapping=aes(x=DIM, y=Outcome, col=LactSet)) +
  facet_wrap(~LactSet) +
  theme(legend.pos="none")
