#'
#'
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, November 2021
#'
#' Paper Vaccines
#' Filter data for SCC modelling
#' Script #4
#'
#'
# Packages and settings: ----------------------------------------


library(tidyverse)
library(gridExtra)
library(data.table)

#Sys.setlocale("LC_ALL","English") # date formatting
#options(stringsAsFactors = FALSE) # prevent factorizing caracters


# Loading data and preparing data ---------------------------

load("K:/woodcurvebox_data/paper/003_merging.RData")
rm(calvings); gc()


# df_all --------------------------------------

# filtering milk and SCC values
# can't do all in one. Have to count data loss for the prisma
df_all <- df6 %>%
  filter(SCC > 0) %>%
  filter(SCC < 9999) %>%
  filter(MILK > 0) %>%   # not mentioned in prisma
  filter(MILK < 100) %>%  # not mentioned in prisma
  filter(BREED == 1) %>%
  filter(HERDTYPE == 1) %>%
  filter(DIM < 306) %>%
  filter(DIM > 5) %>%
  ungroup() %>%
  dplyr::select(BES_ID, DYR_ID, PARITY, DIM, SCC) %>%
  mutate(logSCC = log(SCC)) %>%
  ungroup()
#'
#'better filter:   filter(between(SCC, 1, 9998))
#'
dplyr::n_distinct(df_all$DYR_ID)  # 1526149, pre-above: 2377918
dplyr::n_distinct(df_all$BES_ID)  # 3064, pre-above: 3913



# Didiving into parity groups ---------------------------

df1 <- df_all %>%
  filter(PARITY == 1) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)

df2 <- df_all %>%
  filter(PARITY == 2) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)

df3 <- df_all %>%
  filter(PARITY == 3) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)


df4 <- df_all %>%
  filter(PARITY > 3) %>%
  group_by(BES_ID) %>%
  mutate(count = n()) %>%
  filter(count > 200) %>% # not sure if this should be added
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC)




# top herds -----------------------------------------------

# repeat for all 4 parities:
top1 <- df1 %>%
  ungroup() %>%
  count(BES_ID) %>%
  slice_max(n, n = 30) %>%
  ungroup()

# keep only herds represented in all 4 parities: result: 8 herds
top <- left_join(top1, top2, by='BES_ID') |>
  drop_na()
top <- left_join(top, top3, by= 'BES_ID') |>
  drop_na()
top <- left_join(top, top4, by= 'BES_ID') |>
  drop_na() |>
  select(-n.x, -n.y, -n.x.x, -n.y.y)

# Repeat for each parity - FIX: add a PARITY coloumn!
df4_top <- df4 %>%
  ungroup() %>%
  semi_join(top, by = "BES_ID") %>%
  dplyr::select(BES_ID, DYR_ID, DIM, logSCC) %>%
  ungroup()


df_top <- df_all %>%
  ungroup() %>%
  mutate(BES_ID = factor(BES_ID)) %>%
  semi_join(top, by = "BES_ID") %>%
  dplyr::select(BES_ID, DYR_ID, PARITY, DIM, logSCC) %>%
  ungroup()


rm(df1, df2, df3, df4, top1, top2, top3, top4, df_all); gc()



# create Ani coloumn: Parity and DYR_id combined:
df_top$Ani <- paste(df_top$DYR_ID, df_top$PARITY, sep="_")


# Save Data for modelling -----------------------------------

rm(df6); gc()
save.image("K:/woodcurvebox_data/paper/004_filtering.RData")



















# PRISMA Data loss - for Prisma in article -------------------
# FIX not done for vaccine paper yet

#do it with df8.. So reload ("K:/paper_vaccine/IV_filter_vaccine_TEMP.RData")

df0 <- df_all %>%
  filter(SCC > 0) %>%
  filter(SCC < 9999) %>%
  filter(MILK > 0) %>%   # not mentioned in prisma, but not needed as we dont have any NEG milk
  filter(MILK < 100)     # not mentioned in prisma, but not needed as we dont have any milk >100

df1 <- df0 %>%
  filter(HERDTYPE == 1)
df2 <- df1 %>%
  filter(BREED == 1)
df3 <- df2 %>%
  filter(PCR_TEST == 1)
df4 <- df3 %>%
  filter(PARITY > 1)

df4 <- df_all %>%
  filter(SCC > 0) %>%
  filter(SCC < 9999)
df5 <- df_all %>%
  filter(MILK > 0) %>%
  filter(MILK < 100)
df6 <- df_all %>%
  filter(DIM < 306) %>%
  filter(DIM > 5)

# basis
dplyr::n_distinct(df_all$DYR_ID) # 1551899
dplyr::n_distinct(df_all$BES_ID) # 3843

# after screening:
dplyr::n_distinct(df0$DYR_ID) # 1551847
dplyr::n_distinct(df0$BES_ID) # 3843

# herd type
dplyr::n_distinct(df1$DYR_ID)   # 1392592 (1551847-1392592 = 159,255)
dplyr::n_distinct(df1$BES_ID)   # 3492 (loosing 351 eco herds)

# breed
dplyr::n_distinct(df2$DYR_ID)   # 997713 (loosing 394879 animals)
dplyr::n_distinct(df2$BES_ID)   # 2970 (loosing 552 herds)

# pcr tested
dplyr::n_distinct(df3$DYR_ID)   # 214992 (loosing 782721)
dplyr::n_distinct(df3$BES_ID)   # 1492 (loosing 1478 herds)

# data in each model ready group
dplyr::n_distinct(df2_neg$DYR_ID)   #
dplyr::n_distinct(df2_neg$BES_ID)   #

dplyr::n_distinct(df2_pos$DYR_ID)   #
dplyr::n_distinct(df2_pos$BES_ID)   #

dplyr::n_distinct(df3_neg$DYR_ID)   #
dplyr::n_distinct(df3_neg$BES_ID)   #

dplyr::n_distinct(df3_pos$DYR_ID)   #
dplyr::n_distinct(df3_pos$BES_ID)   #

dplyr::n_distinct(df4_neg$DYR_ID)   #
dplyr::n_distinct(df4_neg$BES_ID)   #

dplyr::n_distinct(df4_pos$DYR_ID)   #
dplyr::n_distinct(df4_pos$BES_ID)   #


df_included <- df_all %>%
  filter(SCC > 0) %>%
  filter(SCC < 9999) %>%
  filter(MILK > 0) %>%   # not mentioned in prisma
  filter(MILK < 100) %>%  # not mentioned in prisma
  filter(PCR_TEST == 1) %>%
  filter(BREED == 1) %>%
  filter(HERDTYPE == 1) %>%
  filter(DIM < 306) %>%
  filter(DIM > 5) %>%
  filter(PARITY > 1) %>%
  ungroup() %>%
  dplyr::select(BES_ID, DYR_ID, PARITY, DIM, SCC, MILK, RES_MAJOR, RES_MINOR) %>%
  mutate(logSCC = log(SCC)) %>%
  ungroup()

dplyr::n_distinct(df_included$DYR_ID)   #
dplyr::n_distinct(df_included$BES_ID)   #


rm(df1, df2, df3, df4, df5, df6); gc()

