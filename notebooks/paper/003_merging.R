#'
#'
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, August 2022
#'
#' Paper woodcurvebox
#' Merging data
#' Script #3
#'

#'
# Packages and settings: ----------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling


# Loading data:
load("K:/woodcurvebox_data/paper/002_preparing.RData")



# Merging step by step to production data ----------------------------------------

## df1: Production + herd type
df1 <- left_join(production, herd, by = "BES_ID", sort="TRUE",allow.cartesian=TRUE)
df1 <- df1 %>%
  filter(DATO_FRA <= KONTROLDATO) %>%
  filter(DATO_TIL >= KONTROLDATO) %>%
  dplyr::select(-DATO_FRA, -DATO_TIL) %>%
  mutate(HERDTYPE = case_when(HERD_TYPE == 'con' ~ 1, HERD_TYPE == 'eco' ~ 0)) %>%
  dplyr::select(-HERD_TYPE)

glimpse(df1) # HERDTYPE: 1=con, 0=eco

dplyr::n_distinct(production$DYR_ID)  # 2524081 unique DYR_ID production
dplyr::n_distinct(df1$DYR_ID)         # 2522761 unique DYR_ID production
dplyr::n_distinct(production$BES_ID)  # 3933
dplyr::n_distinct(df1$BES_ID)         # 3920

rm(herd, production); gc()



# BREED ------------------------------------------
## df2: + breed

df2 <- full_join(df1, breed, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
df2 <- df2 %>%
  drop_na() %>%
  dplyr::select(-RACE) # 1=holstein, 2=jersey, 3=other dairy breeds

glimpse(df2) # BREED: 1=Holstein, 2=Jersey, 3= other
dplyr::n_distinct(df2$DYR_ID)   # 2520991
dplyr::n_distinct(df2$BES_ID)   # 3918

rm(df1, breed); gc()



# PARITY ------------------------------------------------
## df3 + df4: + calvings (create DIM + parity)

df3 <- full_join(df2, calvings, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)

# df4: calving date will be start for each lactation
df4 <- df3 %>%
  drop_na() %>%
  filter(CALVING_DATE < KONTROLDATO) %>%
  filter(KONTROLDATO - 350 < CALVING_DATE) %>% # setting max lact phase to 350 days
  arrange(DYR_ID, KONTROLDATO, desc(CALVING_DATE)) %>%
  distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)

glimpse(df4)
# data loss:
dplyr::n_distinct(df4$DYR_ID)   # 2379445
dplyr::n_distinct(df4$BES_ID)   # 3913

rm(df2, df3); gc() # saving calvings to apply parity in vacc data





# DIM ---------------------------------------------------

# add days in milk,
df4$DIM <- as.Date(as.character(df4$KONTROLDATO), format="%Y-%m-%d")-
  as.Date(as.character(df4$CALVING_DATE), format="%Y-%m-%d")

df4 <- df4 %>%
  mutate(DIM = as.numeric(DIM))

df6 <- df4 # skipped af step, so avoiding changing all df's names
rm(df4); gc()


# temp saving (saving df6 + vaccination)
save.image("K:/paper_vaccine/III_merge_vaccine_TEMP1.RData")



# prepare vaccination  ------------------------------------

# STEPS:
# ## filter dates
# ## merge with calvings to get parity
# ## count Vaccines per parity per DYR_ID
# ## Keep only 3 vaccines cows.
#
# # https://www.landbrugsinfo.dk/-/media/landbrugsinfo/public/8/8/0/sop_vaccination.pdf
# # 1. dosis gives 45 dage før den forventede kælvningsdato,
# # 2. dosis gives en måned derefter (mindst 10 dage før forventet kælvning)
# # 3. dosis gives to måneder derefter (ca. 50 dage efter kælvning
#
# # 1: create branch of master data frame with only calving date and last control date:
# # The last control hereby functions as the an alternative dry off date
# branch <- df6 %>%
#   arrange(DYR_ID, PARITY, desc(KONTROLDATO)) %>%
#   distinct(DYR_ID, PARITY, .keep_all = TRUE)
# branch <- branch %>%
#   dplyr::select(DYR_ID, PARITY, KONTROLDATO, CALVING_DATE)
#
#
# # We only look at those with 3 vaccines
# # 60 days before calving,
# # 75 days after calving.
# # count, group_by: DYR_ID
# vaccination <- left_join(branch, vaccination, by = "DYR_ID", sort="TRUE",allow.cartesian=TRUE)
# vaccination <- vaccination %>%
#   group_by(DYR_ID, PARITY) %>%
#   filter(VACCINATION_DATE + 60 > CALVING_DATE |is.na(VACCINATION_DATE)) %>%
#   filter(VACCINATION_DATE - 75 < CALVING_DATE |is.na(VACCINATION_DATE)) %>%
#   dplyr::select(DYR_ID, PARITY, VACC, VACCINATION_DATE)
# vaccination <- vaccination %>%
#   drop_na()
#
#
# # temp saving
# save.image("K:/paper_vaccine/III_merge_vaccine_TEMP2.RData")
#
# # Count Vaccination before in each lac_phase
# vaccination <- rename(count(vaccination, DYR_ID, PARITY), VACC = n)
# table(vaccination$VACC)
#
# # convert vaccinations to factors:
# vaccination <- vaccination %>%
#   mutate(VACC = factor(VACC))
#
# # temp saving (saving df6 + vaccination)
# save.image("K:/paper_vaccine/III_merge_vaccine_TEMP3.RData")
#
#
#
#
# # Join vaccinations to master, df_vacc -------------------------------
#
# # df7+8: + vaccination
# df7 <- left_join(df6, vaccination, by = c("DYR_ID", "PARITY"), sort="TRUE",allow.cartesian=TRUE)
# df8 <- df7 %>%
#   arrange(DYR_ID, KONTROLDATO) %>%
#   distinct(DYR_ID, KONTROLDATO, .keep_all = TRUE)
#
#
# #convert all NA's in vaccination to 0 (0= NOT vaccinated ever)
# df8$VACC = factor(df8$VACC, levels=c(levels(df8$VACC), 0))
# df8$VACC[is.na(df8$VACC)] = 0
#
# glimpse(df8) #
# dplyr::n_distinct(df8$DYR_ID)   # 2379445
# dplyr::n_distinct(df8$BES_ID)   # 3913
#
#
#
#
#
#
# # clean up and saving ----------------------------------------------
# df_vacc <- df8
# rm(branch, calvings, vaccination, df6, df7); gc()


save.image("K:/woodcurvebox_data/paper/003_merging.RData")


