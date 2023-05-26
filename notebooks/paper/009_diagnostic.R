
library(tidyverse)


# FIX: if we want real AB treatments, we must use same data as for vetstat.
# Follow the same step, but keep all years and diseases.
# And only look at " forbrugt"

# diagnostic and treatments for visualized outliers:

#1011699936_1
#1017134708_2
#1011465089_3
#1013845877_4

# herds:
# '3012712' = "Herd_1",
# '3075212' = "Herd_2",
# '3282712' = "Herd_3",
# '3804812' = "Herd_4",
# '4073112' = "Herd_5",
# '4522612' = "Herd_6",
# '5760812' = "Herd_7",
# '9989138' = "Herd_8")

# "raw data" are taken from PCR cleaning data.
## yr_dim = df5 in III_merge_PCR_major4
## vetpcr from: load("K:/paperI/major4/I_clean_PCR_major4.RData")
## treatments from load("K:/paperI/major4/I_clean_PCR_major4.RData")
## # vetresdyr loaded: read_csv("M:/data/vetresdyrNY.csv") # problems with virus data

# save this data:

# identify animals in the data...:
# df 5 is from III_merge_PC_major:
Ani_yr <- yr_dim %>%
  filter(DYR_ID %in% c(1011699936, 1017134708, 1011465089, 1013845877))

Ani_vetpcr <- vetpcr %>%
  filter(DYR_ID %in% c(1011699936, 1017134708, 1011465089, 1013845877))

Ani_vetresdyr <- vetresdyr %>%
  filter(DYR_ID %in% c(1011699936, 1017134708, 1011465089, 1013845877))

# these treatments are based on assumptions based on the diagnose
Ani_treat <- treatments %>%
  filter(DYR_ID %in% c(1011699936, 1017134708, 1011465089, 1013845877))




# 1011699936 parity 1 ----------------------------------------------
yr_10116 <- Ani_yr |>
  filter(DYR_ID == 1011699936) |>
  filter(PARITY == 1)

AB_10116 <- Ani_treat |>
  filter(DYR_ID == 1011699936) |>
  filter(AB == 1)

treat_10116 <- full_join(yr_10116, AB_10116, by = "DYR_ID") |>
  distinct() |>
  filter(TREATMENT_DATE > CALVING_DATE) |>
  select(DYR_ID, CALVING_DATE, TREATMENT_DATE, DISEASE)
treat_10116$DIM <- as.Date(as.character(treat_10116$TREATMENT_DATE), format="%Y-%m-%d")-
  as.Date(as.character(treat_10116$CALVING_DATE), format="%Y-%m-%d")

treat_10116 <- treat_10116 |>
  mutate(DIM = as.numeric(DIM)) |>
  filter(DIM < 350) |>
  distinct()
# CONCLUION: Several AB treats during lactation phase

pcr_10116 <- Ani_vetpcr |>
  filter(DYR_ID == 1011699936)
# All in 2013 nd all > ct 37. So no reason to evaluate. Further. Conclusion: NO POS PCR


# To Add in paper:
## DIM 3-4: Børbetændelse
## DIM 20-23: Klovbrandbyld
## DIM 87-88: Børbetændelse



# 1017134708 parity 2 ------------------------------------------------

yr_1017 <- Ani_yr |>
  filter(DYR_ID == 1017134708) |>
  filter(PARITY == 2)

AB_1017 <- Ani_treat |>
  filter(DYR_ID == 1017134708) |>
  filter(AB == 1)
treat_1017 <- full_join(yr_1017, AB_1017, by = "DYR_ID") |>
  distinct() |>
  filter(TREATMENT_DATE > CALVING_DATE) |>
  select(DYR_ID, CALVING_DATE, TREATMENT_DATE, DISEASE)
treat_1017$DIM <- as.Date(as.character(treat_1017$TREATMENT_DATE), format="%Y-%m-%d")-
  as.Date(as.character(treat_1017$CALVING_DATE), format="%Y-%m-%d")

treat_1017 <- treat_1017 |>
  mutate(DIM = as.numeric(DIM)) |>
  filter(DIM < 350) |>
  distinct()
# CONCLUION: AB treat at day 2.
# We don't expect lemmelidelse was treated with AB. As we see no reaction to SCC
# check up on this in ? See what data we used in vetstat project. Import this


pcr_1017 <- Ani_vetpcr |>
  filter(DYR_ID == 1017134708)
# No PCR tests done

vetres_1017 <- Ani_vetresdyr |>
  filter(DYR_ID == 1017134708)
# Only something before Parity 2, so not relevant


# To Add in paper:
## DIM 2: Børbetændelse
## DIM 140-143: Lemmelidelse



# 1011465089 parity 3 --------------------------------------------
yr_10114 <- Ani_yr |>
  filter(DYR_ID == 1011465089) |>
  filter(PARITY == 3)

AB_10114 <- Ani_treat |>
  filter(DYR_ID == 1011465089) |>
  filter(AB == 1)

treat_10114 <- full_join(yr_10114, AB_10114, by = "DYR_ID") |>
  distinct() |>
  filter(TREATMENT_DATE > CALVING_DATE) |>
  select(DYR_ID, CALVING_DATE, TREATMENT_DATE, DISEASE)
treat_10114$DIM <- as.Date(as.character(treat_10114$TREATMENT_DATE), format="%Y-%m-%d")-
  as.Date(as.character(treat_10114$CALVING_DATE), format="%Y-%m-%d")

treat_10114 <- treat_10114 |>
  mutate(DIM = as.numeric(DIM)) |>
  filter(DIM < 350) |>
  distinct()
# CONCLUION: Several AB treats during lactation phase


pcr_10114 <- Ani_vetpcr |>
  filter(DYR_ID == 1011465089)
diagn_10114 <- full_join(yr_10114, pcr_10114, by = "DYR_ID") |>
  distinct() |>
  filter(PCR_DATE > CALVING_DATE) |>
  select(DYR_ID, CALVING_DATE, PCR_DATE, PATHOGEN, PCR_VALUE)
diagn_10114$DIM <- as.Date(as.character(diagn_10114$PCR_DATE), format="%Y-%m-%d")-
  as.Date(as.character(treat_10114$CALVING_DATE), format="%Y-%m-%d")
diagn_10114 <- diagn_10114 |>
  mutate(DIM = as.numeric(DIM)) |>
  filter(DIM < 350) |>
  distinct()
# Only what looks like PCR at dry-off

vetres_10114 <- Ani_vetresdyr |>
  filter(DYR_ID == 1011465089)
# All PCR, so all should appear above


# To Add in paper:
## DIM 102-105: Mastitis
## DIM 118-121: Mastitis
## DIm 319-323: Infektion? (not needed)



# 1013845877 parity 4 ---------------------------------

yr_1013 <- Ani_yr |>
  filter(DYR_ID == 1013845877) |>
  filter(PARITY == 4)

AB_1013 <- Ani_treat |>
  filter(DYR_ID == 1013845877) |>
  filter(AB == 1)

treat_1013 <- full_join(yr_1013, AB_1013, by = "DYR_ID") |>
  distinct() |>
  filter(TREATMENT_DATE > CALVING_DATE) |>
  select(DYR_ID, CALVING_DATE, TREATMENT_DATE, DISEASE)
treat_1013$DIM <- as.Date(as.character(treat_1013$TREATMENT_DATE), format="%Y-%m-%d")-
  as.Date(as.character(treat_1013$CALVING_DATE), format="%Y-%m-%d")

treat_1013 <- treat_1013 |>
  mutate(DIM = as.numeric(DIM)) |>
  filter(DIM < 350) |>
  distinct()
# CONCLUION: Several AB treats during lactation phase


pcr_1013 <- Ani_vetpcr |>
  filter(DYR_ID == 1013845877)
diagn_1013 <- full_join(yr_1013, pcr_1013, by = "DYR_ID") |>
  distinct() |>
  filter(PCR_DATE > CALVING_DATE) |>
  select(DYR_ID, CALVING_DATE, PCR_DATE, PATHOGEN, PCR_VALUE)
diagn_1013$DIM <- as.Date(as.character(diagn_1013$PCR_DATE), format="%Y-%m-%d")-
  as.Date(as.character(treat_1013$CALVING_DATE), format="%Y-%m-%d")
diagn_1013 <- diagn_1013 |>
  mutate(DIM = as.numeric(DIM)) |>
  filter(DIM < 350) |>
  distinct()
# Multiple POS results for Mastitis pathogens for PCR for treat at dry-off

vetres_1013 <- Ani_vetresdyr |>
  filter(DYR_ID == 1013845877)




# GOOD animal -------------------------------------------------------------

# 1014616772_1


yr_good <- yr_dim |>
  filter(DYR_ID == 1014616772) |>
  filter(PARITY == 1)

AB_good <- treatments |>
  filter(DYR_ID == 1014616772) |>
  filter(AB == 1)

treat_good <- full_join(yr_good, AB_good, by = "DYR_ID") |>
  distinct() |>
  filter(TREATMENT_DATE > CALVING_DATE) |>
  select(DYR_ID, CALVING_DATE, TREATMENT_DATE, DISEASE)
treat_good$DIM <- as.Date(as.character(treat_good$TREATMENT_DATE), format="%Y-%m-%d")-
  as.Date(as.character(treat_good$CALVING_DATE), format="%Y-%m-%d")

treat_good <- treat_good |>
  mutate(DIM = as.numeric(DIM)) |>
  filter(DIM < 350) |>
  distinct()
# CONCLUION: Several AB treats during lactation phase


pcr_good <- Ani_vetpcr |>
  filter(DYR_ID == 1014616772)
diagn_good <- full_join(yr_good, pcr_good, by = "DYR_ID") |>
  distinct() |>
  filter(PCR_DATE > CALVING_DATE) |>
  select(DYR_ID, CALVING_DATE, PCR_DATE, PATHOGEN, PCR_VALUE)
diagn_good$DIM <- as.Date(as.character(diagn_good$PCR_DATE), format="%Y-%m-%d")-
  as.Date(as.character(treat_good$CALVING_DATE), format="%Y-%m-%d")
diagn_good <- diagn_1013 |>
  mutate(DIM = as.numeric(DIM)) |>
  filter(DIM < 350) |>
  distinct()
# Multiple POS results for Mastitis pathogens for PCR for treat at dry-off

vetres_good <- Ani_vetresdyr |>
  filter(DYR_ID == 1014616772)



# To Add in paper:
## DIM 181-183 + 188: Mastitis AKUT
## Also chose to do treatment at dry-off. Must be based on a pcr




