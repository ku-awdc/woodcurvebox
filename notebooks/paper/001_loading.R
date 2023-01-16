#'
#'
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, August 2022
#'
#' Paper for woodcurvebox
#'
#'
# Packages and settings: ----------------------------------------

library(tidyverse)
library(lubridate) # for date wrangling
#Sys.setlocale("LC_ALL","English") # for date formats
#options(stringsAsFactors = FALSE) # prevent factorizing caracters


# 1: Loading data: ------------------------------------------
#ALL data eventually needed for final model, Regardless if not needed during testing

# treatments
#sundhed       <- read_csv("M:/data/sundhed.csv")
#lksygdomskode <- read.csv("M:/data/lksygdomskode_fixed.csv") #debugged prior loading
#AB_treatments <- read_csv("M:/AB_treatments.csv") # AB treated diseases identified by Søren & Jeanette


# calvings
kaelvninger   <- read_csv("M:/data/kaelvninger.csv")

# production
yktr          <- read_csv("M:/data/yktr.csv")

# breed
dyrinfo       <- read_csv("M:/data/dyrinfo.csv")
racekode      <- read_csv("M:/data/racekode.csv")

# dryoff
#goldninger    <- read_csv("M:/data/goldninger.csv")

# herd
brugsart      <- read_csv("M:/data/brugsart.csv")
brugsartkode  <- read_csv("M:/data/brugsartkode.csv")


# vetpcr ------------------------------------------
# vetrespcr, vetresdyr, vetpcrkode, create:vetpcr

# str(vetrespcr); str(vetresdyr); str(vetpcrkode) # udtagdato format: Date
#
# # vetrespcr:
# vetrespcr <- vetrespcr %>%
#   dplyr::select(VETPCRKD_ID, VETRESDYR_ID, ANALYSEDATAEJAFR) %>%
#   filter(!is.na(VETRESDYR_ID)) %>%
#   rename(ID = VETPCRKD_ID, PCR_VALUE = ANALYSEDATAEJAFR )
#
# # merge with vetpcrkode to include kode ID
# vetrespcr <- left_join(vetrespcr, vetpcrkode , by = "ID") %>%
#   dplyr::select(VETRESDYR_ID, PCR_VALUE, NAVNKORT) %>%
#   rename(PATHOGEN = NAVNKORT)
#
# # vetresdyr; PCR test = vetprtype 10 according to vetprovetype:
# vetresdyr <- vetresdyr %>%
#   filter(UDTAGDATO > as.Date("2009-12-31")) %>%
#   filter(VETPRTYPE_ID == 10) %>%
#   dplyr::select(ID, DYR_ID, UDTAGDATO) %>%
#   rename(VETRESDYR_ID = ID, PCR_DATE = UDTAGDATO)
#
# # create vetpcr: joining vetresdyr and vetrespcr
# vetpcr <- full_join(vetrespcr, vetresdyr, by = "VETRESDYR_ID", sort="TRUE",allow.cartesian=TRUE) %>%
#   dplyr::select(-VETRESDYR_ID) %>%
#   drop_na()
#
# # vetpcr now replace vetresdyr, vetrespcr and vetpcrkode
# rm(vetresdyr, vetrespcr, vetpcrkode); gc()

# breed ----------------------------------------------------------
# create dataset with only animal ID and breed (NA when merging: rename to `other`)

str(dyrinfo); str(racekode) # FOEDSELSDATO is date format

dyrinfo <- dyrinfo %>%
  dplyr::select(ID, RACE_ID, FOEDSELSDATO) %>%
  rename(BIRTH = FOEDSELSDATO, DYR_ID = ID)

racekode <- racekode %>%
  dplyr::select(ID, RACENAVN) %>%
  rename(RACE_ID = ID, RACE = RACENAVN )

# join racekode and dyrinfo to create breed:
breed <- left_join(dyrinfo, racekode , by = "RACE_ID") %>%
  dplyr::select(-RACE_ID) %>%
  drop_na()

rm(dyrinfo, racekode); gc()


# yktr -> production ------------------------------------------------------

str(yktr) # kontroldato format: Date

production <- yktr %>%
  dplyr::select(DYR_ID, BES_ID, KONTROLDATO, CELLETAL, KGMAELK) %>%
  drop_na() %>%
  filter(KONTROLDATO > as.Date("2009-12-31")) %>%
  rename(SCC = CELLETAL, MILK = KGMAELK)

rm(yktr); gc()



# calvings ------------------------------------------------------

str(kaelvninger) # kaelvedato format: POSIXc

calvings <- kaelvninger %>%
  mutate_if(~'POSIXt' %in% class(.x), as.Date) %>% # change date format
  dplyr::select(DYR_ID, KAELVEDATO, KAELVNINGSNR) %>%
  filter(KAELVEDATO > as.Date("2009-12-31")) %>%
  drop_na() %>%
  rename(CALVING_DATE = KAELVEDATO, PARITY = KAELVNINGSNR)

rm(kaelvninger); gc()




# vaccinations -------------------------------------------

# str(sundhed); str(lksygdomskode) # sygdomsdato format: Date
#
#
# treatments <- sundhed %>%
#   filter(SYGDOMSDATO > as.Date("2009-12-31")) %>%
#   dplyr::select(DYR_ID, SYGDOMSDATO, LKSK_ID) %>%
#   drop_na() %>%
#   rename(TREATMENT_DATE = SYGDOMSDATO, ID = LKSK_ID)
#
# treatments <- left_join(treatments, lksygdomskode , by = "ID") %>%
#   dplyr::select(DYR_ID, TREATMENT_DATE, LKSYGTEKST) %>%
#   rename(DISEASE = LKSYGTEKST)
#
# rm(sundhed, lksygdomskode, AB_treatments); gc()




# brugsart -> herd, eco vs con --------------------------------------------

str(brugsart); str(brugsartkode)

brugsart <- brugsart %>%
  rename(ID = BRUGSART_ID) %>%
  group_by(DATO_TIL) %>%
  replace_na(list(DATO_TIL = as.Date("2020-06-01")))

brugsartkode <- dplyr::filter(brugsartkode, grepl('lk', BRUGSARTTEKST)) # keep only milk herds
brugsartkode <- brugsartkode %>%
  dplyr::select(ID, BRUGSARTTEKST) %>%
  rename(HERD_TYPE = BRUGSARTTEKST) %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "logisk$"), "eco", HERD_TYPE)) %>%
  mutate(HERD_TYPE = if_else(str_detect(HERD_TYPE, pattern = "lk"), "con", HERD_TYPE))


# create herd dataset by joining code and brugsart:
herd <- left_join(brugsart, brugsartkode , by = "ID") %>%
  dplyr::select(-ID) %>%
  drop_na()

rm(brugsart, brugsartkode); gc()



# check classes for each variable ------------------------------------
# (note: better to wait changing to factors after joining)

str(breed)
str(calvings)
#str(dryoff)
str(herd)
str(production)
#str(treatments)
#str(vetpcr)



# save cleaned data: --------------------------------------------

save.image("K:/woodcurvebox_data/paper/001_loading.RData")

