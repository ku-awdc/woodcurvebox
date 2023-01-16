#'
#'
#' Maj Beldring, majbh@sund.ku.dk
#' UCPH, August 2022
#'
#' Preparing cleaned data for merging
#' Script #2 in woodcurvebox
#'
#'
# Packages and settings: ----------------------------------------

library(tidyverse)
Sys.setlocale("LC_ALL","English") # for date formats
options(stringsAsFactors = FALSE) # prevent factorizing caracters

# Loading data: ------------------------------------------

load("K:/woodcurvebox_data/paper/001_loading.RData")


# Breed: ---------------------------------------------

breed <- breed %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Holstein"), "holstein", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Jersey"), "jersey", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "broget"), "other", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "alkerace$"), "other", RACE)) %>%
  mutate(RACE = if_else(str_detect(RACE, pattern = "Krydsning"), "other", RACE))

breed <- dplyr::filter(breed, grepl('holstein|jersey|other', RACE)) #keep only 3

# add coloumn to breed wit numerious values:
breed <- breed %>%
  mutate(BREED = case_when(RACE == "holstein" ~ 1,
                              RACE == "jersey" ~ 2,
                              RACE == "other" ~ 3))



# Vaccinations -----------------------------------------------

# # 206, 120340, 340, Mastitis Staf/E.coli, vaccination, 1299
#
# # mastitis vaccine, with 1 for vaccine:
# vaccination <- dplyr::filter(treatments,
#                               grepl('Mastitis Staf/E.coli, vaccination', DISEASE))
# # Binary vaccination status
# vaccination <- vaccination %>%
#   mutate(VACC = case_when(DISEASE == "Mastitis Staf/E.coli, vaccination" ~ 1)) %>%
#   rename(VACCINATION_DATE = TREATMENT_DATE) %>%
#   dplyr::select(-DISEASE)
#
#
# rm(treatments)
# gc()




# save cleaned data -------------------------------------------

save.image("K:/woodcurvebox_data/paper/002_preparing.RData")


