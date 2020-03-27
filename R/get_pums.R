#===============================================================================#
# RECTANGUL ACS PUMS
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "janitor",
          "httr",
          "tidycensus",
          "dtplyr")
lapply(libs, library, character.only = TRUE)

# read in files and drop replicate weights
housing_file <- read_csv("data/acs/raw_pums/psam_h02.csv") %>%
  select(-matches("GTP[[:digit:]]+"))
person_file <- read_csv("data/acs/raw_pums/psam_p02.csv") %>% 
  select(-matches("GTP[[:digit:]]+"))

PERSON_VARS <- c("SERIALNO", "PUMA", "ST", "ADJINC", "PWGTP", "AGEP", "CIT",
                 "SEX", "OCCP", "PERNP", "POVPIP")

HOUSEHOLD_VARS <- c("SERIALNO", "ADJHSG", "ADJINC", "WGTP", "TYPE", "TEN")

# per instructions in ACS PUMS documentation (translated from SAS)
full_file <- left_join(person_file,
                       housing_file,
                       by = "SERIALNO") 

rm(person_file, housing_file)