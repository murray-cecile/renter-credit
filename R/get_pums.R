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


PERSON_VARS <- c("SERIALNO", "SPORDER", "PUMA", "ST", "ADJINC", "PWGTP",
                 "AGEP", "COW", "MSP", "RELP", "SCH", "SCHL",
                 "SEMP", "WAGP", "ESR", "HICOV", "NAICSP", "SOCP",
                 "PERNP", "PINCP", "POVPIP", "RAC1P", "HISP",
                 "SFN", "SFR")

HOUSEHOLD_VARS <- c("SERIALNO", "ADJHSG", "ADJINC", "WGTP", "NP", "TYPE", 
                    "CONP", "MHP", "RNTP", "TEN", "VACS", 
                    "FES", "FINCP", "FPARC", "GRNTP", "GRPIP", "HHL",
                    "HHT", "HINCP", "PSF", "R18", "R65", "WIF",
                    "WKEXREL", "WORKSTAT")


# read in files and drop replicate weights
h1 <- read_csv("data/acs/raw_pums/psam_husa.csv") %>%
  select(-matches("GTP[[:digit:]]+")) 
h2 <- read_csv("data/acs/raw_pums/psam_husb.csv") %>%
  select(one_of(HOUSEHOLD_VARS))

h2 <- h2 %>% select(one_of(HOUSEHOLD_VARS))

h1 <- bind_rows(h1, h2)
rm(h2)
write_csv(h1, "data/acs/housing_2018.csv")
rm(h1)
  
p1 <- read_csv("data/acs/raw_pums/psam_pusa.csv") %>% 
  select(one_of(PERSON_VARS))
p2 <- read_csv("data/acs/raw_pums/psam_pusb.csv") %>% 
  select(one_of(PERSON_VARS))

p1 <- bind_rows(p1, mutate(p2, ST = as.character(ST)))
rm(p2)
write_csv(p1, "data/acs/person_2018.csv")

# per instructions in ACS PUMS documentation (translated from SAS)
full_file <- left_join(p1,
                       h1,
                       by = "SERIALNO") 

rm(p1, h1)

write_csv(full_file, "data/acs/merged_2018_acs.csv")


