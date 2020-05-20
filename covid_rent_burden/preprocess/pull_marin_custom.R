#===============================================================================#
# CUSTOM PULL FOR MARIN COUNTY
#
# Cecile Murray
# 2020-05-19
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "haven")
lapply(libs, library, character.only = TRUE)

load("data/acs/prepared_data_2018.Rdata")
load("data/acs/renters_2018.Rdata")

source("covid_rent_burden/preprocess/utils.R")


# PUMA-metro crosswalk
puma_xwalk <- read_csv("data/geo/puma_county_metro_xwalk.csv")

MARIN_PUMAS <- c("0604101", "0604102")

vulnerable_sectors <- c("Non-essential retail",
                        "Food service",
                        "Mining",
                        "Entertainment",
                        "Non-essential manufacturing",
                        "Non-essential travel/transportation",
                        "Other services")


#===============================================================================#
# CHECK SAMPLE SIZE
#===============================================================================#

marin_all <- data %>% 
  filter(puma_id %in% MARIN_PUMAS)

# 600 ish total people in renter households
marin_renters <- renters %>% 
  filter(puma_id %in% MARIN_PUMAS)

# 299 renter households
marin_renters %>% 
  distinct(SERIAL, .keep_all = TRUE) %>% 
  nrow()

marin_renters %>% 
  filter(sector %in% vulnerable_sectors) %>% 
  nrow()

marin_renters %>% count(raceth) 
marin_renters %>% count(age_cat) 

marin_renters %>% 
  distinct(SERIAL, .keep_all = TRUE) %>% 
  count(cost_burdened) 

#===============================================================================#
# CREATE MARIN EXTRACT
#===============================================================================#

# use all people/households but flag renters and vulnerable
marin <- data %>% 
  filter(puma_id %in% MARIN_PUMAS) %>% 
  mutate(
    renter_hhold = if_else(OWNERSHP == 2, 1, 0),
    is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)
  ) %>% 
  group_by(SERIAL) %>% 
  mutate(num_vulnerable_workers = sum(is_vulnerable, na.rm = TRUE)) 
