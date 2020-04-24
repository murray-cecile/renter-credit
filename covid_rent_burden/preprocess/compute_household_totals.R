#===============================================================================#
# COMPUTE TOTAL HOUSEHOLDS
#
# Cecile Murray
#===============================================================================#

# packages
libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "haven")
lapply(libs, library, character.only = TRUE)

# data
load("data/acs/prepared_data_2018.Rdata")
load("data/acs/renters_2018.Rdata")
puma_xwalk <- read_csv("data/geo/puma_county_metro_xwalk.csv")

# functions
source("covid_rent_burden/preprocess/utils.R")

# define vulnerable sectors
vulnerable_sectors <- c("Non-essential retail",
                        "Food service",
                        "Mining",
                        "Entertainment",
                        "Non-essential manufacturing",
                        "Non-essential travel/transportation",
                        "Other services")


#===============================================================================#
# TAG VULNERABLE HOUSEHOLDS
#===============================================================================#

# these are households (all, renter) with at least one worker in a vulnerable
# sector

vulnerable_hholds <- data %>% 
  select(SERIAL, PERNUM, sector) %>% 
  mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
  group_by(SERIAL) %>% 
  summarize(vulnerable_ct = sum(is_vulnerable, na.rm = TRUE)) %>% 
  filter(vulnerable_ct > 0) %>% 
  distinct(SERIAL) %>% 
  mutate(is_vulnerable = 1)

vulnerable_renters <- renters %>% 
  select(SERIAL, PERNUM, sector) %>% 
  mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
  group_by(SERIAL) %>% 
  summarize(vulnerable_ct = sum(is_vulnerable, na.rm = TRUE)) %>% 
  mutate(is_vulnerable = if_else(vulnerable_ct > 0, 1, 0)) %>% 
  distinct(SERIAL, is_vulnerable) 


#===============================================================================#
# COMPUTE TOTAL # OF HOUSEHOLDS BY GEOGRAPHY
#===============================================================================#

us_hholds <- vulnerable_hholds %>% 
  left_join(data,
            by = "SERIAL") %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  mutate(GEOID = "00",
         NAME = "United States")

st_hholds <- vulnerable_hholds %>% 
  left_join(data,
            by = "SERIAL") %>% 
  group_by(STATEFIP) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  convert_state_code()

metro_hholds <- vulnerable_hholds %>% 
  left_join(data,
            by = "SERIAL") %>% 
  group_by(puma_id) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  allocate_puma_to_metro() %>% 
  convert_cbsa_code()

hhold_table <- bind_rows(
  us_hholds,
  st_hholds,
  metro_hholds
)

# write_excel_csv(hhold_table, path = "data/tables/household_counts.csv")

#===============================================================================#
# COMPUTE TOTAL # OF RENTER HOUSEHOLDS BY GEOGRAPHY
#===============================================================================#

us_renters <- renters %>% 
  distinct(SERIAL, HHWT) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  mutate(GEOID = "00",
         NAME = "United States")

st_renters <- renters %>% 
  distinct(SERIAL, HHWT, STATEFIP) %>% 
  group_by(STATEFIP) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  convert_state_code()

metro_renters <- renters %>% 
  distinct(SERIAL, HHWT, puma_id) %>% 
  group_by(puma_id) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  allocate_puma_to_metro() %>% 
  convert_cbsa_code()

renters_table <- bind_rows(
  us_renters,
  st_renters,
  metro_renters
)

# write_excel_csv(renters_table, "data/tables/total_renter_households.csv")
