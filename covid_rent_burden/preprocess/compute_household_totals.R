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
# COMPUTE TOTAL # OF HOUSEHOLDS BY GEOGRAPHY
#===============================================================================#

us_hholds <- data %>% 
  distinct(SERIAL,
           HHWT) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  mutate(GEOID = "00",
         NAME = "United States")

st_hholds <- data %>% 
  distinct(SERIAL,
           HHWT,
           STATEFIP) %>% 
  group_by(STATEFIP) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  convert_state_code()

metro_hholds <- data %>% 
  distinct(SERIAL,
           HHWT,
           puma_id) %>% 
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

#===============================================================================#
# TAG VULNERABLE HOUSEHOLDS
#===============================================================================#

# function to count # vulnerable per household
count_vulnerable_workers_by_hh <- function(df) {
  df %>% 
    select(SERIAL,
           HHWT,
           sector) %>% 
    mutate(
      is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)
    ) %>% 
    group_by(SERIAL) %>% 
    mutate(
      vulnerable_ct = sum(is_vulnerable, na.rm = TRUE),
      is_vulnerable = if_else(vulnerable_ct > 0, 1, 0)
    ) %>% 
    ungroup() %>% 
    distinct(SERIAL, 
             HHWT, 
             vulnerable_ct,
             is_vulnerable)
}


# these are dfs of households (all, renter) with counts of workers in vulnerable
# sectors

vulnerable_hholds <- data %>% 
  count_vulnerable_workers_by_hh() 

vulnerable_renters <- renters %>% 
  count_vulnerable_workers_by_hh()

#===============================================================================#
# TALLY RENTER HOUSEHOLDS BY # OF AFFECTED WORKERS
#===============================================================================#

# count households by # affected workers by geography 
tally_hh_by_num_vulnerable <- function(df, groups = c("")) {
  df %>% 
    group_by_at(vars(one_of(c("vulnerable_ct", groups)))) %>% 
    summarize(
      sample_size = n(),
      n_households = sum(HHWT)
    )
}

us_hh_by_num_vulnerable <- vulnerable_renters %>% 
  tally_hh_by_num_vulnerable() %>% 
  mutate(GEOID = "00",
         NAME = "United States")

st_hh_by_num_vulnerable <- vulnerable_renters %>% 
  left_join(
    select(renters, SERIAL, STATEFIP),
    by = c("SERIAL")
  ) %>% 
  tally_hh_by_num_vulnerable(groups = c("STATEFIP")) %>% 
  convert_state_code()

metro_hh_by_num_vulnerable <- vulnerable_renters %>% 
  left_join(
    select(renters, SERIAL, puma_id),
    by = c("SERIAL")
  ) %>% 
  tally_hh_by_num_vulnerable(groups = c("puma_id")) %>% 
  allocate_puma_to_metro(groups = c("vulnerable_ct")) %>% 
  convert_cbsa_code()

geo_hh_by_num_vulnerable <- bind_rows(
  us_hh_by_num_vulnerable,
  st_hh_by_num_vulnerable,
  metro_hh_by_num_vulnerable
) %>% 
  select(GEOID,
         NAME,
         vulnerable_ct,
         sample_size,
         n_households) %>% 
  dplyr::rename("n_renter_households" = "n_households")

write_csv(geo_hh_by_num_vulnerable, 
          path = "data/tables/renter_households_by_num_vulnerable.csv")


