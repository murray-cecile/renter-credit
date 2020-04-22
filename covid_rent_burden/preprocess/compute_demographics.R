#===============================================================================#
# COMPUTE AGE AND RACE DEMOGRAPHICS FOR VULNERABLE
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
# # OF BURDENED AND NEWLY VULNERABLE HHOLDS / # RENTER HOUSEHOLDS
#===============================================================================#

# function to compute share of burdened and not-burdened households with at
# least one worker in a vulnerable sector
compute_vulnerable_shares_by_geo <- function(df, geo) {
  df %>% 
    select(SERIAL, HHWT, one_of(c(geo)), sector, cost_burdened) %>% 
    left_join(vulnerable_renters,
              by = "SERIAL") %>%  
    distinct_at(vars(one_of(c("SERIAL",
                              "HHWT",
                              geo,
                              "cost_burdened",
                              "is_vulnerable")))) %>% 
    group_by_at(vars(one_of(c(geo, "cost_burdened", "is_vulnerable")))) %>% 
    summarize(sample_size_burden_vulnerable = n(),
              n_burden_vulnerable = sum(is_vulnerable, wt = HHWT)) %>% 
    ungroup() %>% 
    group_by_at(vars(one_of(geo))) %>% 
    mutate(n_households = sum(n_burden_vulnerable),
           share_vulnerable = n_burden_vulnerable / n_households) %>% 
    filter(cost_burdened != "Zero household income")
}

# compute share of vulnerable renters for US, states, and metros

us_vulnerable_shares <- renters %>% 
  mutate("GEOID" = "00") %>% 
  compute_vulnerable_shares_by_geo("GEOID") %>% 
  mutate(NAME = "United States")

st_vulnerable_shares <- renters %>% 
  compute_vulnerable_shares_by_geo("STATEFIP") %>% 
  left_join(distinct(tidycensus::fips_codes,
                     state_code,
                     state_name),
            by = c("STATEFIP" = "state_code")) %>% 
  dplyr::rename("GEOID" = "STATEFIP",
                "NAME" = "state_name")

metro_vulnerable_shares <- renters %>% 
  compute_vulnerable_shares_by_geo("puma_id") %>% 
  allocate_puma_to_metro(c("cost_burdened", "is_vulnerable")) %>% 
  mutate(share_vulnerable = n_burden_vulnerable / n_households) %>% 
  left_join(distinct(puma_xwalk,
                     cbsa_code,
                     cbsa_title),
            by = "cbsa_code") %>% 
  dplyr::rename("GEOID" = "cbsa_code",
                "NAME" = "cbsa_title") %>% 
  ungroup()

# stitch together tables from each geography
geo_vulnerable_shares <- bind_rows(
  us_vulnerable_shares,
  st_vulnerable_shares,
  mutate(metro_vulnerable_shares, GEOID = as.character(GEOID))
) %>% 
  select(GEOID, NAME, everything())


save(geo_vulnerable_shares, 
     file = "covid_rent_burden/data/geo_vulnerable_shares.Rdata")

#===============================================================================#
# COMPUTE AGE
#===============================================================================#

# compute counts of vulnerable and all people by demographic category
get_demo_by_vulnerable <- function(renter_df, full_df, groups = c("")) {
  
  # get benchmark from full population (not just renters)
  benchmark <- full_df %>% 
    group_by_at(vars(one_of(groups))) %>%
    summarize(sample_size_all = n(),
              group_total = sum(PERWT))
  
  # people in renter households with at least one vulnerable worker
  renter_df %>% 
    mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
    group_by(SERIAL) %>% 
    mutate(vulnerable_ct = sum(is_vulnerable),
           is_vulnerable_renter = if_else(vulnerable_ct > 0, PERWT, 0)) %>% 
    group_by_at(vars(one_of(groups))) %>%
    summarize(sample_size = n(),
              vulnerable_in_group = sum(is_vulnerable_renter)) %>%
    ungroup() %>% 
    left_join(benchmark, 
              by = groups)
}

# function to pivot and compute shares
prep_for_plot <- function(df, groups = c("")) {
  df %>% 
    group_by_at(vars(one_of(groups))) %>%
    mutate(total = sum(group_total),
           vulnerable_total = sum(vulnerable_in_group),
           group_share = group_total / total,
           vulnerable_share = vulnerable_in_group / vulnerable_total) %>% 
    pivot_longer(cols = c("group_share", "vulnerable_share")) 
}


# compute age and vulnerability for US, states, metros
us_age_by_vulnerable <- renters %>% 
  mutate(GEOID = "00") %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_demo_by_vulnerable(mutate(data, GEOID = "00"),
                         c("GEOID", "age_cat")) %>% 
  prep_for_plot(c("GEOID")) %>% 
  mutate(NAME = "United States")

st_age_by_vulnerable <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_demo_by_vulnerable(data,
                         c("STATEFIP", "age_cat")) %>% 
  prep_for_plot(c("STATEFIP")) %>% 
  convert_state_code()

metro_age_by_vulnerable <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_demo_by_vulnerable(data,
                         c("puma_id", "age_cat")) %>% 
  allocate_puma_to_metro(c("age_cat")) %>% 
  group_by(cbsa_code, age_cat) %>% 
  mutate(group_total = sum(vulnerable_in_group)) %>% 
  prep_for_plot(c("cbsa_code")) %>% 
  convert_cbsa_code()

# bind all geos together (name is old but reflects what's in shiny code)
geo_age_by_burden <- bind_rows(
  us_age_by_vulnerable,
  st_age_by_vulnerable,
  metro_age_by_vulnerable
) %>% 
  select(GEOID, NAME, everything())


save(geo_age_by_burden,
     file = "covid_rent_burden/data/geo_age_by_burden.Rdata")

#===============================================================================#
# RACE/ETHNICITY
#===============================================================================#

# repeat same exercise by race/ethnicity 

us_raceth_by_vulnerable <- renters %>% 
  mutate(GEOID = "00") %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_demo_by_vulnerable(mutate(data, GEOID = "00"),
                         c("GEOID", "raceth")) %>% 
  prep_for_plot(c("GEOID")) %>% 
  mutate(NAME = "United States")

st_raceth_by_vulnerable <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_demo_by_vulnerable(data,
                         c("STATEFIP", "raceth")) %>% 
  prep_for_plot(c("STATEFIP")) %>% 
  convert_state_code()

metro_raceth_by_vulnerable <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_demo_by_vulnerable(data,
                         c("puma_id", "raceth")) %>% 
  allocate_puma_to_metro(c("raceth")) %>% 
  group_by(cbsa_code, raceth) %>% 
  mutate(group_total = sum(vulnerable_in_group)) %>% 
  prep_for_plot(c("cbsa_code")) %>% 
  convert_cbsa_code()

geo_raceth_by_burden <- bind_rows(
  us_raceth_by_vulnerable,
  st_raceth_by_vulnerable,
  metro_raceth_by_vulnerable
) %>% 
  select(GEOID, NAME, everything())


save(geo_raceth_by_burden,
     file = "covid_rent_burden/data/geo_raceth_by_burden.Rdata")

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
# EXCEL EXPORT
#===============================================================================#

README <- tibble(
  "Sheet names" = c("geo_age_by_burden",
                    "geo_raceth_by_burden"),
  "Description" = c("Age distribution by household cost burden and presence of vulnerable worker",
                    "Race/ethnicity makeup by household cost burden and presence of vulnerable worker"),
  "Notes" = c("vulnerable_share is what the race/ethnicity chart actually visualizes",
              "group_share is the population benchmark")
)

write_to_excel("data/tables/demographics_backup_data.xlsx",
               c("README",
                 "geo_age_by_burden",
                 "geo_raceth_by_burden"))