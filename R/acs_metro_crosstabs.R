#===============================================================================#
# ACS METRO-LEVEL CROSSTABS
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "magrittr",
          "purrr",
          "janitor",
          "ipumsr",
          "haven",
          "openxlsx")
lapply(libs, library, character.only = TRUE)

load("data/acs/prepared_data_2018.Rdata")
load("data/acs/renters_2018.Rdata")

puma_county_metro <- read_csv("data/geo/puma_county_metro_xwalk.csv")

source("R/utils.R")
source("R/theme.R")

# concatenate PUMA ID
data %<>% mutate(
  STATEFIP = str_pad(STATEFIP, width = 2, side = "left", pad = "0"),
  PUMA = str_pad(PUMA, width = 5, side = "left", pad = "0"),
  puma_id = paste0(STATEFIP, PUMA)
)
renters %<>% mutate(
  STATEFIP = str_pad(STATEFIP, width = 2, side = "left", pad = "0"),
  PUMA = str_pad(PUMA, width = 5, side = "left", pad = "0"),
  puma_id = paste0(STATEFIP, PUMA)
)

# function to allocate PUMAs to metros
allocate_puma_to_metro <- function(df, groups = c("age_cat")) {
   df %>% left_join(select(puma_county_metro,
                   puma_id,
                   stcofips,
                   afact),
            by = "puma_id") %>% 
    group_by_at(vars(one_of(c("stcofips", groups)))) %>% 
    summarize_at(vars(contains("n_"), 
                      contains("sample_size")),
                 ~ sum(. * afact)) %>% 
    left_join(select(puma_county_metro,
                     stcofips,
                     cbsa_code),
              by = "stcofips") %>% 
    ungroup() %>% 
    select(-stcofips) %>% 
    group_by_at(vars(one_of(c("cbsa_code", groups)))) %>% 
    summarize_all(~round(sum(.))) %>% 
    left_join(select(puma_county_metro,
                     cbsa_code,
                     cbsa_title,
                     is_metro),
              by = "cbsa_code") 
}


#===============================================================================#
# AGE
#===============================================================================#

# function to compute # of people, renters, and shares of renters
get_renter_age_dist <- function(df, subdf, groups = c("age_cat")) {
  
  all <- df %>%
    group_by_at(vars(one_of(groups))) %>% 
    summarize(n = sum(PERWT),
              sample_size = n())
  subset <- subdf %>%
    group_by_at(vars(one_of(groups))) %>% 
    summarize(n = sum(PERWT),
              sample_size = n())

  age_table <- full_join(all,
                         subset,
                         by = groups,
                         suffix = c("_all", "_renters")) %>%
    mutate(share_renting = n_renters / n_all,
           share_of_renters = n_renters / sum(n_renters),
           cumulative_share_of_renters = cumsum(share_of_renters))
  return(age_table)
}

metro_age_table <- get_renter_age_dist(data,
                                       renters,
                                       c("age_cat", "puma_id")) %>% 
  allocate_puma_to_metro()
  
#===============================================================================#
# POVERTY
#===============================================================================#

# summarize poverty for people in renter households
metro_renter_poverty <- renters %>% 
    group_by(age_cat, poor, puma_id) %>% 
    summarize(n = sum(PERWT),
              sample_size = n()) %>% 
    ungroup() %>% 
    mutate(poor = if_else(poor, "poor", "not_poor")) %>% 
    pivot_wider(names_from = c("poor"), 
                values_from = c("n", "sample_size")) %>%
  allocate_puma_to_metro(groups = c("age_cat")) %>% 
  mutate(total = n_poor + n_not_poor,
           povrate = n_poor / total)

#===============================================================================#
# POVERTY LEVEL
#===============================================================================#

# summarize poverty for people in renter households
metro_renter_pov_by_lvl <- renters %>%
  group_by(puma_id, pov_level, age_cat) %>% 
  summarize(n_agepov =sum(PERWT),
            sample_size = n()) %>% 
  ungroup() %>% 
  allocate_puma_to_metro(groups = c("age_cat", "pov_level")) 

#===============================================================================#
# INCOME 
#===============================================================================#

# summarize renters by income category
metro_renter_income <- renters %>% 
  group_by(puma_id, income_level) %>% 
  summarize(n_inc = sum(PERWT),
            sample_size = n()) %>% 
  allocate_puma_to_metro(groups = c("income_level"))
