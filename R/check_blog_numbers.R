#===============================================================================#
# NUMBER CHECKS
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
          "haven",
          "tidycensus")
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
# NUMBER CHECKS
#===============================================================================#

renters %>% 
  select(SERIAL, PERNUM, HHWT, STATEFIP, sector) %>% 
  mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
  group_by(SERIAL) %>% 
  summarize(vulnerable_ct = sum(is_vulnerable),
            HHWT = first(HHWT),
            STATEFIP = first(STATEFIP)) %>% 
  distinct(SERIAL, HHWT, vulnerable_ct, STATEFIP) %>%
  mutate(is_vulnerable = if_else(vulnerable_ct > 0, 1, 0)) %>% 
  group_by(is_vulnerable, STATEFIP) %>% 
  summarize(sum = sum(HHWT)) %>% 
  spread(is_vulnerable, sum) %>% 
  adorn_totals() %>% 
  mutate(share = `1`/(`0`+`1`))
  

renters %>% 
  left_join(vulnerable_renters, 
            by = "SERIAL") %>% 
  # mutate(ya = if_else(between(AGE, 18, 24), 1, 0)) %>% 
  # filter(ya==1) %>% 
  group_by(is_vulnerable) %>% 
  summarize(sum = sum(PERWT))

renters %>% 
  left_join(vulnerable_renters, 
            by = "SERIAL") %>%
  filter(is_vulnerable == 1) %>% 
  distinct(SERIAL, HHWT, cost_burdened) %>% 
  group_by(cost_burdened) %>% 
  summarize(n = sum(HHWT)) %>% 
  adorn_percentages(denominator = 'col')

renters %>% 
  filter(sector != "NILF/unemployed") %>% 
  group_by(sector) %>% 
  summarize(n = sum(PERWT)) %>% 
  adorn_percentages(denominator = "col")


#===============================================================================#
# STATE #s
#===============================================================================#

renters %>% 
  left_join(vulnerable_renters, 
            by = "SERIAL") %>% 
  filter(is_vulnerable == 1,
         cost_burdened != "Zero household income") %>% 
  distinct(SERIAL, STATEFIP, HHWT) %>% 
  mutate(in_CA = if_else(STATEFIP %in% c("06", "48", "36", "12"), TRUE, FALSE)) %>% 
  group_by(STATEFIP) %>% 
  summarize(n = sum(HHWT)) %>% 
  View()
  # adorn_percentages(denominator = "col")

renters %>% 
  left_join(vulnerable_renters, 
            by = "SERIAL") %>%
  filter(is_vulnerable == 1) %>% 
  distinct(SERIAL, HHWT, STATEFIP, cost_burdened) %>% 
  group_by(STATEFIP, cost_burdened) %>% 
  summarize(n_st = sum(HHWT)) %>% 
  ungroup() %>% 
  group_by(STATEFIP) %>% 
  mutate(n = sum(n_st)) %>% 
  mutate(share = scales::number(n_st / n)) %>% 
  View()
