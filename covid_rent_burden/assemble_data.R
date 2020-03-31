#===============================================================================#
# DATA ASSEMBLY FOR SHINY APP
#
# Cecile Murray
#===============================================================================#

libs <- c("tidyverse",
          "purrr",
          "janitor", 
          "tidycensus")
lapply(libs, library, character.only = TRUE)


#===============================================================================#
# GEOGRAPHY
#===============================================================================#

state_list <- distinct(fips_codes, state_code, state_name) %>% 
  dplyr::rename("GEOID" = "state_code",
                "NAME" = "state_name") %>% 
  filter(GEOID < 57) # ignores territories for now

# get metros by population
metro_pop <- get_acs("metropolitan statistical area/micropolitan statistical area",
                      variables = "B01001_001",
                      year = 2018,
                      survey = "acs1",
                      cache_table = TRUE)

top100 <- metro_pop %>% 
  mutate(pop_rank = dense_rank(desc(estimate))) %>% 
  filter(pop_rank <= 100) %>% 
  select(GEOID, NAME) 

geo_list <- bind_rows(top100, state_list) %>% arrange(NAME)

# save(geo_list, file = "geo_list.Rdata")

#===============================================================================#
# STATE
#===============================================================================#

state_data <- readxl::read_xlsx("../data/tables/state_ACS_crosstabs.xlsx",
                                sheet = 2) %>% 
  mutate(GEOID = str_pad(STATEFIP, width = 2, side = "left", pad = "0"))

# save(state_data, file = "covid_rent_burden/state_data.Rdata")

