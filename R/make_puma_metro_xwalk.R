#===============================================================================#
# PREPARE PUMA TO METRO CROSSWALK
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "janitor",
          "tidycensus",
          "readxl")
lapply(libs, library, character.only = TRUE)

#===============================================================================#
# READ IN DATA
#===============================================================================#

# read in PUMA to county crosswalk from MABLE-Geocorr
puma2cty_colnames <- read_csv("data/geo/puma2012_to_county.csv",
                           n_max = 1) %>%
  gather()
puma2cty <- read_csv("data/geo/puma2012_to_county.csv",
                     skip = 2,
                     col_names = puma2cty_colnames$key) %>% 
  clean_names()

# read in Census delineation file
delin18 <- readxl::read_xls("data/geo/list1_Sep_2018.xls",
                            skip = 2) %>% 
  clean_names() %>% 
  mutate(stcofips = paste0(fips_state_code,
                           fips_county_code),
         is_metro = if_else(metropolitan_micropolitan_statistical_area ==
                           "Metropolitan Statistical Area", 
                         TRUE, 
                         FALSE),
         is_micro = if_else(metropolitan_micropolitan_statistical_area ==
                           "Micropolitan Statistical Area", 
                         TRUE, 
                         FALSE))

# write this to csv for convenience
# write_csv(delin18, "data/geo/metro_delineation_2018-09.csv")

#===============================================================================#
# BUILD PUMA-COUNTY-METRO-XWALK
#===============================================================================#

# most PUMAs nest perfectly within counties, but not all
puma2cty %>% 
  ggplot(aes(x = afact)) +
  geom_freqpoly()

puma_metro_join <- puma2cty %>% 
  mutate(puma_id = paste0(state, puma12)) %>% 
  select(puma_id,
         county14,
         stab,
         cntyname2,
         hus10, 
         afact) %>% 
  left_join(select(delin18,
                   stcofips, 
                   cbsa_code,
                   cbsa_title,
                   is_metro,
                   is_micro),
            by = c("county14" = "stcofips"))

puma_metro_join %>% 
  dplyr::rename("stcofips" = "county14") %>% 
  select(puma_id,
         stcofips,
         cbsa_code,
         cbsa_title,
         is_metro,
         afact) %>% 
  write_csv("data/geo/puma_county_metro_xwalk.csv")

#===============================================================================#
# EXPLORE ALTERNATIVES
#===============================================================================#

# fewer metropolitan PUMAs don't perfectly nest
puma_metro_join %>% 
  filter(is_metro) %>% 
  ggplot(aes(x = afact)) +
  geom_freqpoly()


# assign PUMA to county where majority of housing units fall
puma_max_xwalk <- puma_metro_join %>% 
  group_by(puma_id) %>% 
  mutate(max_afact = max(afact)) %>% 
  filter(afact == max_afact)

length(which(puma_xwalk$afact < 0.5))
filter(puma_xwalk, afact < 0.5) %>% View()

puma_metro_join %>% 
  group_by(puma_id) %>% 
  mutate(max_afact = max(afact)) %>% 
  filter(afact == max_afact,
         is_metro) %>% 
  group_by(cbsa_title) %>% 
  summarize(ct = n()) %>% 
  arrange(-ct) %>% 
  View()
