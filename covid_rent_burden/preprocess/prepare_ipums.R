#===============================================================================#
# PREPARE IPUMS EXTRACT
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "magrittr",
          "purrr",
          "janitor",
          "ipumsr",
          "haven")
lapply(libs, library, character.only = TRUE)

#===============================================================================#
# GEOGRAPHY LIST
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

save(geo_list, file = "data/geo_list.Rdata")

#===============================================================================#
# VULNERABLE INDUSTRY CROSSWALK
#===============================================================================#

industry_list <- readxl::read_xlsx("data/Vulnerable Industries.xlsx",
                                   sheet = "detail_xwalk") %>% 
  mutate(sector = replace_na(sector, "Not vulnerable"),
         sector = if_else(IND %in% c("0", "9920", "bbbb"),
                          "NILF/unemployed",
                          sector)) %>% 
  select(IND, sector) %>% 
  bind_rows(
    tibble("IND" = c("0"),
           "sector" = c("NILF/unemployed"))
  )

#===============================================================================#
# RECODE
#===============================================================================#


load("data/acs/acs_2018.Rdata")

# generate age categories, race/ethnicity recode
data %<>%
  select(SERIAL, NUMPREC, HHWT, HHTYPE, ADJUST, STATEFIP, METRO, PUMA,
         OWNERSHP, RENTGRS, HHINCOME, NFAMS, NSUBFAM, PERNUM, PERWT,
         SUBFAM, SEX, AGE, RACE, HISPAN, EMPSTAT, OCC, IND, CLASSWKR,
         INCTOT, INCWAGE, INCBUS00, INCEARN, POVERTY) %>% 
  mutate(
    STATEFIP = str_pad(STATEFIP, width = 2, side = "left", pad = 0),
    PUMA = str_pad(PUMA, width = 5, side = "left", pad = "0"),
    puma_id = paste0(STATEFIP, PUMA),
      age_cat = case_when(
        AGE < 18 ~ "Under 18",
        between(AGE, 18, 24) ~ "18-24",
        between(AGE, 25, 54) ~ "25-54",
        between(AGE, 55, 64) ~ "55-64",
        AGE >= 65 ~ "65+"),
    age_cat = factor(age_cat,
                     levels = c("Under 18", "18-24", "25-54",
                                "55-64", "65+")),
    raceth = case_when(
      RACE == 1 & HISPAN %in% c(0, 9) ~ "White",
      RACE == 2 & HISPAN %in% c(0, 9) ~ "Black",
      RACE == 3 & HISPAN %in% c(0, 9) ~ "Other",
      between(RACE, 4, 6) & HISPAN %in% c(0, 9) ~ "Asian",
      RACE > 6 & HISPAN %in% c(0, 9) ~ "Other",
      between(HISPAN, 1, 4) ~ "Latinx",
      TRUE ~ "Other"
    ),
    poor = if_else(POVERTY < 100, TRUE, FALSE),
    pov_level = case_when(
      POVERTY < 50 ~ "Under 50% FPL",
      between(POVERTY, 50, 100) ~ "50-100% FPL",
      between(POVERTY, 100, 150) ~ "100-150% FPL",
      between(POVERTY, 150, 300) ~ "150-300% FPL",
      POVERTY > 300 ~ "Not poor"
    ),
    pov_level = factor(pov_level,
                       levels = c("Under 50% FPL", "50-100% FPL",
                                  "100-150% FPL", "150-300% FPL",
                                  "Not poor")), 
    income_level = case_when(
      HHINCOME < 25000 ~ "Under 25K",
      between(HHINCOME, 25000, 50000) ~ "25-50K",
      between(HHINCOME, 50000, 75000) ~ "50-75K",
      between(HHINCOME, 75000, 100000) ~ "75-100K",
      between(HHINCOME, 100000, 150000) ~ "100-150K",
      HHINCOME > 150000 ~ ">150K"
    ),
    income_level = factor(income_level,
                          levels = c("Under 25K", "25-50K", "50-75K",
                                     "75-100K", "100-150K", ">150K"))) %>% 
  mutate(IND_ch = as.character(IND)) %>% 
  left_join(industry_list,
            by = c("IND_ch" = "IND"))

# 
# # check industry join
# data %>% select(IND, IND_ch, sector) %>% 
#   group_by(sector) %>% 
#   count(IND) %>% View()
#   

# filter to only renters and add household cost burden
renters <- data %>% 
  filter(OWNERSHP == 2) %>% 
  mutate(cost_burdened = case_when(
           (RENTGRS * 12) / HHINCOME > 0.3 ~ "Rent burdened",
           (RENTGRS * 12) / HHINCOME <= 0.3 ~ "Not burdened",
           HHINCOME == 0 ~ "Zero household income"))

#===============================================================================#
# SAVE
#===============================================================================#

save(data, file = "data/acs/prepared_data_2018.Rdata")
save(renters, file = "data/acs/renters_2018.Rdata")