#===============================================================================#
# PREPARE ACS
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

load("data/acs/acs_2018.Rdata")

# generate age categories, race/ethnicity recode
data %<>% 
  mutate(
    age_cat = case_when(
      AGE < 20 ~ "Under20",
      between(AGE, 20, 29) ~ "20-29",
      between(AGE, 30, 39) ~ "30-39",
      between(AGE, 40, 49) ~ "40-49",
      between(AGE, 50, 59) ~ "50-59",
      between(AGE, 60, 69) ~ "60-69",
      between(AGE, 70, 79) ~ "70-79",
      between(AGE, 80, 89) ~ "80-89",
      between(AGE, 90, 99) ~ "90-99",
      AGE >= 100 ~ "100+"),
    age_cat = factor(age_cat,
                     levels = c("Under20", "20-29", "30-39", "40-49",
                                "50-59", "60-69", "70-79", "80-89",
                                "90-99", "100+")),
    raceth = case_when(
      RACE == 1 & HISPAN %in% c(0, 9) ~ "white",
      RACE == 2 & HISPAN %in% c(0, 9) ~ "black",
      RACE == 3 & HISPAN %in% c(0, 9) ~ "ai_an",
      between(RACE, 4, 6) & HISPAN %in% c(0, 9) ~ "east_asian",
      RACE > 6 & HISPAN %in% c(0, 9) ~ "other_mult",
      between(HISPAN, 1, 4) ~ "hispanic",
      TRUE ~ "not determined"
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
      HHINCOME > 150000 ~ ">150K",
    ),
    income_level = factor(income_level,
                          levels = c("Under 25K", "25-50K", "50-75K",
                                     "75-100K", "100-150K", ">150K"))) 

# filter to only renters and add household cost burden
renters <- data %>% 
  filter(OWNERSHP == 2) %>% 
  mutate(cost_burden = RENT / HHINCOME,
         burden_status = case_when(
           HHINCOME == 0 ~ "Zero income",
           cost_burden < 0.3 ~ "Not burdened",
           between(cost_burden, 0.3, 0.5) ~ "30-50% cost burdened",
           between(cost_burden, 0.500001, 1) ~ "50-100% cost burdened",
           cost_burden > 1 ~ "> 100% cost burdened"
         ),
         burden_status = factor(burden_status,
                                levels = c("Zero income", "> 100% cost burdened",
                                           "50-100% cost burdened",
                                           "30-50% cost burdened",
                                           "Not burdened")))

#===============================================================================#
# SAVE
#===============================================================================#

save(data, file = "data/acs/prepared_data_2018.Rdata")
save(renters, file = "data/acs/renters_2018.Rdata")
