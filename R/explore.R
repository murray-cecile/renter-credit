#===============================================================================#
# INITIAL CROSSTABS / DATA EXPLORATION: ACS PUMS
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "magrittr",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "ipumsr")
lapply(libs, library, character.only = TRUE)

load("data/acs/acs_2018.Rdata")

#===============================================================================#
# SUBSET DATA & RECODE
#===============================================================================#

# filter to only renters
renter18 <- data %>% filter(OWNERSHP == 2)

# generate age categories, 
renter18 %<>% 
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
    raceth = case_when(
      RACE == 1 & HISPAN %in% c(0, 9) ~ "white",
      RACE == 2 & HISPAN %in% c(0, 9) ~ "black",
      RACE == 3 & HISPAN %in% c(0, 9) ~ "ai_an",
      between(RACE, 4, 6) & HISPAN %in% c(0, 9) ~ "east_asian",
      RACE > 6 & HISPAN %in% c(0, 9) ~ "other_mult",
      between(HISPAN, 1, 4) ~ "hispanic",
      TRUE ~ "not determined"
    ),
    poor = if_else(POVERTY > 100, TRUE, FALSE)) 

#===============================================================================#
# CROSSTABS
#===============================================================================#

renter18 %>% count(age_cat, wt = PERWT)
sum(renter18$PERWT[renter18$AGE > 40])

