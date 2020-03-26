#===============================================================================#
# NATIONAL CROSSTABS
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

source("R/utils.R")
source("R/theme.R")

#===============================================================================#
# AGE
#===============================================================================#

# function to compute # of people, renters, and shares of renters
get_renter_age_dist <- function(df, subdf) {
  age_table <- full_join(
    count(data, age_cat, wt = PERWT, sort = TRUE),
    count(renters, age_cat, wt = PERWT, sort = TRUE),
    by = "age_cat",
    suffix = c("_all", "_renters")
  ) %>% 
    mutate(share_renting = n_renters / n_all,
           share_of_renters = n_renters / sum(n_renters),
           cumulative_share_of_renters = cumsum(share_of_renters))
  return(age_table)
}

natl_age_table <- get_renter_age_dist(data, renters)

#===============================================================================#
# POVERTY
#===============================================================================#

# summarize poverty for people in renter households
get_pov_by_age_dist <- function(df) {
  rv <- df %>% 
    group_by(poor) %>% 
    count(age_cat, wt = PERWT, sort = TRUE) %>% 
    spread("poor", "n") %>% 
    clean_names() %>% 
    dplyr::rename("not_poor" = "false",
                  "poor" = "true") %>% 
    mutate(total = poor + not_poor,
           povrate = poor / total)
  return(rv)
}

natl_renter_poverty <- get_pov_by_age_dist(renters)

#===============================================================================#
# POVERTY LEVEL
#===============================================================================#

# summarize poverty for people in renter households
natl_renter_pov_by_lvl <- renters %>%
  group_by(pov_level) %>% 
  count(age_cat, wt = PERWT, sort = TRUE) %>% 
  ungroup() %>% 
  group_by(pov_level) %>% 
  mutate(n_pov = sum(n),
         age_share = n / n_pov) 


#===============================================================================#
# INCOME 
#===============================================================================#

# summarize renters by income category
natl_renter_income <- renters %>% 
  count(income_level, wt = PERWT) %>% 
  mutate(income_share = n / sum(n))

#===============================================================================#
# COST BURDEN
#===============================================================================#

natl_cost_by_age <-  renters %>% 
  mutate(cost_burden = renters$RENTGRS / HHINCOME,
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
                                           "Not burdened"))) %>% 
  group_by(age_cat) %>% 
  count(burden_status, wt = PERWT) %>% 
  group_by(burden_status) %>% 
  mutate(n_age = sum(n),
         age_share = n / n_age)


#===============================================================================#
# OCCUPATIONS
#===============================================================================#

occupations <- read.xlsx("data/acs/occupations.xlsx") %>%
  select(-X5, acs_n) %>% 
  mutate(occ_code = str_pad(occ_code, width = 4, side = "left", pad = "0"))

natl_renter_occp <- renters %>% 
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
                                           "Not burdened"))) %>% 
  group_by(OCC) %>% 
  count(pov_level, wt = PERWT) %>% 
  haven::as_factor("both")
  
  
  mutate(n_occ = sum(n),
         age_share = n / n_occ) %>% 
  ungroup() %>% 
  zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 

natl_occp_cost_burden <- natl_renter_occp %>% 
  mutate(
    vulnerable_occ = case_when(
      str_detect(occ_label, "EAT-") ~ "food service",
      str_detect(occ_label, "CLN-") ~ "cleaning",
      str_detect(occ_label, "PRS-") ~ "personal care",
      str_detect(occ_label, "ENT-") ~ "entertainment",
      str_detect(occ_label, "MED-") ~ "medical",
      str_detect(occ_label, "HLS-") ~ "health assistance"
    )) %>% 
  group_by(vulnerable_occ) %>% 
  summarize(n = sum(n),
            n_occ = sum(n_occ),
            acs_n = sum(acs_n))
  
  

#===============================================================================#
# EXPORT
#===============================================================================#

write_to_excel("data/tables/national_ACS_crosstabs.xlsx",
               c("natl_age_table",
                 "natl_cost_by_age",
                 "natl_renter_income",
                 "natl_renter_poverty",
                 "natl_renter_pov_by_lvl"))
