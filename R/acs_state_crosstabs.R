#===============================================================================#
# ACS STATE CROSSTABS
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
get_renter_age_dist <- function(df, subdf, groups = c("age_cat")) {
  
  all <- df %>%
    group_by_at(vars(one_of(groups))) %>% 
    summarize(n = sum(PERWT),
              sample_size = n())
  subset <- subdf %>%
    group_by_at(vars(one_of(groups))) %>% 
    summarize(n = sum(PERWT),
              sample_size = n())
  print(head(all))
  
  age_table <- full_join(all,
                         subset,
                         by = groups,
                         suffix = c("_all", "_renters")) %>%
    mutate(share_renting = n_renters / n_all,
           share_of_renters = n_renters / sum(n_renters),
           cumulative_share_of_renters = cumsum(share_of_renters))
  return(age_table)
}

st_age_table <- get_renter_age_dist(data, renters, c("age_cat", "STATEFIP"))

#===============================================================================#
# POVERTY
#===============================================================================#

# summarize poverty for people in renter households
get_pov_by_age_dist <- function(df, groups = c("age_cat", "poor")) {
  rv <- df %>% 
    group_by_at(vars(one_of(groups))) %>% 
    summarize(n = sum(PERWT),
              sample_size = n()) %>% 
    ungroup() %>% 
    mutate(poor = if_else(poor, "poor", "not_poor")) %>% 
     pivot_wider(names_from = c("poor"), 
                 values_from = c("n", "sample_size")) %>%
    mutate(total = n_poor + n_not_poor,
            povrate = n_poor / total)
  return(rv)
}

st_renter_poverty <- get_pov_by_age_dist(renters,
                                         c("age_cat", "poor", "STATEFIP"))

#===============================================================================#
# POVERTY LEVEL
#===============================================================================#

# summarize poverty for people in renter households
st_renter_pov_by_lvl <- renters %>%
  group_by(pov_level, STATEFIP, age_cat) %>% 
  summarize(n_agepov =sum(PERWT),
            sample_size = n()) %>% 
  ungroup() %>% 
  group_by(pov_level, STATEFIP) %>% 
  mutate(n_pov = sum(n_agepov),
         age_share = n_agepov / n_pov) 

#===============================================================================#
# INCOME 
#===============================================================================#

# summarize renters by income category
st_renter_income <- renters %>% 
  group_by(STATEFIP, income_level) %>% 
  summarize(n = sum(PERWT),
            sample_size = n()) %>% 
  mutate(income_share = n / sum(n))

#===============================================================================#
# COST BURDEN
#===============================================================================#

st_cost_by_age <-  renters %>% 
  mutate(cost_burden = (renters$RENTGRS * 12) / HHINCOME,
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
  group_by(age_cat, STATEFIP, burden_status) %>% 
  summarize(n_age_burden = sum(PERWT),
            sample_size = n()) %>% 
  group_by(burden_status) %>% 
  mutate(n_age = sum(n_age_burden),
         age_share = n_age_burden / n_age)



#===============================================================================#
# OCCUPATIONS
#===============================================================================#

occupations <- read.xlsx("data/acs/occupations.xlsx") %>%
  select(-X5,
         -acs_n,
         -is_vulnerable) %>% 
  mutate(occ_code = str_pad(occ_code, width = 4, side = "left", pad = "0"))

state_occp <- data %>% 
  group_by(STATEFIP, OCC) %>% 
  summarize(n_occ = sum(PERWT),
            sample_size = n()) %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 

# compute cost-burdened share
st_cost_burden_by_occp <- renters %>%
  mutate(cost_burden = (RENT*12) / HHINCOME,
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
  group_by(STATEFIP, OCC, burden_status) %>% 
  summarize(n_st_occ_burden = sum(PERWT),
            sample_size = n()) %>% 
  group_by(STATEFIP, OCC) %>% 
  mutate(n_occ = sum(n_st_occ_burden),
         share_of_occ = n_st_occ_burden / n_occ) %>% 
  ungroup() %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 


# Try median rent
st_median_rent_by_occp <- renters %>% 
  group_by(STATEFIP, OCC) %>% 
  summarize(median_rent = median(RENTGRS, wt = PERWT),
            median_pearnings = median(INCEARN, wt = PERWT),
            sample_size = n()) %>% 
  ungroup() %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 


# try poverty level
st_pov_status_by_occp <- renters %>% 
  group_by(STATEFIP, OCC, pov_level) %>% 
  summarize(n_people = sum(wt = PERWT),
            sample_size = n()) %>% 
  ungroup() %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 


#===============================================================================#
# EXPORT
#===============================================================================#


README <- tibble(
  "Sheet Name" = c("st_age_table",
                   "st_cost_by_age",
                   "st_renter_income",
                   "st_renter_poverty",
                   "st_renter_pov_by_lvl"),
  "Description" = c("# of individuals and # renters by age group by state",
                    "# of renters by income category by state", 
                    "# of renters by age and poverty status by state",
                    "# of renters by age group and cost burden by state",
                    "# of renters by age and % of poverty by state"),
  "Notes" = c("", "", "" ,
              "Cost burden = gross rent * 12 / household income",
              "n_pov is the # of people in a given poverty category across ages")
)

write_to_excel("data/tables/state_ACS_crosstabs.xlsx",
               c("README",
                 "st_age_table",
                 "st_cost_by_age",
                 "st_renter_income",
                 "st_renter_poverty",
                 "st_renter_pov_by_lvl"))


README <- tibble(
  "Sheet Name" = c("state_occp",
                   "st_cost_burden_by_occp",
                   "st_median_rent_by_occp",
                   "st_pov_status_by_occp"),
  "Description" = c("# of individuals by occupation by state",
                    "# of renters by occupation and household cost burden by state",
                    "Median household rent and personal earnings by occupation by state among renters",
                    "# of renters by occupation by % of poverty by state"),
  "Notes" = c("Universe for this is all people, not just renters",
              "Cost burden = gross rent * 12 / household income",
              "",
              "Note this is household rent but personal earned income")
)

write_to_excel("data/tables/state_occupations.xlsx",
               c("README",
                 "state_occp",
                 "st_cost_burden_by_occp",
                 "st_median_rent_by_occp",
                 "st_pov_status_by_occp"))
