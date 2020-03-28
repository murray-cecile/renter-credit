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
    left_join(distinct(select(puma_county_metro,
                     cbsa_code,
                     cbsa_title,
                     is_metro)),
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

#===============================================================================#
# OCCUPATIONS
#===============================================================================#

occupations <- read.xlsx("data/acs/occupations.xlsx") %>%
  select(-X5,
         -acs_n,
         -is_vulnerable) %>% 
  mutate(occ_code = str_pad(occ_code, width = 4, side = "left", pad = "0"))

metro_occp <- data %>% 
  group_by(puma_id, OCC) %>% 
  summarize(n_occ = sum(PERWT),
            sample_size = n()) %>% 
  allocate_puma_to_metro(groups = c("OCC")) %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 

# compute cost-burdened share
metro_cost_burden_by_occp <- renters %>%
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
  group_by(puma_id, OCC, burden_status) %>% 
  summarize(n_metro_occ_burden = sum(PERWT),
            sample_size = n()) %>% 
  allocate_puma_to_metro(groups = c("OCC", "burden_status")) %>% 
  group_by(cbsa_code, OCC) %>% 
  mutate(n_occ = sum(n_metro_occ_burden),
         share_of_occ = n_metro_occ_burden / n_occ) %>% 
  ungroup() %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 


# Try median rent
metro_median_rent_by_occp <- renters %>% 
  group_by(puma_id, OCC) %>% 
  summarize(median_rent = median(RENTGRS, wt = PERWT),
            median_pearnings = median(INCEARN, wt = PERWT),
            sample_size = n()) %>% 
  ungroup() %>% 
  allocate_puma_to_metro(groups = c("OCC")) %>% 
  ungroup() %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 


# try poverty level
metro_pov_status_by_occp <- renters %>% 
  group_by(puma_id, OCC, pov_level) %>% 
  summarize(n_people = sum(wt = PERWT),
            sample_size = n()) %>% 
  ungroup() %>% 
  allocate_puma_to_metro(groups = c("OCC", "pov_level")) %>% 
  ungroup() %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 

#===============================================================================#
# EXPORT
#===============================================================================#


README <- tibble(
  "Sheet Name" = c("metro_age_table",
                   "metro_renter_income",
                   "metro_renter_poverty",
                   "metro_renter_pov_by_lvl"),
  "Description" = c("# of individuals and # renters by age group by MSA",
                    "# of renters by age and poverty status by MSA",
                    "# of renters by age group and cost burden by MSA",
                    "# of renters by age and % of poverty by MSA"),
  "Notes" = c("is_metro is a flag for metropolitan vs. micropolitan area",
              "" ,
              "Cost burden = gross rent * 12 / household income",
              "n_pov is the # of people in a given poverty category across ages")
)

write_to_excel("data/tables/MSA_ACS_crosstabs.xlsx",
               c("README",
                 "metro_age_table",
                 "metro_renter_income",
                 "metro_renter_poverty",
                 "metro_renter_pov_by_lvl"))


README <- tibble(
  "Sheet Name" = c("metro_occp",
                   "metro_cost_burden_by_occp",
                   # "metro_median_rent_by_occp",
                   "metro_pov_status_by_occp"),
  "Description" = c("# of individuals by occupation by MSA",
                    "# of renters by occupation and household cost burden by MSA",
                    # "Median household rent and personal earnings by occupation by MSA among renters",
                    "# of renters by occupation by % of poverty by MSA"),
  "Notes" = c("Universe for this is all people, not just renters, and is_metro is a flag for metropolitan vs. micropolitan area",
              "Cost burden = gross rent * 12 / household income",
              # "",
              "Note this is household rent but personal earned income")
)

write_to_excel("data/tables/metro_occupations.xlsx",
               c("README",
                 "metro_occp",
                 "metro_cost_burden_by_occp",
                 # "metro_median_rent_by_occp",
                 "metro_pov_status_by_occp"))
