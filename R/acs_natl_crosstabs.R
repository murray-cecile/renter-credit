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
  group_by(age_cat) %>% 
  count(burden_status, wt = PERWT) %>% 
  group_by(burden_status) %>% 
  mutate(n_age = sum(n),
         age_share = n / n_age)

natl_cost_by_age %>% 
  ggplot(aes(x = burden_status,
             fill = age_cat)) +
  geom_col(aes(y = age_share)) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(title = "Cost-burdened renters skew younger",
       subtitle = "Share of individuals of each age by gross rent as a share of household income, 2018",
       x = "",
       y = "% of people in renter households",
       fill = "Age group") +
  terner_theme()

#===============================================================================#
# OCCUPATIONS
#===============================================================================#

occupations <- read.xlsx("data/acs/occupations.xlsx") %>%
  select(-X5,
         -acs_n,
         -is_vulnerable) %>% 
  mutate(occ_code = str_pad(occ_code, width = 4, side = "left", pad = "0"))

natl_occp <- data %>% 
  count(OCC, wt = PERWT) %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 

# compute cost-burdened share
natl_cost_burden_by_occp <- renters %>%
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
  group_by(OCC) %>% 
  count(burden_status, wt = PERWT) %>%
  group_by(OCC) %>% 
  mutate(n_occ = sum(n),
         share_of_occ = n / n_occ) %>% 
  ungroup() %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 



# Try median rent
natl_median_rent_by_occp <- renters %>% 
  group_by(OCC) %>% 
  summarize(median_rent = median(RENTGRS, wt = PERWT),
            median_pearnings = median(INCEARN, wt = PERWT),
            sample_size = n()) %>% 
  ungroup() %>% 
  haven::zap_labels() %>% 
  mutate(OCC = str_pad(OCC, width = 4, side = "left", pad = "0")) %>% 
  left_join(occupations,
            by = c("OCC" = "occ_code")) 


# try poverty level
natl_pov_status_by_occp <- renters %>% 
  group_by(OCC, pov_level) %>% 
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
  "Sheet Name" = c("natl_age_table",
                   "natl_cost_by_age",
                   "natl_renter_income",
                   "natl_renter_poverty",
                   "natl_renter_pov_by_lvl"),
  "Description" = c("# of individuals and # renters by age group",
                    "# of renters by income category", 
                    "# of renters by age and poverty status",
                    "# of renters by age group and cost burden",
                    "# of renters by age and % of poverty"),
  "Notes" = c("", "", "" ,
              "Cost burden = gross rent * 12 / household income",
              "n_pov is the # of people in a given poverty category across ages")
)

write_to_excel("data/tables/national_ACS_crosstabs.xlsx",
               c("README",
                 "natl_age_table",
                 "natl_renter_income",
                 "natl_renter_poverty",
                 "natl_cost_by_age",
                 "natl_renter_pov_by_lvl"))


README <- tibble(
  "Sheet Name" = c("natl_occp",
                   "natl_cost_burden_by_occp",
                   "natl_median_rent_by_occ",
                   "natl_pov_status_by_occp"),
  "Description" = c("# of individuals by occupation",
                    "# of renters by occupation and household cost burden",
                    "Median household rent and personal earnings by occupation among renters",
                    "# of renters by occupation by % of poverty"),
  "Notes" = c("Universe for this is all people, not just renters",
              "Cost burden = gross rent * 12 / household income",
              "",
              "Note this is household rent but personal earned income")
)

write_to_excel("data/tables/natl_occupations.xlsx",
               c("README",
                 "natl_occp",
                 "natl_cost_burden_by_occp",
                 "natl_median_rent_by_occ",
                 "natl_pov_status_by_occp"))

