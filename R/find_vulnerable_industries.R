#===============================================================================#
# FIND VULNERABLE OCCUPATIONS
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "janitor",
          "haven")
lapply(libs, library, character.only = TRUE)

source("R/theme.R")

# read in full 1 year file
# data <- read_csv("data/acs/merged_2018_acs.csv")

data <- full_file %>% 
  filter(TYPE == 1)

#===============================================================================#
# INDUSTRY CUTS
#===============================================================================#

sector_ct <- full_file %>% 
  mutate(sector = substr(NAICSP, 1, 2)) %>% 
  group_by(sector) 

#===============================================================================#
# WITH IPUMS
#===============================================================================#

load("data/acs/prepared_data_2018.Rdata")
load("data/acs/renters_2018.Rdata")

# industry recode ugh
data %<>% mutate(
  cost_burdened = case_when(
    (RENTGRS * 12) / HHINCOME > 0.3 ~ "Rent burdened",
    (RENTGRS * 12) / HHINCOME <= 0.3 ~ "Not burdened",
    HHINCOME == 0 ~ "Zero household income"),
  sector = case_when(
    between(IND, 170, 290) ~ "Agriculture, forestry, fishing, hunting",
    between(IND, 370, 490) ~ "Mining, quarrying, oil/gas extraction",
    IND == 770 ~ "Construction",
    between(IND, 1070, 3990) ~ "Manufacturing",
    between(IND, 4070, 4590) ~ "Wholesale trade",
    between(IND, 4670, 5790) ~ "Retail trade",
    between(IND, 6070, 6390) ~ "Transportation and warehousing",
    between(IND, 570, 690) ~ "Utilities",
    between(IND, 6470, 6780) ~ "Information",
    between(IND, 6870, 6992) ~ "Finance and insurance",
    between(IND, 7071, 7190) ~ "Real estate and rental leasing",
    between(IND, 7270, 7490) ~ "Professional, scientific, technical services",
    IND == 7570 ~ "Management of companies and enterprises",
    between(IND, 7580, 7790) ~ "Administrative, support, and waste management services",
    between(IND, 7860, 7890) ~ "Education",
    between(IND, 7970, 8470) ~ "Health care and social assistance",
    between(IND, 8560, 8590) ~ "Arts, entertainment, recreation",
    between(IND, 8660, 8690) ~ "Accommodation and food services",
    between(IND, 8770, 9290) ~ "Other services",
    between(IND, 9370, 9590) ~ "Public administration",
    between(IND, 9670, 9870) ~ "Military",
    IND == 9920 ~ "Unemployed or out of workforce or never worked",
    IND == 0 ~ "NILF"
  )
  )

renters %<>% mutate(
  cost_burdened = case_when(
    (RENTGRS * 12) / HHINCOME > 0.3 ~ "Rent burdened",
    (RENTGRS * 12) / HHINCOME <= 0.3 ~ "Not burdened",
    HHINCOME == 0 ~ "Zero household income"
  ),
  sector = case_when(
    between(IND, 170, 290) ~ "Agriculture, forestry, fishing, hunting",
    between(IND, 370, 490) ~ "Mining, quarrying, oil/gas extraction",
    IND == 770 ~ "Construction",
    between(IND, 1070, 3990) ~ "Manufacturing",
    between(IND, 4070, 4590) ~ "Wholesale trade",
    between(IND, 4670, 5790) ~ "Retail trade",
    between(IND, 6070, 6390) ~ "Transportation and warehousing",
    between(IND, 570, 690) ~ "Utilities",
    between(IND, 6470, 6780) ~ "Information",
    between(IND, 6870, 6992) ~ "Finance and insurance",
    between(IND, 7071, 7190) ~ "Real estate and rental leasing",
    between(IND, 7270, 7490) ~ "Professional, scientific, technical services",
    IND == 7570 ~ "Management of companies and enterprises",
    between(IND, 7580, 7790) ~ "Administrative, support, and waste management services",
    between(IND, 7860, 7890) ~ "Education",
    between(IND, 7970, 8470) ~ "Health care and social assistance",
    between(IND, 8560, 8590) ~ "Arts, entertainment, recreation",
    between(IND, 8660, 8690) ~ "Accommodation and food services",
    between(IND, 8770, 9290) ~ "Other services",
    between(IND, 9370, 9590) ~ "Public administration",
    between(IND, 9670, 9870) ~ "Military",
    IND == 9920 ~ "Unemployed or out of workforce or never worked",
    IND == 0 ~ "NILF"
  )
)

data %>% filter(is.na(sector)) %>% distinct(sector, IND) %>% View()
renters %>% filter(is.na(sector)) %>% distinct(sector, IND) %>% View()

#===============================================================================#
# CROSSTABS
#===============================================================================#

# total worker counts
natl_industry <- full_join(
  data %>% count(sector, wt = PERWT),
  renters %>% count(sector, wt = PERWT),
  by = "sector",
  suffix = c("_all", "_renters")
)

# function to compute # of people, renters, and shares of renters
get_renter_crosstab <- function(df, subdf, groups = c("age_cat")) {
  
  all <- df %>%
    group_by_at(vars(one_of(groups))) %>% 
    summarize(n = sum(PERWT),
              sample_size = n())
  subset <- subdf %>%
    group_by_at(vars(one_of(groups))) %>% 
    summarize(n = sum(PERWT),
              sample_size = n())

  crosstab <- full_join(all,
                         subset,
                         by = groups,
                         suffix = c("_all", "_renters"))
  return(crosstab)
}

natl_industry_cost_burden <- get_renter_crosstab(data,
                                                 renters,
                                                 groups = c("sector",
                                                            "cost_burdened")) %>% 
  group_by(sector) %>% 
  mutate(n_sector_all = sum(n_all),
         n_sector_renter = sum(n_renters),
         burden_share_renters = n_renters / n_sector_renter)

#===============================================================================#
# HOW CLOSE ARE PEOPLE TO BEING BURDENED?
#===============================================================================#

# many distributions
renters %>%
  filter(HHINCOME > 0,
         RENTGRS > 0,
         !sector %in% c("NILF",
                        "Unemployed or out of workforce or never worked")) %>% 
  mutate(rent_burden = (RENTGRS * 12) / HHINCOME) %>%
  ggplot(aes(x = rent_burden,
             color = sector)) +
  scale_x_continuous(limits = c(0, 1)) +
  geom_freqpoly(show.legend = FALSE) +
  facet_wrap(~sector) +
  terner_theme()

# summarize with median and std dev
rent_burden_summary_by_industry <- renters %>%
  filter(HHINCOME > 0,
         RENTGRS > 0,
         !sector %in% c("NILF",
                        "Unemployed or out of workforce or never worked")) %>% 
  mutate(rent_burden = (RENTGRS * 12) / HHINCOME) %>% 
  group_by(sector) %>% 
  summarize(median_rent = median(RENTGRS, wt = PERWT),
            median_hhinc = median(HHINCOME, wt = PERWT),
            median_rent_burden = median(rent_burden, wt = PERWT),
            rent_p25 = quantile(RENTGRS, probs = 0.25, wt = PERWT),
            hhinc_p25 = quantile(HHINCOME, probs = 0.25, wt = PERWT),
            rent_burden_p25 = quantile(rent_burden, probs = 0.25, wt = PERWT),
            rent_p75 = quantile(RENTGRS, probs = 0.75, wt = PERWT),
            hhinc_p75 = quantile(HHINCOME, probs = 0.75, wt = PERWT),
            rent_burden_p75 = quantile(rent_burden, probs = 0.75, wt = PERWT),
            rent_burden_iqr = rent_burden_p75 - rent_burden_p25) 

#===============================================================================#
# EXPORT
#===============================================================================#

README <- tibble(
  "Sheet Name" = c("natl_industry",
                   "natl_industry_cost_burden",
                   "rent_burden_summary_by_industry"),
  "Description" = c("# of individuals and # renters by sector",
                    "# of individuals and # renters by household cost burden by sector", 
                    "Median, 25th, and 75th percentile rents, incomes, and cost burdens by sector"),
  "Notes" = c("", 
              "burden_share_renters = # renters in cost burden category in sector / # renters in sector",
              "All figures weighted using PERSON-level weights")
)

source("R/utils.R")

write_to_excel("data/tables/national_sector_crosstabs.xlsx",
               c("README",
                 "natl_industry",
                 "natl_industry_cost_burden",
                 "rent_burden_summary_by_industry"))
