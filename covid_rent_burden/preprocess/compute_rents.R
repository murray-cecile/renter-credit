#===============================================================================#
# COMPUTE BURDENED VS. NOT BURDENED RENTS FOR VULNERABLE
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "haven",
          "matrixStats")
lapply(libs, library, character.only = TRUE)


load("data/acs/prepared_data_2018.Rdata")
load("data/acs/renters_2018.Rdata")

source("covid_rent_burden/preprocess/utils.R")

# PUMA-metro crosswalk
puma_xwalk <- read_csv("data/geo/puma_county_metro_xwalk.csv") 

# create version of crosswalk without county aggregation step
puma_cbsa_only <- puma_xwalk %>%  
  group_by(puma_id, cbsa_code) %>% 
  summarize(cbsa_title = first(cbsa_title),
            afact = sum(afact)) %>% 
  ungroup()

vulnerable_sectors <- c("Non-essential retail",
                        "Food service",
                        "Mining",
                        "Entertainment",
                        "Non-essential manufacturing",
                        "Non-essential travel/transportation",
                        "Other services")


#===============================================================================#
# COMPUTE MEDIAN RENT AND TOTAL RENT FOR US, STATES, AND METROS
#===============================================================================#

# compute median monthly rents and total rent for burdened vs. non-burdened vulnerable
get_rent_by_burden <- function(df, groups = c("")) {
  df %>% 
    mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
    group_by(SERIAL) %>% 
    mutate(vulnerable_ct = sum(is_vulnerable),
           is_vulnerable = if_else(vulnerable_ct > 0, TRUE, FALSE)) %>%
    filter(cost_burdened != "Zero household income") %>%
    distinct_at(vars(one_of("RENTGRS", "HHINCOME", "HHWT",
                            "is_vulnerable", groups))) %>% 
    group_by_at(vars(one_of("is_vulnerable", groups))) %>% 
    summarize(sample_size = n(),
              n_households = sum(HHWT),
              median_rent = weightedMedian(RENTGRS, w = HHWT),
              total_rent = sum(RENTGRS * HHWT), 
              median_hhinc = weightedMedian(HHINCOME, w = HHWT)) %>% 
    ungroup() 
}

# calculate med rent, total rent, med hhinc for all vulnerable
us_rent <- renters %>% 
  get_rent_by_burden() %>% 
  mutate("GEOID" = "00",
         "NAME" = "United States",
         "cost_burdened" = "all vulnerable") # meaning all vulnerable w/ income > 0

# same calculation broken out by burden
us_rent_by_burden <- renters %>% 
  get_rent_by_burden(c("cost_burdened")) %>% 
  mutate("GEOID" = "00",
         "NAME" = "United States")

# combine tables
us_table <- bind_rows(
  us_rent,
  us_rent_by_burden
) %>% 
  select(GEOID, 
         NAME,
         is_vulnerable,
         cost_burdened,
         everything())

# repeat the above exercise but for states
st_rent <- renters %>% 
  get_rent_by_burden(c("STATEFIP")) %>% 
  convert_state_code() %>% 
  mutate(cost_burdened = "all vulnerable")

st_rent_by_burden <- renters %>% 
  get_rent_by_burden(c("STATEFIP", "cost_burdened")) %>% 
  convert_state_code()

st_table <- bind_rows(
  st_rent,
  st_rent_by_burden
) %>% 
  select(GEOID, 
         NAME,
         is_vulnerable,
         cost_burdened,
         everything())



# have to do a custom version for the median rent at metro level
# idea is to randomly select hholds for pumas that are split
# note that totals and sample sizes match other method
allocated_renters <- renters %>% 
  mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
  group_by(SERIAL) %>% 
  mutate(vulnerable_ct = sum(is_vulnerable),
         is_vulnerable = if_else(vulnerable_ct > 0, TRUE, FALSE)) %>%
  filter(cost_burdened != "Zero household income") %>% 
  distinct_at(vars(one_of("puma_id",
                          "RENTGRS",
                          "HHWT",
                          "HHINCOME",
                          "is_vulnerable",
                          "cost_burdened"))) %>% 
  left_join(select(puma_cbsa_only,
                   -cbsa_title),
            by = "puma_id") %>% 
  mutate(allocate = if_else(runif(1) < afact, TRUE, FALSE)) %>%
  filter(allocate) 

metro_rent <- allocated_renters %>% 
  select(-puma_id) %>% 
  group_by(cbsa_code, is_vulnerable) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT),
            median_rent = weightedMedian(RENTGRS, w = HHWT),
            total_rent = sum(RENTGRS * HHWT),
            median_hhinc = weightedMedian(HHINCOME, w = HHWT)) %>% 
  ungroup() %>% 
  convert_cbsa_code() %>% 
  mutate(cost_burdened = "all vulnerable")

metro_rent_by_burden <- allocated_renters %>% 
  select(-puma_id) %>% 
  group_by(cbsa_code, is_vulnerable, cost_burdened) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT),
            median_rent = weightedMedian(RENTGRS, w = HHWT),
            total_rent = sum(RENTGRS * HHWT),
            median_hhinc = weightedMedian(HHINCOME, w = HHWT)) %>% 
  ungroup() %>% 
  convert_cbsa_code()


# make metro table
metro_table <- bind_rows(
  metro_rent,
  metro_rent_by_burden
) %>% 
  select(GEOID, 
         NAME,
         is_vulnerable,
         cost_burdened,
         everything())


# finally assemble into master table 
geo_table <- bind_rows(
  us_table,
  st_table,
  metro_table
) %>% 
  select(GEOID, NAME, everything())


# save(geo_rent_by_burden,
#      file = "covid_rent_burden/data/geo_rent_by_burden.Rdata")

#===============================================================================#
# EXPORT FOR EK
#===============================================================================#

README <- tibble(
  "Sheet Names" = c("us_table",
                  "st_table",
                  "metro_table",
                  "geo_table"),
  "Description" = c("Total and median monthly rent and median household income for households by cost burden and presence of vulnerable worker",
                    "Same as above but cut by state",
                    "Same, but cut by metro",
                    "All three previous tables combined for easy machine reading"),
  "Notes" = c("All figures exclude renter households reporting zero/negative household income",
              "",
              "Allocating median: for households in PUMAs that cross metro borders, I randomly assign a subset to be within the metro w/ probability set at the housing unit allocation factor for that PUMA-metro",
              "")
)

write_to_excel("data/tables/median_total_rent_by_cost_burden.xlsx",
               c("README",
                 "us_table",
                 "st_table",
                 "metro_table",
                 "geo_table"))
