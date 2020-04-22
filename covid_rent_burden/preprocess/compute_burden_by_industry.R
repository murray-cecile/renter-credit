#===============================================================================#
# COMPUTE VULNERABLE HOUSEHOLDS BY SECTOR
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "haven")
lapply(libs, library, character.only = TRUE)

load("data/acs/prepared_data_2018.Rdata")
load("data/acs/renters_2018.Rdata")

# PUMA-metro crosswalk
puma_xwalk <- read_csv("data/geo/puma_county_metro_xwalk.csv")

#===============================================================================#
# CHART 1: # BURDENED WORKERS IN SECTOR / # BURDENED WORKERS
#===============================================================================#


# function to compute # of people by group(s)
get_worker_crosstab <- function(df, groups = c("")) {
  df %>%
    filter(sector != "NILF/unemployed") %>% 
    group_by_at(vars(one_of(groups))) %>% 
    summarize(n = sum(PERWT),
              sample_size = n()) 
}

assemble_table <- function(industry_df, burden_df, burden_by_industry_df,
                          geos = c(""), groups = c("")) {
  full_join(
    industry_df,
    burden_df,
    by = geos
    ) %>% 
    full_join(
      burden_by_industry_df,
      by = groups
    ) %>% 
    mutate(sector_burden_share = n_sector_burdened / n_burdened,
           sector_share = n_sector / total_workers)
}


# US TOTAL  =============================#

# total worker counts for US
us_industry <- data %>% 
  get_worker_crosstab(c("sector")) %>% 
  ungroup() %>% 
  dplyr::rename("n_sector" = "n",
                "sample_size_sector" = "sample_size") %>% 
  mutate(total_workers = sum(n_sector),
         GEOID = "00") 

# total burden counts, excluding zero-income hholds
us_burden <- renters %>% 
  # filter(cost_burdened != "Zero household income") %>%
  get_worker_crosstab(c("cost_burdened")) %>% 
  ungroup() %>% 
  dplyr::rename("n_burdened" = "n",
                "sample_size_burdened" = "sample_size") %>% 
  mutate(GEOID = "00")

# burdened workers by industry, excluding zero-income hholds
us_burden_by_industry <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_worker_crosstab(c("sector", "cost_burdened")) %>% 
  ungroup() %>% 
  dplyr::rename("n_sector_burdened" = "n",
                "sample_size_sector_burdened" = "sample_size") %>% 
  mutate(GEOID = "00")

# assemble full table
us_table <- assemble_table(us_industry,
                           us_burden,
                           us_burden_by_industry,
                           geos = "GEOID",
                           groups = c("GEOID", "sector", "cost_burdened"))

# STATE  =============================#

# total worker counts by state
st_industry <- data %>% 
  get_worker_crosstab(c("STATEFIP", "sector")) %>% 
  ungroup() %>% 
  dplyr::rename("n_sector" = "n",
                "sample_size_sector" = "sample_size") %>% 
  group_by(STATEFIP) %>% 
  mutate(total_workers = sum(n_sector)) 

# total burden counts by state, excluding zero-income hholds
st_burden <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_worker_crosstab(c("STATEFIP", "cost_burdened")) %>% 
  ungroup() %>% 
  dplyr::rename("n_burdened" = "n",
                "sample_size_burdened" = "sample_size")

# burdened workers by industry by state
st_burden_by_industry <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_worker_crosstab(c("STATEFIP", "sector", "cost_burdened")) %>% 
  ungroup() %>% 
  dplyr::rename("n_sector_burdened" = "n",
                "sample_size_sector_burdened" = "sample_size")

# assemble full table and compute shares
st_table <- assemble_table(st_industry,
                           st_burden,
                           st_burden_by_industry,
                           geos = "STATEFIP",
                           groups = c("STATEFIP", "sector", "cost_burdened")) %>% 
  left_join(distinct(fips_codes,
                   state_code,
                   state_name),
            by = c("STATEFIP" = "state_code"))


# save(st_table, file = "st_burden_by_sector.Rdata")

# METRO LEVEL  =============================#

# function to allocate PUMAs to metros
allocate_puma_to_metro <- function(df, groups = c("")) {
  df %>% 
    ungroup() %>% 
    left_join(select(puma_xwalk,
                          puma_id,
                          stcofips,
                          afact),
                   by = "puma_id") %>% 
    group_by_at(vars(one_of(c("stcofips", groups)))) %>% 
    summarize_at(vars(contains("n_"), 
                      contains("sample_size")),
                 ~ sum(. * afact)) %>% 
    left_join(distinct(puma_xwalk,
                     stcofips,
                     cbsa_code),
              by = "stcofips") %>% 
    ungroup() %>% 
    select(-stcofips) %>% 
    group_by_at(vars(one_of(c("cbsa_code", groups)))) %>% 
    summarize_all(~round(sum(.))) 
}


# total worker counts by metro
metro_industry <- data %>% 
  mutate(PUMA = str_pad(PUMA, width = 5, side = "left", pad = "0"),
         puma_id = paste0(STATEFIP, PUMA)) %>% 
  get_worker_crosstab(c("puma_id", "sector")) %>% 
  ungroup() %>% 
  dplyr::rename("n_sector" = "n",
                "sample_size_sector" = "sample_size") %>% 
  allocate_puma_to_metro(c("sector")) %>% 
  group_by(cbsa_code) %>% 
  mutate(total_workers = sum(n_sector)) 

# total burden counts by metro, excluding zero-income hholds
metro_burden <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  mutate(PUMA = str_pad(PUMA, width = 5, side = "left", pad = "0"),
         puma_id = paste0(STATEFIP, PUMA)) %>% 
  get_worker_crosstab(c("puma_id", "cost_burdened")) %>% 
  ungroup() %>% 
  dplyr::rename("n_burdened" = "n",
                "sample_size_burdened" = "sample_size") %>% 
  allocate_puma_to_metro(c("cost_burdened"))

# burdened workers by industry by metro
metro_burden_by_industry <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  mutate(PUMA = str_pad(PUMA, width = 5, side = "left", pad = "0"),
         puma_id = paste0(STATEFIP, PUMA)) %>%
  get_worker_crosstab(c("puma_id", "sector", "cost_burdened")) %>% 
  ungroup() %>% 
  dplyr::rename("n_sector_burdened" = "n",
                "sample_size_sector_burdened" = "sample_size") %>% 
  allocate_puma_to_metro(c("cost_burdened", "sector"))

# assemble full table and compute shares
metro_table <- assemble_table(metro_industry,
                              metro_burden,
                              metro_burden_by_industry,
                              geos = "cbsa_code",
                              groups = c("cbsa_code", "sector", "cost_burdened")) %>% 
  left_join(distinct(puma_xwalk,
                     cbsa_code,
                     cbsa_title),
            by = "cbsa_code") %>% 
  ungroup() %>% 
  mutate(cbsa_code = as.character(cbsa_code))


#===============================================================================#
# STITCH GEOGRAPHIC LEVELS TOGETHER
#===============================================================================#

industry_burden_table <- bind_rows(
  mutate(us_table, NAME = "United States"),
  dplyr::rename(st_table,
                "GEOID" = "STATEFIP",
                "NAME" = "state_name"),
  dplyr::rename(metro_table,
                "GEOID" = "cbsa_code",
                "NAME" = "cbsa_title")) %>% 
  select(GEOID,
         NAME,
         sector, 
         everything())

save(industry_burden_table,
     file = "covid_rent_burden/data/industry_burden_table.Rdata")

#===============================================================================#
# EXPORT FOR HUMAN ANALYSIS
#===============================================================================#

source("R/utils.R")

README <- tibble(
  "Sheet Name" = c("us_table",
                   "st_table",
                   "metro_table",
                   "industry_burden_table"),
  "Description" = c("Data for figures 1 and 2 for US",
                    "Data for figures 1 and 2 for states",
                    "Data for figures 1 and 2 for CBSAs",
                    "Previous three tables combined into one for machine readability"),
  "Notes" = c("chart 1 variable is sector_burden_share = n_sector_burdened / n_burdened",
              "chart 2 variable is n_burdened for rent burdened vs not burdened",
              "",
              "")
)

write_to_excel("data/tables/burdened_workers_by_industry.xlsx",
               c("README",
                 "us_table",
                 "st_table",
                 "metro_table",
                 "industry_burden_table"))

