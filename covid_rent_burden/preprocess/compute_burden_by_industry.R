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

join_crosstab <- function(df1, df2, groups = c(""), labs = c("_all", "_renters")) {
  full_join(
    get_worker_crosstab(df1, groups),
    get_worker_crosstab(df2, groups),
    by = groups,
    suffix = labs
  ) 
}

# total worker counts by state
st_industry <- get_worker_crosstab(data, c("STATEFIP", "sector")) %>% 
  ungroup() %>% 
  mutate(STATEFIP = str_pad(STATEFIP, width = 2, side = "left", pad = "0")) %>% 
  dplyr::rename("n_sector" = "n",
                "sample_size_sector" = "sample_size") %>% 
  group_by(STATEFIP) %>% 
  mutate(total_workers = sum(n_sector)) 

# total burden counts by state, excluding zero-income hholds
st_burden <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_worker_crosstab(c("STATEFIP", "cost_burdened")) %>% 
  ungroup() %>% 
  mutate(STATEFIP = str_pad(STATEFIP, width = 2, side = "left", pad = "0")) %>% 
  dplyr::rename("n_burdened" = "n",
                "sample_size_burdened" = "sample_size")

# burdened workers by industry by state
st_burden_by_industry <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_worker_crosstab(c("STATEFIP", "sector", "cost_burdened")) %>% 
  ungroup() %>% 
  mutate(STATEFIP = str_pad(STATEFIP, width = 2, side = "left", pad = "0")) %>% 
  dplyr::rename("n_sector_burdened" = "n",
                "sample_size_sector_burdened" = "sample_size")

# assemble full table and compute shares
st_table <- full_join(st_industry,
          st_burden,
          by = c("STATEFIP")) %>% 
  full_join(st_burden_by_industry,
            by = c("STATEFIP", "sector", "cost_burdened")) %>% 
  mutate(sector_burden_share = n_sector_burdened / n_burdened,
         sector_share = n_sector / total_workers)

save(st_table, file = "st_burden_by_sector.Rdata")
