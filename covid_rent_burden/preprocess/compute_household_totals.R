#===============================================================================#
# COMPUTE TOTAL HOUSEHOLDS
#
# Cecile Murray
#===============================================================================#

# packages
libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "haven")
lapply(libs, library, character.only = TRUE)

# data
load("data/acs/prepared_data_2018.Rdata")
load("data/acs/renters_2018.Rdata")
puma_xwalk <- read_csv("data/geo/puma_county_metro_xwalk.csv")

# functions
source("covid_rent_burden/preprocess/utils.R")

# define vulnerable sectors
vulnerable_sectors <- c("Non-essential retail",
                        "Food service",
                        "Mining",
                        "Entertainment",
                        "Non-essential manufacturing",
                        "Non-essential travel/transportation",
                        "Other services")


#===============================================================================#
# COMPUTE TOTAL # OF HOUSEHOLDS BY GEOGRAPHY
#===============================================================================#

us_hholds <- data %>% 
  distinct(SERIAL,
           HHWT) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  mutate(GEOID = "00",
         NAME = "United States")

st_hholds <- data %>% 
  distinct(SERIAL,
           HHWT,
           STATEFIP) %>% 
  group_by(STATEFIP) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  convert_state_code()

metro_hholds <- data %>% 
  distinct(SERIAL,
           HHWT,
           puma_id) %>% 
  group_by(puma_id) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  allocate_puma_to_metro() %>% 
  convert_cbsa_code()

hhold_table <- bind_rows(
  us_hholds,
  st_hholds,
  metro_hholds
)

# write_excel_csv(hhold_table, path = "data/tables/household_counts.csv")

#===============================================================================#
# COMPUTE TOTAL # OF RENTER HOUSEHOLDS BY GEOGRAPHY
#===============================================================================#

us_renters <- renters %>% 
  distinct(SERIAL, HHWT) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  mutate(GEOID = "00",
         NAME = "United States")

st_renters <- renters %>% 
  distinct(SERIAL, HHWT, STATEFIP) %>% 
  group_by(STATEFIP) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  convert_state_code()

metro_renters <- renters %>% 
  distinct(SERIAL, HHWT, puma_id) %>% 
  group_by(puma_id) %>% 
  summarize(sample_size = n(),
            n_households = sum(HHWT)) %>% 
  allocate_puma_to_metro() %>% 
  convert_cbsa_code()

renters_table <- bind_rows(
  us_renters,
  st_renters,
  metro_renters
)

# write_excel_csv(renters_table, "data/tables/total_renter_households.csv")

#===============================================================================#
# TAG VULNERABLE HOUSEHOLDS
#===============================================================================#

# function to count # vulnerable per household
count_vulnerable_workers_by_hh <- function(df) {
  df %>% 
    select(SERIAL,
           HHWT,
           sector) %>% 
    mutate(
      is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)
    ) %>% 
    group_by(SERIAL) %>% 
    mutate(
      vulnerable_ct = sum(is_vulnerable, na.rm = TRUE),
      is_vulnerable = if_else(vulnerable_ct > 0, 1, 0)
    ) %>% 
    ungroup() %>% 
    distinct(SERIAL, 
             HHWT, 
             vulnerable_ct,
             is_vulnerable)
}


# these are dfs of households (all, renter) with counts of workers in vulnerable
# sectors

vulnerable_hholds <- data %>% 
  count_vulnerable_workers_by_hh() 

vulnerable_renters <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  count_vulnerable_workers_by_hh()


# TOTAL VULNERABLE COUNTS  =============================#

tally_vulnerable_hh <- function(df, groups = c("")) {
  df %>% 
    filter(is_vulnerable == 1) %>% 
    distinct_at(vars(one_of(c("SERIAL",
                              "HHWT",
                              groups)))) %>% 
    group_by_at(vars(one_of(groups))) %>% 
    summarize(
      sample_size = n(),
      n_households = sum(HHWT)
    )
}

us_vulnerable <- vulnerable_renters %>% 
  tally_vulnerable_hh() %>% 
  mutate(GEOID = "00",
         NAME = "United States")

st_vulnerable <- vulnerable_renters %>% 
  left_join(
    distinct(renters, SERIAL, STATEFIP),
    by = c("SERIAL")
  ) %>%
  tally_vulnerable_hh(c("STATEFIP")) %>% 
  convert_state_code()

metro_vulnerable <- vulnerable_renters %>% 
  left_join(
    select(renters, SERIAL, puma_id),
    by = c("SERIAL")
  ) %>% 
  tally_vulnerable_hh(groups = c("puma_id")) %>% 
  allocate_puma_to_metro() %>% 
  convert_cbsa_code()

geo_vulnerable <- bind_rows(
  us_vulnerable,
  st_vulnerable,
  metro_vulnerable
) %>% 
  select(NAME, 
         GEOID,
         everything())

#===============================================================================#
# TALLY RENTER HOUSEHOLDS BY # OF AFFECTED WORKERS
#===============================================================================#

# count households by # affected workers by geography 
tally_hh_by_num_vulnerable <- function(df, groups = c("")) {
  df %>% 
    distinct() %>% 
    group_by_at(vars(one_of(c("vulnerable_ct", groups)))) %>% 
    summarize(
      sample_size = n(),
      n_households = sum(HHWT)
    )
}

us_hh_by_num_vulnerable <- vulnerable_renters %>% 
  tally_hh_by_num_vulnerable() %>% 
  mutate(GEOID = "00",
         NAME = "United States")

st_hh_by_num_vulnerable <- vulnerable_renters %>% 
  left_join(
    select(renters, SERIAL, STATEFIP),
    by = c("SERIAL")
  ) %>% 
  tally_hh_by_num_vulnerable(groups = c("STATEFIP")) %>% 
  convert_state_code()

metro_hh_by_num_vulnerable <- vulnerable_renters %>% 
  left_join(
    select(renters, SERIAL, puma_id),
    by = c("SERIAL")
  ) %>% 
  tally_hh_by_num_vulnerable(groups = c("puma_id")) %>% 
  allocate_puma_to_metro(groups = c("vulnerable_ct")) %>% 
  convert_cbsa_code()

geo_hh_by_num_vulnerable <- bind_rows(
  us_hh_by_num_vulnerable,
  st_hh_by_num_vulnerable,
  metro_hh_by_num_vulnerable
) %>% 
  select(GEOID,
         NAME,
         vulnerable_ct,
         sample_size,
         n_households) %>% 
  dplyr::rename("n_renter_households" = "n_households")

# write_csv(geo_hh_by_num_vulnerable, 
#           path = "data/tables/renter_households_by_num_vulnerable.csv")

#===============================================================================#
# TALLY # OF HOUSEHOLDS WHERE ALL WORKERS ARE VULNERABLE
#===============================================================================#

# function to flag households w/workers where all are in vulnerable sectors
flag_fully_vulnerable <- function(df) {
  df %>% 
    mutate(
      is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0),
      is_worker = if_else(sector != "NILF/unemployed", 1, 0)
    ) %>% 
    group_by(SERIAL) %>% 
    mutate(
      vulnerable_ct = sum(is_vulnerable),
      worker_ct = sum(is_worker),
      all_vulnerable = if_else(vulnerable_ct == worker_ct & 
                                 worker_ct > 0, 1, 0)
    ) %>% 
    ungroup()
}

# flag them and keep them to use 
all_vulnerable_hh <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  flag_fully_vulnerable()

# function to count these households by arbitrary grouping
tally_fully_vulnerable <- function(df, groups = c("")) {
  df %>% 
    filter(all_vulnerable == 1) %>% 
    distinct_at(vars(one_of(c("SERIAL",
                         "HHWT",
                         groups)))) %>% 
    group_by_at(vars(one_of(groups))) %>% 
    summarize(
      sample_size = n(),
      n_households = sum(HHWT)
    )
}

us_all_vulnerable <- all_vulnerable_hh %>% 
  tally_fully_vulnerable() %>% 
  mutate(GEOID = "00",
         NAME = "United States") 

st_all_vulnerable <- all_vulnerable_hh %>% 
  tally_fully_vulnerable(c("STATEFIP")) %>% 
  convert_state_code()

metro_all_vulnerable <- all_vulnerable_hh %>% 
  tally_fully_vulnerable(c("puma_id")) %>% 
  allocate_puma_to_metro() %>% 
  convert_cbsa_code()

geo_all_vulnerable <- bind_rows(
  us_all_vulnerable,
  st_all_vulnerable,
  metro_all_vulnerable
) %>% 
  select(NAME, 
         GEOID,
         everything())


# write_csv(geo_all_vulnerable,
#           path = "data/tables/renter_households_by_all_vulnerable.csv")


# CONSTRUCT SHARE TABLE  =============================#

geo_all_vulnerable_shares <- full_join(
  geo_vulnerable,
  geo_all_vulnerable,
  by = c("GEOID", "NAME"),
  suffix = c("_any_vulnerable",
             "_all_vulnerable")
  ) %>% 
  full_join(
    renters_table,
    by = c("GEOID", "NAME")
  )  %>% 
  dplyr::rename(
    "renter_sample_size" = "sample_size",
    "renter_households" = "n_households",
  ) %>% 
  mutate(
    share_of_affected = n_households_all_vulnerable / n_households_any_vulnerable,
    share_of_renters = n_households_any_vulnerable / renter_households,
  ) %>% 
  filter(str_length(GEOID) < 5 | 
           GEOID %in% puma_xwalk$cbsa_code[puma_xwalk$is_metro == TRUE])

# write_csv(geo_all_vulnerable_shares,
#           path = "data/tables/renter_households_only_affected_shares.csv")

us_benchmark <- filter(geo_all_vulnerable_shares, GEOID == "00")

geo_all_vulnerable_shares %>% 
  mutate(
    geo_type = case_when(
    GEOID == "00" ~ "US",
    str_length(GEOID) == 2 ~ "State",
    str_length(GEOID) == 5 ~ "Metro"
    )
  ) %>%
  filter(geo_type != "US") %>% 
  ggplot(aes(x = share_of_renters)) +
  geom_histogram() +
  geom_vline(xintercept = us_benchmark$share_of_renters) +
  facet_grid(rows = vars(geo_type)) 

geo_all_vulnerable_shares %>% 
  mutate(
    geo_type = case_when(
      GEOID == "00" ~ "US",
      str_length(GEOID) == 2 ~ "States",
      str_length(GEOID) == 5 ~ "Metros"
    )
  ) %>%
  filter(geo_type != "US") %>% 
  ggplot(aes(x = renter_households,
             y = share_of_renters)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_log10(labels = scales::comma) +
  facet_wrap(~geo_type) +
  labs(title = "Renter households vs. share of renter households where all workers likely affected",
       x = "# of renter households",
       y = "# households where all workers impacted / renter households")

ggsave("docs/affected_shares_sample_size_plot.png")
