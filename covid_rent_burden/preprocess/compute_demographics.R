#===============================================================================#
# COMPUTE AGE AND RACE DEMOGRAPHICS FOR VULNERABLE
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

vulnerable_sectors <- c("Non-essential retail",
                        "Food service",
                        "Mining",
                        "Entertainment",
                        "Non-essential manufacturing",
                        "Non-essential travel/transportation",
                        "Other services")


#===============================================================================#
# TAG VULNERABLE HOUSEHOLDS
#===============================================================================#

vulnerable_hholds <- data %>% 
  select(SERIAL, PERNUM, sector) %>% 
  mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
  group_by(SERIAL) %>% 
  summarize(vulnerable_ct = sum(is_vulnerable, na.rm = TRUE)) %>% 
  filter(vulnerable_ct > 0) %>% 
  distinct(SERIAL) %>% 
  mutate(is_vulnerable = 1)

vulnerable_renters <- renters %>% 
  select(SERIAL, PERNUM, sector) %>% 
  mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
  group_by(SERIAL) %>% 
  summarize(vulnerable_ct = sum(is_vulnerable, na.rm = TRUE)) %>% 
  mutate(is_vulnerable = if_else(vulnerable_ct > 0, 1, 0)) %>% 
  distinct(SERIAL, is_vulnerable) 
  
#===============================================================================#
# # OF BURDENED AND NEWLY VULNERABLE HHOLDS / # RENTER HOUSEHOLDS
#===============================================================================#

st_vulnerable_shares <- renters %>% 
  select(SERIAL, HHWT, STATEFIP, sector, cost_burdened) %>% 
  left_join(vulnerable_renters,
            by = "SERIAL") %>%  
  distinct(SERIAL, HHWT, STATEFIP, cost_burdened, is_vulnerable) %>% 
  group_by(STATEFIP, cost_burdened, is_vulnerable) %>% 
  summarize(sample_size = n(),
            n_burden_vulnerable = sum(is_vulnerable, wt = HHWT)) %>% 
  ungroup() %>% 
  group_by(STATEFIP) %>% 
  mutate(n_households = sum(n_burden_vulnerable),
         share_vulnerable = n_burden_vulnerable / n_households) %>% 
  filter(cost_burdened != "Zero household income")

compute_vulnerable_shares_by_geo <- function(df, geo) {
  df %>% 
    select(SERIAL, HHWT, one_of(c(geo)), sector, cost_burdened) %>% 
    left_join(vulnerable_renters,
              by = "SERIAL") %>%  
    distinct_at(vars(one_of(c("SERIAL",
                              "HHWT",
                              geo,
                              "cost_burdened",
                              "is_vulnerable")))) %>% 
    group_by_at(vars(one_of(c(geo, cost_burdened, is_vulnerable)))) %>% 
    summarize(sample_size = n(),
              n_burden_vulnerable = sum(is_vulnerable, wt = HHWT)) %>% 
    ungroup() %>% 
    group_by(!!geo) %>% 
    mutate(n_households = sum(n_burden_vulnerable),
           share_vulnerable = n_burden_vulnerable / n_households) %>% 
    filter(cost_burdened != "Zero household income")
}


save(st_vulnerable_shares, 
     file = "covid_rent_burden/st_vulnerable_shares.Rdata")

#===============================================================================#
# COMPUTE AGE
#===============================================================================#

# compute counts of burdened vulnerable and all people by demographic
get_demo_by_vulnerable <- function(renter_df, full_df, groups = c("")) {
  
  # get benchmark
  benchmark <- full_df %>% 
    group_by_at(vars(one_of(groups))) %>%
    summarize(sample_size_all = n(),
              group_total = sum(PERWT))
  
  # people in renter households with at least one vulnerable workers
  renter_df %>% 
    mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
    group_by(SERIAL) %>% 
    mutate(vulnerable_ct = sum(is_vulnerable),
           is_vulnerable_renter = if_else(vulnerable_ct > 0, PERWT, 0)) %>% 
    group_by_at(vars(one_of(groups))) %>%
    summarize(sample_size = n(),
              vulnerable_in_group = sum(is_vulnerable_renter)) %>%
    ungroup() %>% 
    left_join(benchmark, 
              by = groups)
}

prep_for_plot <- function(df, groups = c("")) {
  df %>% 
    group_by_at(vars(one_of(groups))) %>%
    mutate(total = sum(group_total),
           vulnerable_total = sum(vulnerable_in_group),
           group_share = group_total / total,
           vulnerable_share = vulnerable_in_group / vulnerable_total) %>% 
    pivot_longer(cols = c("group_share", "vulnerable_share")) 
}

st_age_by_burden <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_demo_by_vulnerable(data,
                         c("STATEFIP", "age_cat")) %>% 
  prep_for_plot(c("STATEFIP"))

save(st_age_by_burden,
     file = "covid_rent_burden/st_age_by_burden.Rdata")



st_raceth_by_burden <- renters %>% 
  filter(cost_burdened != "Zero household income") %>% 
  get_demo_by_vulnerable(data,
                         c("STATEFIP", "raceth")) %>% 
  prep_for_plot(c("STATEFIP"))

save(st_raceth_by_burden,
     file = "covid_rent_burden/st_raceth_by_burden.Rdata")
