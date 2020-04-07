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
# COMPUTE 
#===============================================================================#

# compute median monthly rents and total rent for burdened vs. non-burdened vulnerable
get_rent_by_burden <- function(df, groups = c("")) {
  df %>% 
    mutate(is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)) %>% 
    group_by(SERIAL) %>% 
    mutate(vulnerable_ct = sum(is_vulnerable),
           is_vulnerable = if_else(vulnerable_ct > 0, TRUE, FALSE)) %>%
    filter(is_vulnerable,
           cost_burdened != "Zero household income") %>% 
    group_by_at(vars(one_of(groups))) %>% 
    summarize(sample_size = n(),
              median_rent = median(RENTGRS, wt = HHWT),
              total_rent = sum(RENTGRS, wt = HHWT)) %>% 
    ungroup() 
}

st_rent_by_burden <- renters %>% 
  get_rent_by_burden(c("STATEFIP", "cost_burdened")) %>% 
  mutate(STATEFIP = str_pad(STATEFIP, width = 2, side = "left", pad = "0")) 

save(st_rent_by_burden,
     file = "covid_rent_burden/st_rent_by_burden.Rdata")

