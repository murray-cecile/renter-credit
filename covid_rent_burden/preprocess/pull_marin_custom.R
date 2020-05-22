#===============================================================================#
# CUSTOM PULL FOR MARIN COUNTY
#
# Cecile Murray
# 2020-05-22
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


MARIN_PUMAS <- c("0604101", "0604102")

vulnerable_sectors <- c("Non-essential retail",
                        "Food service",
                        "Mining",
                        "Entertainment",
                        "Non-essential manufacturing",
                        "Non-essential travel/transportation",
                        "Other services")


#===============================================================================#
# CHECK SAMPLE SIZE
#===============================================================================#

# marin_all <- data %>% 
#   filter(puma_id %in% MARIN_PUMAS)
# 
# # 600 ish total people in renter households
# marin_renters <- renters %>% 
#   filter(puma_id %in% MARIN_PUMAS)
# 
# # 299 renter households
# marin_renters %>% 
#   distinct(SERIAL, .keep_all = TRUE) %>% 
#   nrow()
# 
# marin_renters %>% 
#   filter(sector %in% vulnerable_sectors) %>% 
#   nrow()
# 
# marin_renters %>% count(raceth) 
# marin_renters %>% count(age_cat) 
# 
# marin_renters %>% 
#   distinct(SERIAL, .keep_all = TRUE) %>% 
#   count(cost_burdened) 

#===============================================================================#
# CREATE MARIN EXTRACT
#===============================================================================#

# use all people/households but flag renters and vulnerable
marin <- data %>% 
  filter(puma_id %in% MARIN_PUMAS) %>% 
  mutate(
    renter_hhold = if_else(OWNERSHP == 2, 1, 0),
    is_vulnerable = if_else(sector %in% vulnerable_sectors, 1, 0)
  ) %>% 
  group_by(SERIAL) %>% 
  mutate(
    num_vulnerable_workers = sum(is_vulnerable, na.rm = TRUE),
    is_vulnerable_hhold = if_else(num_vulnerable_workers > 0, 1, 0)
  ) %>% 
  ungroup() %>% 
  left_join(
    distinct(
      renters,
      SERIAL,
      cost_burdened
    ),
    by = "SERIAL"
  )


#===============================================================================#
# IMPACTED RENTERS
#===============================================================================#

# number of households w/ at least one worker likely impacted
marin_impacted <- marin %>% 
  distinct(
    SERIAL,
    HHWT,
    renter_hhold,
    is_vulnerable_hhold,
    RENTGRS,
    cost_burdened
  ) %>% 
  filter(
    renter_hhold == 1,
    cost_burdened != "Zero income household"
  ) %>% 
  summarize(
    total_renters = sum(HHWT, na.rm = TRUE),
    impacted_hholds = sum(is_vulnerable_hhold * HHWT, na.rm = TRUE),
    total_rent = sum(RENTGRS * HHWT, na.rm = TRUE),
    impacted_rent = sum(is_vulnerable_hhold * HHWT * RENTGRS, na.rm = TRUE)
  ) 

# compute median rent and income of such households
marin_impacted_finances <- marin %>% 
  distinct(
    SERIAL,
    HHWT,
    renter_hhold,
    is_vulnerable_hhold,
    cost_burdened,
    RENTGRS,
    HHINCOME
  ) %>% 
  filter(
    renter_hhold == 1, 
    is_vulnerable_hhold == 1,
    cost_burdened != "Zero household income"
  ) %>% 
  summarize(
    impacted_median_rent = weightedMedian(RENTGRS, w = HHWT),
    impacted_median_income = weightedMedian(HHINCOME, w = HHWT)
  )

#===============================================================================#
# CUT IMPACTED RENTERS BY EXISTING COST BURDEN
#===============================================================================#

# number of impacted hholds cut by existing cost burden
marin_impacted_burden <- marin %>% 
  distinct(
    SERIAL,
    HHWT,
    renter_hhold,
    is_vulnerable_hhold,
    RENTGRS,
    cost_burdened
  ) %>% 
  filter(
    renter_hhold == 1,
    cost_burdened != "Zero income household"
  ) %>% 
  group_by(cost_burdened) %>% 
  summarize(
    total_renters = sum(HHWT, na.rm = TRUE),
    impacted_hholds = sum(is_vulnerable_hhold * HHWT, na.rm = TRUE),
    total_rent = sum(RENTGRS * HHWT, na.rm = TRUE),
    impacted_rent = sum(is_vulnerable_hhold * HHWT * RENTGRS, na.rm = TRUE)
  ) 

# median rent and income among impacted by existing cost burden
marin_impacted_finances_burden <- marin %>% 
  distinct(
    SERIAL,
    HHWT,
    renter_hhold,
    is_vulnerable_hhold,
    cost_burdened,
    RENTGRS,
    HHINCOME
  ) %>% 
  filter(
    renter_hhold == 1, 
    is_vulnerable_hhold == 1,
    cost_burdened != "Zero household income"
  ) %>% 
  group_by(cost_burdened) %>% 
  summarize(
    impacted_median_rent = weightedMedian(RENTGRS, w = HHWT),
    impacted_median_income = weightedMedian(HHINCOME, w = HHWT)
  )

#===============================================================================#
# UNIT TESTS
#===============================================================================#

assertthat::are_equal(
  marin_impacted$total_renters,
  sum(marin_impacted_burden$total_renters)
)

assertthat::are_equal(
  marin_impacted$impacted_hholds,
  sum(marin_impacted_burden$impacted_hholds)
)

assertthat::are_equal(
  marin_impacted$total_rent,
  sum(marin_impacted_burden$total_rent)
)

assertthat::are_equal(
  marin_impacted$impacted_rent,
  sum(marin_impacted_burden$impacted_rent)
)

#===============================================================================#
# PREPARE FOR EXPORT
#===============================================================================#

marin_impacted_long <- bind_rows(
  marin_impacted %>% pivot_longer(everything(), names_to = "metric"),
  marin_impacted_finances %>% pivot_longer(everything(), names_to = "metric"),
) %>% 
  mutate(
    value = if_else(str_detect(metric, "median_rent"),
                    round(value, -1),
                    round(value, -3)),
  )

marin_impacted_burden_long <- bind_rows(
  marin_impacted_burden %>% pivot_longer(-cost_burdened, names_to = "metric"),
  marin_impacted_finances_burden %>% pivot_longer(-cost_burdened, names_to = "metric"),
) %>% 
  mutate(
    value = if_else(str_detect(metric, "median_rent"),
                    round(value, -1),
                    round(value, -3)),
  ) %>% 
  pivot_wider(
    names_from = "cost_burdened",
    values_from = "value"
  )

marin_table <- inner_join(
  marin_impacted_long,
  marin_impacted_burden_long,
  by = "metric"
) %>% 
  dplyr::rename(
    "Total" = "value",
    "Already burdened" = "Rent burdened",
    "Newly vulnerable" = "Not burdened"
  ) %>% 
  mutate(
    label = case_when(
      metric == "total_renters" ~ "Total renter households",
      metric == "impacted_hholds" ~ "Renter households with at least one worker likely impacted",
      metric == "total_rent" ~ "Total monthly rents",
      metric == "impacted_rent" ~ "Total monthly rents of likely impacted households",
      metric == "impacted_median_rent" ~ "Median gross rent of likely impacted households",
      metric == "impacted_median_income" ~ "Median income of likely impacted households"
    )
  ) %>% 
  select(
    label,
    `Already burdened`,
    `Newly vulnerable`,
    `Total`
  ) 

#===============================================================================#
# EXPORT FILES
#===============================================================================#

write.xlsx(marin_table, "data/tables/Marin_appendix_table.xlsx")

write_dta(marin, "data/acs/Marin_PUMA_extract.dta")
