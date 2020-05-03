#===============================================================================#
# ANALYZE TAX CHARACTERISTICS OF SPM UNITS WHO RENT
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "reldist")
lapply(libs, library, character.only = TRUE)

source("utils.R")

#===============================================================================#
# ASSEMBLE CPS DATA FRAME
#===============================================================================#

# read in CPS ASEC household file and divide weights by 100
cpshh <- read_csv("data/cps_asec/cpspb/hhpub19.csv") %>% 
  mutate(hhwt = HSUP_WGT / 100)

# read in CPS ASEC person file 
cpsper <- read_csv("data/cps_asec/cpspb/pppub19.csv") 

# filter to just vars of interest
persons <- cpsper %>% 
  select(one_of(person_vars), # this is defined in another file right now
         contains("SPM_")) %>% 
  mutate(perwt = MARSUPWT / 100,
         h_idnum = substr(PERIDNUM, 1, 20)) %>% 
  left_join(
    select(cpshh,
           H_IDNUM,
           GESTFIPS, 
           H_TENURE,
           HHSTATUS,
           GTCBSAST,
           GTMETSTA),
    by = c("h_idnum" = "H_IDNUM")
  )

p_renters <- persons %>% 
  filter(H_TENURE == 2,
         HHSTATUS != 0)

#===============================================================================#
# TALLY NON-WAGE INCOME TYPES BY SPM UNIT
#===============================================================================#

# define list of vars of interest
INCOME_VAR_LIST <- c("CAP_VAL",
                     "ANN_VAL",
                     "DBTN_VAL",
                     "DIV_VAL",
                     "INT_VAL",
                     "POTHVAL",
                     "has_cap_gains",
                     "has_ret_ann",
                     "has_div",
                     "has_int",
                     "has_other")

nonwage <- p_renters %>% 
  mutate(
    has_cap_gains = if_else(CAP_VAL > 0, TRUE, FALSE),
    has_ret_ann = if_else(ANN_VAL > 0 | DBTN_VAL > 0, TRUE, FALSE),
    has_div = if_else(DIV_VAL > 0, TRUE, FALSE),
    has_int = if_else(INT_VAL > 0, TRUE, FALSE),
    has_other = if_else(POTHVAL > 0, TRUE, FALSE),
    tuhead = if_else(SPM_HEAD == 1, TRUE, FALSE),
    ct = 1
  ) %>% 
  group_by(SPM_ID, SPM_WEIGHT) %>% 
  summarize_at(
    vars(one_of(c(INCOME_VAR_LIST, "tuhead", "ct"))),
    ~ sum(., na.rm = TRUE)
  ) %>% 
  mutate(
    has_cap_gains = if_else(CAP_VAL > 0, 1, 0),
    has_ret_ann = if_else(ANN_VAL > 0 | DBTN_VAL > 0, 1, 0),
    has_div = if_else(DIV_VAL > 0, 1, 0),
    has_int = if_else(INT_VAL > 0, 1, 0),
    has_other = if_else(POTHVAL > 0, 1, 0)
  ) %>% 
  ungroup() %>% 
  select(-SPM_ID) %>% 
  summarize_all(
    list(~ sum(., na.rm = TRUE),
         ~ sum(. * SPM_WEIGHT / 100, na.rm = TRUE))
  ) %>% 
  pivot_longer(-starts_with("tuhead"),
              names_to = "key",
              values_to = "value") %>% 
  mutate(
    key2 = if_else(str_detect(key, "_sum..1"), "sample_size", ""),
    key2 = if_else(str_detect(key, "_sum..2"), "n", key2),
    key = str_remove(key, "_sum..\\d")
  ) %>% 
  pivot_wider(
    names_from = key2,
    values_from = value
  ) %>% 
  dplyr::rename(
    "tuhead_sample_size" = "tuhead_sum..1",
    "tuhead_n" = "tuhead_sum..2",
  ) %>% 
  filter(key != "SPM_WEIGHT") %>% 
  select(
    key,
    sample_size,
    n, 
    contains("tuhead")
  ) %>% 
  mutate(
    spm_share = n / tuhead_n
  )

# where are folks with significant share of non pearnval earnings



#===============================================================================#
# AGI
#===============================================================================#

# what is typical AGI by state/metro status?
typical_agi <- p_renters %>% 
  arrange(TAX_ID, DEP_STAT) %>% 
  group_by(TAX_ID, perwt) %>%
  summarize(
    tu_agi = sum(AGI, na.rm = TRUE),
    stfips = str_pad(as.character(first(GESTFIPS)), 2, "left", "0"),
    metro_status = first(GTMETSTA),
    tuhead_perwt = first(perwt)
  ) %>% 
  ungroup() %>% 
  group_by(stfips, metro_status) %>% 
  summarize(
    sample_size = n(),
    n_tax_units = round(sum(tuhead_perwt)), # not 100% sure about this
    p25_agi = wtd.quantile(tu_agi, q = 0.25, weight = tuhead_perwt),
    median_agi = wtd.quantile(tu_agi, q = 0.5, weight = tuhead_perwt),
    p75_agi = wtd.quantile(tu_agi, q = 0.75, weight = tuhead_perwt),
    p90_agi = wtd.quantile(tu_agi, q = 0.9, weight = tuhead_perwt)
  ) %>% 
  mutate(
    metro_status = if_else(metro_status == 1, "Metro", "Non-metro"),
  ) %>% 
  left_join(
    distinct(tidycensus::fips_codes, state_name, state_code),
    by = c("stfips" = "state_code")
  ) %>% 
  select(
    stfips, 
    state_name,
    metro_status,
    everything()
    )

write_csv(typical_agi,
          path = "data/tables/typical_renter_agi_CPS.csv")

#===============================================================================#
# STATE TAXES
#===============================================================================#

state_tax <- p_renters %>% 
  arrange(TAX_ID, DEP_STAT) %>% 
  group_by(TAX_ID) %>% 
  summarize(
    stfips = str_pad(as.character(first(GESTFIPS)), 2, "left", "0"),
    metro_status = first(GTMETSTA),
    FILESTAT = first(FILESTAT),
    FEDTAX_AC = first(FEDTAX_AC),
    STATETAX_A = first(STATETAX_A),
    tuhead_perwt = first(perwt)
  ) %>% 
  ungroup() %>% 
  mutate(
    std_deduct = case_when(
      FILESTAT == 1 ~ 24400,
      FILESTAT == 2 ~ 25700, # ignoring blind/disability increase here
      FILESTAT == 3 ~ 27000,
      FILESTAT == 4 ~ 18350,
      FILESTAT == 5 ~ 12200,
      FILESTAT == 6 ~ NA_real_
    ),
    state_tax_pct = STATETAX_A / std_deduct,
    sttax_flag = if_else(state_tax_pct > 0.5, 1, 0)
  ) %>% 
  group_by(stfips, metro_status) %>% 
  summarize(
    sample_size = n(),
    n_tax_units = round(sum(tuhead_perwt)), # not 100% sure about this
    median_fedtax = wtd.quantile(FEDTAX_AC, q = 0.5, weight = tuhead_perwt),
    median_sttax = wtd.quantile(STATETAX_A, q = 0.5, weight = tuhead_perwt),
    n_sttax_large = round(sum(sttax_flag * tuhead_perwt, na.rm = TRUE))
  ) %>% 
  mutate(
    high_sttax_share = n_sttax_large / n_tax_units,
    metro_status = if_else(metro_status == 1, "Metro", "Non-metro"),
  ) %>% 
  left_join(
    distinct(tidycensus::fips_codes, state_name, state_code),
    by = c("stfips" = "state_code")
  ) %>% 
  select(
    stfips, 
    state_name,
    metro_status,
    everything()
  )

# write_csv(state_tax,
#           path = "data/tables/state_tax_figures_CPS.csv")
