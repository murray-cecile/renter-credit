#===============================================================================#
# CPS ASEC INCOME NUMBERS
#
# Cecile Murray
#===============================================================================#

libs <- c("here",
          "tidyverse",
          "purrr",
          "knitr", 
          "kableExtra",
          "janitor",
          "matrixStats")
lapply(libs, library, character.only = TRUE)

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

#===============================================================================#
# SPM UNITS WHO ARE RENTERS
#===============================================================================#

# subset to renters who aren't in group quarters
p_renters <- persons %>% 
  filter(H_TENURE == 2,
         HHSTATUS != 0) 

# now I want to know what share have positive taxes after credits
summary <- p_renters %>% 
  filter(SPM_HEAD == 1) %>% 
  mutate(
    pos_fedtax_ac = if_else(SPM_FEDTAX > 0, 1, 0),
    has_cap_gains = if_else(CAP_VAL > 0, 1, 0),
    has_ret_ann = if_else(ANN_VAL > 0 | DBTN_VAL > 0, 1, 0),
    has_div = if_else(DIV_VAL > 0, 1, 0),
    has_int = if_else(INT_VAL > 0, 1, 0),
    has_other = if_else(POTHVAL > 0, 1, 0),
    # earn_pct_income = if_else(PTOTVAL != 0, PEARNVAL / PTOTVAL, NA_real_), # note this could be negative
    std_deduct = case_when(
      FILESTAT == 1 ~ 24400,
      FILESTAT == 2 ~ 25700, # ignoring blind/disability increase here
      FILESTAT == 3 ~ 27000,
      FILESTAT == 4 ~ 18350,
      FILESTAT == 5 ~ 12200,
      FILESTAT == 6 ~ NA_real_
    ),
    state_tax_pct = SPM_STTAX / std_deduct,
    state_tax_gt_std_dd = if_else(state_tax_pct > 0.75, 1, 0),
    ct = 1
  ) %>% 
  select(-PERIDNUM, -h_idnum,  -std_deduct) %>%
  summarize_at(vars(pos_fedtax_ac:ct),
               list(~ sum(. * SPM_WEIGHT / 100, na.rm = TRUE),
                    ~ mean(. * SPM_WEIGHT / 100, na.rm = TRUE))) %>% 
  gather("var", "val",  -ct_sum) %>% 
  mutate(share = val / ct_sum)

# what about other sources?
p_renters %>% dplyr::count(OI_OFF)

p_renters %>% 
  filter(OI_OFF != 0) %>% 
  group_by(SPM_ID, SPM_WEIGHT) %>% 
  summarize(
    ct = sum(POTHVAL > 0,  na.rm = TRUE),
    sum = sum(POTHVAL, na.rm = TRUE)
  ) %>% 
  ungroup() %>% 
  summarize(
    ct = sum((ct > 0) * SPM_WEIGHT / 100, na.rm = TRUE),
    sum = sum(sum * SPM_WEIGHT / 100, na.rm = TRUE)
  )

#===============================================================================#
# HOUSEHOLD TYPES AND SIZES
#===============================================================================#

# get renter households who aren't in group quarters
renters <- cpshh %>% 
  filter(H_TENURE == 2,
         HHSTATUS != 0) 


# family households vs. not, household totals look like ACS totals
renters %>% 
  group_by(HHSTATUS) %>% 
  summarize(sum(hhwt))

# then # of people per HH: 90% have 4 or fewer people
renters %>% 
  group_by(H_NUMPER) %>% 
  summarize(n_hh = sum(hhwt)) %>% 
  ungroup() %>% 
  mutate(total = sum(n_hh),
         n_hh_share = n_hh / total,
         cum_hh_share = cumsum(n_hh_share)) %>% 
  ggplot(aes(x = H_NUMPER,
             y = cum_hh_share)) +
  geom_col()

# kid presence in renter homes vs. owned homes, exlcuding group quarters/no cash rent
cpshh %>% 
  filter(HHSTATUS != 0,
         H_TENURE %in% c(1, 2)) %>% 
  group_by(H_TENURE, HUNDER18) %>% 
  summarize(n_hh = sum(hhwt)) %>% 
  ungroup() %>% 
  group_by(H_TENURE) %>% 
  mutate(total = sum(n_hh),
         n_hh_share = n_hh / total,
         cum_hh_share = cumsum(n_hh_share)) %>% 
  ggplot(aes(x = HUNDER18,
             y = n_hh_share,
             group = H_TENURE,
             fill = H_TENURE)) +
  geom_col(position = "dodge")

# maybe slightly more likely to have one kid than owned homes, but not so different

#===============================================================================#
# INCOME
#===============================================================================#

renters %>% 
  recode_hhinc() %>% 
  group_by(hhinc_labels, HHINC) %>% 
  summarize(n_hh = sum(hhwt)) %>% 
  ungroup() %>% 
  arrange(HHINC) %>% 
  mutate(total = sum(n_hh),
         n_hh_share = n_hh / total,
         cum_hh_share = cumsum(n_hh_share)) %>% 
  ggplot(aes(x = reorder(hhinc_labels, HHINC),
             y = cum_hh_share)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45))

# about 50% of renter households make less than $45K and about 75% make less
# than $77.5K

# wages and salaries as a percent of income




