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
          "janitor")
lapply(libs, library, character.only = TRUE)

# read in CPS ASEC household file and divide weights by 100
cpshh <- read_csv("data/cps_asec/cpspb/hhpub19.csv") %>% 
  mutate(hhwt = HSUP_WGT / 100)

# get renter households who aren't in group quarters
renters <- cpshh %>% 
  filter(H_TENURE == 2,
         HHSTATUS != 0) 

#===============================================================================#
# HOUSEHOLD TYPES AND SIZES
#===============================================================================#

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




