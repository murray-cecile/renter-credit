#===============================================================================#
# USEFUL FUNCTIONS
#
# Cecile Murray
#===============================================================================#


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


# convert statefips to generic geoid
convert_state_code <- function(df) {
  df %>% 
    left_join(distinct(tidycensus::fips_codes,
                       state_code,
                       state_name),
              by = c("STATEFIP" = "state_code")) %>% 
    dplyr::rename("GEOID" = "STATEFIP",
                  "NAME" = "state_name")
}

convert_cbsa_code <- function(df) {
  df %>% 
    left_join(distinct(puma_xwalk,
                       cbsa_code,
                       cbsa_title),
              by = "cbsa_code") %>% 
    ungroup() %>% 
    mutate(cbsa_code = as.character(cbsa_code)) %>% 
    dplyr::rename("GEOID" = "cbsa_code",
                  "NAME" = "cbsa_title")
}

write_to_excel <- function(wb_path, table_list) {
  
  wb <- createWorkbook()
  for(t in table_list){
    addWorksheet(wb, t)
    writeData(wb, t, get(t))
  }
  saveWorkbook(wb, wb_path, overwrite = TRUE)
}
