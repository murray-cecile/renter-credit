#===============================================================================#
# FUNCTIONS
#
# Cecile Murray
#===============================================================================#

library(openxlsx)


write_to_excel <- function(wb_path, table_list) {
  
  wb <- createWorkbook()
  for(t in table_list){
    addWorksheet(wb, t)
    writeData(wb, t, get(t))
  }
  saveWorkbook(wb, wb_path, overwrite = TRUE)
}

# demo
# l <- list("natl_age_table", "natl_renter_income")
# write_to_excel("test.xlsx", l)
