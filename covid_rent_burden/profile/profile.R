#===============================================================================#
# IMPROVE SHINY PERFORMANCE
#
# Cecile Murray
#===============================================================================#

library(shiny)
library(profvis)

profvis({
  runApp("covid_rent_burden")
})

# INSIGHTS:
#
# 1. Reduce the # of metro areas considered (e.g. exclude all micro)
# 2. Reorganize so only call filtered data frames 1x?
# 2. Try using dtplyr or data.table syntax