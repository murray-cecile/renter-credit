#===============================================================================#
# EXTRACT IPUMS DATA FILES
#
# Cecile Murray
#===============================================================================#

library(here)
library(ipumsr)

# NOTE: To load data, you must download both the extract's data and the DDI
# and also set the working directory to the folder with these files (or change the path below).
ddi <- read_ipums_ddi("data/acs/usa_00002.xml")
data <- read_ipums_micro(ddi)

save(data, file = "data/acs/acs_2018.Rdata")
