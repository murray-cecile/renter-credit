#!usr/bin/bash

cd ~/Documents/renter-credit/data/acs/raw_pums

filetype="h p"

for f in $filetype; do

    # this is slow - consider running in background
    wget "https://www2.census.gov/programs-surveys/acs/data/pums/2018/5-Year/csv_${f}us.zip"
    unzip csv_$fus.zip

done