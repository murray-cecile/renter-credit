#!usr/bin/bash

cd ~/Documents/renter-credit/data/acs/raw_pums

filetype="h p"

for f in $filetype; do

    # this is slow - consider running in background
    # wget "https://www2.census.gov/programs-surveys/acs/data/pums/2018/1-Year/csv_${f}us.zip"
    mv csv_${f}us.zip csv_${f}us_2018_1yr.zip
    unzip csv_${f}us_2018_1yr.zip

    # # concatenate files together
    # cat psam_$fusa.csv > $fusa.csv
    # for l in "b c d"; do

    #     sed 1d psam_$fus$l.csv >> $fusa.csv

done


