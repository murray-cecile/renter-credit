#===============================================================================#
# CPS DATA CLEANING FUNCTIONS
#
# Cecile Murray
#===============================================================================#


#===============================================================================#
# VARIABLE LISTS
#===============================================================================#

person_vars <- c("PERIDNUM",
                 "PH_SEQ",
                 "PF_SEQ",
                 "P_SEQ",
                 "MARSUPWT",
                 "GTCBSA",
                 "A_AGE",
                 "A_FAMTYP",
                 "HHDFMX",
                 "PERRP",
                 "PEHSPNON",
                 "PRDTRACE",
                 "PEARNVAL",
                 "SEMP_VAL",
                 "ANN_VAL",
                 "DBTN_VAL",
                 "CAP_VAL",
                 "DIV_VAL",
                 "INT_VAL",
                 "OI_OFF",
                 "OI_VAL",
                 "PNSN_VAL",
                 "POTHVAL",
                 "PTOTVAL",
                 "RNT_VAL",
                 "AGI",
                 "DEP_STAT",
                 "CTC_CRD",
                 "ACTC_CRD",
                 "EIT_CRED",
                 "FED_RET",
                 "FEDTAX_BC",
                 "FEDTAX_AC",
                 "FICA",
                 "FILESTAT",
                 "STATETAX_A",
                 "STATETAX_B",
                 "TAX_ID",
                 "TAX_INC"
                 )

#===============================================================================#
# INCOME
#===============================================================================#

recode_hhinc <- function(df) {
  df %>% mutate(hhinc_labels = case_when(
    HHINC == 1 ~ "$0 TO $2,500",
    HHINC == 2 ~ "$2,500 TO $4,999",
    HHINC == 3 ~ "$5,000 TO $7,499",
    HHINC == 4 ~ "$7,500 TO $9,999",
    HHINC == 5 ~ "$10,000 TO $12,499",
    HHINC == 6 ~ "$12,500 TO $14,999",
    HHINC == 7 ~ "$15,000 TO $17,499",
    HHINC == 8 ~ "$17,500 TO $19,999",
    HHINC == 9 ~ "$20,000 TO $22,499",
    HHINC == 10 ~ "$22,500 TO $24,999",
    HHINC == 11 ~ "$25,000 TO $27,499",
    HHINC == 12 ~ "$27,500 TO $29,999",
    HHINC == 13 ~ "$30,000 TO $32,499",
    HHINC == 14 ~ "$32,500 TO $34,999",
    HHINC == 15 ~ "$35,000 TO $37,499",
    HHINC == 16 ~ "$37,500 TO $39,999",
    HHINC == 17 ~ "$40,000 TO $42,499",
    HHINC == 18 ~ "$42,500 TO $44,999",
    HHINC == 19 ~ "$45,000 TO $47,499",
    HHINC == 20 ~ "$47,500 TO $49,999", 
    HHINC == 21 ~ "$50,000 TO $52,499", 
    HHINC == 22 ~ "$52,500 TO $54,999",
    HHINC == 23 ~ "$55,000 TO $57,499",
    HHINC == 24 ~ "$57,500 TO $59,999",
    HHINC == 25 ~ "$60,000 TO $62,499",
    HHINC == 26 ~ "$62,500 TO $64,999",
    HHINC == 27 ~ "$65,000 TO $67,499",
    HHINC == 28 ~ "$67,500 TO $69,999",
    HHINC == 29 ~ "$70,000 TO $72,499",
    HHINC == 30 ~ "$72,500 TO $74,999",
    HHINC == 31 ~ "$75,000 TO $77,499",
    HHINC == 32 ~ "$77,500 TO $79,999",
    HHINC == 33 ~ "$80,000 TO $82,499",
    HHINC == 34 ~ "$82,500 TO $84,999",
    HHINC == 35 ~ "$85,000 TO $87,499",
    HHINC == 36 ~ "$87,500 TO $89,999",
    HHINC == 37 ~ "$90,000 TO $92,499",
    HHINC == 38 ~ "$92,500 TO $94,999",
    HHINC == 39 ~ "$95,000 TO $97,499",
    HHINC == 40 ~ "$97,500 TO $99,999",
    HHINC == 41 ~ "Over $100,000",
    TRUE ~ NA_character_)
  )
}
