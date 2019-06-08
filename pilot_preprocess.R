### Preprocess the Sepsis Pilot Data
### Date Created: 2/24/2019
### Date Updated: 2/24/2019
### Author: Anthony Lin (anthony.lee.lin@gmail.com)

#### Make sure your packages are installed ####
#install.packages("data.table", repo="http://archive.linux.duke.edu/cran/")
#install.packages("lubridate", repo="http://archive.linux.duke.edu/cran/")
#install.packages("bit64", repo="http://archive.linux.duke.edu/cran/")

#### Load up your libraries ####
library(data.table)
library(lubridate)
library(bit64)

#### Clean your workspace ####
rm(list = ls())

#### Read in your raw data ####
encounters <- fread("P:/dihi_qi/sepsis_outcome_validation/Data/11_06_2018_02_06_2019/encounter.txt")
analytes <- fread("P:/dihi_qi/sepsis_outcome_validation/Data/11_06_2018_02_06_2019/analyte.txt")
medications <- fread("P:/dihi_qi/sepsis_outcome_validation/Data/11_06_2018_02_06_2019/medication.txt")
sw_status <- fread("P:/dihi_qi/sepsis_outcome_validation/Data/11_06_2018_02_06_2019/sw_patient_status_all.txt")

#### Preprocessing Step ####

# Standardize all the timestamps as POSIXct
encounters$EMERGENCY_ADMIT_TIME <- as.POSIXct(encounters$EMERGENCY_ADMIT_TIME, format = "%Y-%m-%d %H:%M:%S")
encounters$INPATIENT_ADMIT_TIME <- as.POSIXct(encounters$INPATIENT_ADMIT_TIME, format = "%Y-%m-%d %H:%M:%S")
encounters$OUTPATIENT_ADMIT_TIME <- as.POSIXct(encounters$OUTPATIENT_ADMIT_TIME, format = "%Y-%m-%d %H:%M:%S")
encounters$DISCHARGE_TIME <- as.POSIXct(encounters$DISCHARGE_TIME, format = "%Y-%m-%d %H:%M:%S")
encounters$BIRTH_DATE <- as.POSIXct(encounters$BIRTH_DATE, format = "%Y-%m-%d %H:%M:%S")

analytes$ORDER_TIME <- as.POSIXct(analytes$ORDER_TIME, format = "%Y-%m-%d %H:%M:%S")
analytes$COLLECTION_TIME <- as.POSIXct(analytes$COLLECTION_TIME, format = "%Y-%m-%d %H:%M:%S")
analytes$RESULT_TIME <- as.POSIXct(analytes$RESULT_TIME, format = "%Y-%m-%d %H:%M:%S")

medications$ADMINISTRATION_TIME <- as.POSIXct(medications$ADMINISTRATION_TIME, format = "%Y-%m-%d %H:%M:%S")

sw_status$STATUS_TIME <- as.POSIXct(sw_status$STATUS_TIME, format = "%Y-%m-%d %H:%M:%S")
sw_status$CREATED_AT <- as.POSIXct(sw_status$CREATED_AT, format = "%Y-%m-%d %H:%M:%S")

# Sort into post-processed data tables
DAT_encounters <- encounters[,.(ENCOUNTER_ID, PATIENT_MRN, EMERGENCY_ADMIT_TIME, INPATIENT_ADMIT_TIME, OUTPATIENT_ADMIT_TIME, DISCHARGE_TIME)]
DAT_patients <- encounters[,.(PATIENT_MRN, BIRTH_DATE)]
DAT_patients <- unique(DAT_patients, by="PATIENT_MRN")
DAT_analytes <- analytes[,.(ENCOUNTER_ID, RAW_NAME, ANALYTE_GROUP, VALUE, VALUE_TXT, REFERENCE_UNIT, ORDER_TIME, COLLECTION_TIME, RESULT_TIME)]
DAT_medications <- medications[,.(ENCOUNTER_ID, RAW_NAME, THERA_CLASS, DOSE, DOSE_UNIT, ADMINISTRATION_TIME)]
DAT_sw_status <- sw_status[,.(ENCOUNTER_ID, STATUS_TIME, STATUS, REMINDER, CREATED_BY, CREATED_AT)]

# Standardize the ordering of each data table
DAT_encounters <- DAT_encounters[order(PATIENT_MRN, ENCOUNTER_ID, EMERGENCY_ADMIT_TIME)]
DAT_patients <- DAT_patients[order(PATIENT_MRN)]
DAT_analytes <- DAT_analytes[order(ENCOUNTER_ID, ORDER_TIME, COLLECTION_TIME, RESULT_TIME, RAW_NAME)]
DAT_medications <- DAT_medications[order(ENCOUNTER_ID, ADMINISTRATION_TIME, RAW_NAME)]
DAT_sw_status <- DAT_sw_status[order(ENCOUNTER_ID, STATUS_TIME)]

#### Remove all temporary variables in the workspace ####
rm(list=setdiff(ls(), ls(pattern="^DAT")))

#### Save workspace ####
save.image("P:/dihi_qi/sepsis_outcome_validation/Data/Cleaned/2019_02_24 Cleaned Pilot Data.RData")


