### Build Key Sepsis Definitions for the Pilot Data
### Date Created: 2/24/2019
### Date Updated: 2/24/2019
### Author: Anthony Lin (anthony.lee.lin@gmail.com)

#### Make sure your packages are installed ####
#install.packages("data.table", repo="http://archive.linux.duke.edu/cran/")
#install.packages("lubridate", repo="http://archive.linux.duke.edu/cran/")
#install.packages("bit64", repo="http://archive.linux.duke.edu/cran/")
#install.packages("dplyr", repo="http://archive.linux.duke.edu/cran/")

#### Load up your libraries ####
library(data.table)
library(lubridate)
library(bit64)
library(dplyr)

#### Clean your workspace ####
rm(list = ls())

#### Load preprocessed data elements
load("P:/dihi_qi/sepsis_outcome_validation/Data/Cleaned/2019_02_24 Cleaned Pilot Data.RData")

# Load in your definition-specific crosswalks
cdc_abx_crosswalk <- fread("P:/dihi_qi/sepsis_outcome_validation/Crosswalks/cdc_antibiotics_crosswalk.csv")

# Subset analytes and medications to make definition calculations easier
blood_cx <- DAT_analytes[ANALYTE_GROUP == "CULTURE"]
bilirubin <- DAT_analytes[ANALYTE_GROUP == "BILIRUBIN"]
creatinine <- DAT_analytes[ANALYTE_GROUP == "CREATININE"]
platelet <- DAT_analytes[ANALYTE_GROUP == "PLATELETS"]
lactate <- DAT_analytes[ANALYTE_GROUP == "LACTATE"]

antibiotics <- DAT_medications[THERA_CLASS == "Antibiotics"]
vasopressors <- DAT_medications[THERA_CLASS == "Vasopressors"]

# subset the antibiotics to just CDC-qualifying antibiotics
cdc_antibiotics <- merge(antibiotics, cdc_abx_crosswalk[,.(COMPONENT_NAME, CDC_DEFN_YN, ANTIBIOTIC, ROUTE)], by.x="RAW_NAME", by.y="COMPONENT_NAME", all.x = TRUE)
cdc_antibiotics <- cdc_antibiotics[CDC_DEFN_YN == "YES"]

cdc_antibiotics <- cdc_antibiotics[,.(ENCOUNTER_ID, RAW_NAME, ADMINISTRATION_TIME, ANTIBIOTIC, ROUTE)]
cdc_antibiotics <- cdc_antibiotics[order(ENCOUNTER_ID, ADMINISTRATION_TIME)]

# Mark the CDC antibiotics to find the administrations that mark the beginning of 4 consecutive calendar days of antibiotics
cdc_antibiotics$INDEX <- seq(1:nrow(cdc_antibiotics))
cdc_antibiotics$DAY <- floor_date(cdc_antibiotics$ADMINISTRATION_TIME, "days")

tmp_antibiotics <- unique(cdc_antibiotics, by=c("ENCOUNTER_ID", "DAY"))
tmp_antibiotics <- tmp_antibiotics[order(ENCOUNTER_ID, -ADMINISTRATION_TIME)] #flip the order of the times to calculate the first day of 4 consectutive days of antibiotics
tmp_antibiotics[, DIFF_TIME := c(NA, difftime(DAY[-nrow(tmp_antibiotics)],
                                              DAY[-1],
                                              units="day")), by = ENCOUNTER_ID]

rle_consecutive_days <- rle(tmp_antibiotics$DIFF_TIME == 1)
rle_consecutive_days$values <- rle_consecutive_days$values & rle_consecutive_days$lengths >= 3

tmp_antibiotics <- tmp_antibiotics[inverse.rle(rle_consecutive_days), ]
tmp_antibiotics <- tmp_antibiotics %>% group_by(ENCOUNTER_ID) %>% slice(3:n())

cdc_antibiotics$CONSECUTIVE_4 <- FALSE
cdc_antibiotics[tmp_antibiotics$INDEX]$CONSECUTIVE_4 <- TRUE #mark these rows because they're the ones that have 4 days of consecutive antibiotics. N.B. these will only be the "first" timestamp of each day that has that has had ≥4 consecutive days of antibiotics administration

tmp <- cdc_antibiotics[CONSECUTIVE_4 == TRUE]

## Check to make sure that that same antibiotic wasn't used within the last 2 days + that that antibiotic was IV
tmp[, DIFF_TIME := c(NA, difftime(DAY[-1],
                                  DAY[-nrow(tmp)],
                                  units="day")), by = ENCOUNTER_ID]

tmp <- tmp[, PREV_ANTIBIOTIC := c(NA, ANTIBIOTIC[-.N]), by = ENCOUNTER_ID] #make note of the previously administered antibiotic

qualifying_abx_start_dates <- tmp[is.na(PREV_ANTIBIOTIC) | DIFF_TIME >= 3 | (ANTIBIOTIC != PREV_ANTIBIOTIC)] #these are all the antibiotics that were new antibiotics OR no antibiotics had been given in the prior 2 calendar days OR the previously administered antibiotic was different from the one being administered now

qualifying_abx_start_dates <- qualifying_abx_start_dates[ROUTE == "IV/IM"] #only IV/IM antibiotics can count as a qualifying start date

# For each blood culture collection time, add 2 calendar days and substract 2 calendar days --> this is your window that antibiotic initiation must fall within that time frame
blood_cx[, before_date:=floor_date(COLLECTION_TIME-(48*60*60),"days")]
blood_cx[, after_date:=ceiling_date(COLLECTION_TIME+(48*60*60),"days")-1]

cdc_infection <- merge(blood_cx, qualifying_abx_start_dates, by="ENCOUNTER_ID", allow.cartesian = TRUE)
cdc_infection <- cdc_infection[cdc_infection$ADMINISTRATION_TIME >= cdc_infection$before_date & cdc_infection$ADMINISTRATION_TIME <= cdc_infection$after_date]

cdc_infection <- cdc_infection[,.(ENCOUNTER_ID, COLLECTION_TIME, before_date, after_date, ADMINISTRATION_TIME)]
names(cdc_infection) <- c("ENCOUNTER_ID", "BC_TIME", "before_date", "after_date", "ABX_TIME")



#Cardiovascular -- initation of vasopressors

vasopressors$DAY <- floor_date(vasopressors$ADMINISTRATION_TIME, "days")

vasopressors <- vasopressors[order(ENCOUNTER_ID, ADMINISTRATION_TIME)]
vasopressors[, DIFF_TIME := c(NA, difftime(DAY[-1],
                                           DAY[-nrow(vasopressors)],
                                           units="day")), by = ENCOUNTER_ID]

vp_initiation <- vasopressors[is.na(DIFF_TIME) | DIFF_TIME > 1] #find all the new vasopressor initiation dates

#Pulmonary -- initiation of mechnical ventilation
# TODO: need to pull ETT intubation from clarity to measure vent initiation

#Renal -- Cr 2x the lowest value (considered the baseline)
cr_baselines <- aggregate(creatinine$VALUE, by=list(creatinine$ENCOUNTER_ID), min, na.rm=TRUE)
creatinine <- merge(creatinine, cr_baselines, by.x="ENCOUNTER_ID", by.y="Group.1")
creatinine$ORGAN_DYSFUNCTION <- FALSE
creatinine[VALUE >= 2*x]$ORGAN_DYSFUNCTION <- TRUE

#Hepatic -- bilirubin ≥2.0 AND 2x of the lowest value (considered the baseline)
bili_baselines <- aggregate(bilirubin$VALUE, by=list(bilirubin$ENCOUNTER_ID), min, na.rm=TRUE)
bilirubin <- merge(bilirubin, bili_baselines, by.x="ENCOUNTER_ID", by.y="Group.1")
bilirubin$ORGAN_DYSFUNCTION <- FALSE
bilirubin[VALUE >= 1.5*x & VALUE >= 2]$ORGAN_DYSFUNCTION <- TRUE

#Coagulation -- platelets <100 AND ≥50% decrease from the highest value (considered the baseline)
#if baseline is <100 to begin with, then you can't use platelets to determine organ dysfunction
plt_baselines <- aggregate(platelet$VALUE, by=list(platelet$ENCOUNTER_ID), max, na.rm=TRUE)
platelet <- merge(platelet, plt_baselines, by.x="ENCOUNTER_ID", by.y="Group.1")
platelet$ORGAN_DYSFUNCTION <- FALSE
platelet[VALUE <100 & VALUE <= .5*x & x >= 100]$ORGAN_DYSFUNCTION <- TRUE

#Neurologic -- lactate >= 2.0
lactate$ORGAN_DYSFUNCTION <- FALSE
lactate[VALUE >= 2]$ORGAN_DYSFUNCTION <- TRUE



#Check all the windows and see if there is any overlapping organ dysfunction
cdc_defn_vp <- merge(cdc_infection, vp_initiation, by="ENCOUNTER_ID", all.x = TRUE, allow.cartesian = TRUE)
cdc_defn_vp <- cdc_defn_vp[ADMINISTRATION_TIME >= before_date & ADMINISTRATION_TIME <= after_date]

#cdc_defn_ett <- merge(cdc_infection, ett_initiation, by="ENCOUNTER_ID", all.x = TRUE, allow.cartesian = TRUE)
#cdc_defn_ett <- cdc_defn_ett[PLACEMENT_INSTANT >= before_date & PLACEMENT_INSTANT <= after_date]

cdc_defn_cr <- merge(cdc_infection, creatinine[ORGAN_DYSFUNCTION == TRUE], by="ENCOUNTER_ID", all.x = TRUE, allow.cartesian = TRUE)
cdc_defn_cr <- cdc_defn_cr[COLLECTION_TIME >= before_date & COLLECTION_TIME <= after_date]

cdc_defn_bili <- merge(cdc_infection, bilirubin[ORGAN_DYSFUNCTION == TRUE], by="ENCOUNTER_ID", all.x = TRUE, allow.cartesian = TRUE)
cdc_defn_bili <- cdc_defn_bili[COLLECTION_TIME >= before_date & COLLECTION_TIME <= after_date]

cdc_defn_plt <- merge(cdc_infection, platelet[ORGAN_DYSFUNCTION == TRUE], by="ENCOUNTER_ID", all.x = TRUE, allow.cartesian = TRUE)
cdc_defn_plt <- cdc_defn_plt[COLLECTION_TIME >= before_date & COLLECTION_TIME <= after_date]

cdc_defn_lact <- merge(cdc_infection, lactate[ORGAN_DYSFUNCTION == TRUE], by="ENCOUNTER_ID", all.x = TRUE, allow.cartesian = TRUE)
cdc_defn_lact <- cdc_defn_lact[COLLECTION_TIME >= before_date & COLLECTION_TIME <= after_date]


# Build the DEFN_cdc data table
DEFN_cdc <- data.frame()
for(str in ls(pattern="^cdc_defn_")) {
  
  dataframe <- eval(parse(text = str))
  data <- dataframe[,.(ENCOUNTER_ID, BC_TIME, ABX_TIME)]
  
  label <- substr(str,10,nchar(str))
  data$ORGAN_DYS <- label
  
  time_variable <- colnames(dataframe)[colnames(dataframe) %in% c("PLACEMENT_INSTANT", "COLLECTION_TIME", "ADMINISTRATION_TIME")]
  data$ORGAN_DYS_TIME <- dataframe[[time_variable]]
  
  DEFN_cdc <- rbind(DEFN_cdc, data)
}

#Add patient MRNs in to DEFN_cdc
DEFN_cdc <- merge(DEFN_cdc, DAT_encounters[,.(ENCOUNTER_ID, PATIENT_MRN)], by="ENCOUNTER_ID", all.x = TRUE)
DEFN_cdc <- DEFN_cdc[,.(ENCOUNTER_ID, PATIENT_MRN, BC_TIME, ABX_TIME, ORGAN_DYS_TIME, ORGAN_DYS)]

#Deduplicate multiple organ dysfunctions of the same type
DEFN_cdc <- DEFN_cdc[order(ENCOUNTER_ID, BC_TIME, ORGAN_DYS_TIME)]
DEFN_cdc <- DEFN_cdc[!duplicated(DEFN_cdc[,c("ENCOUNTER_ID", "BC_TIME", "ORGAN_DYS")])]

DEFN_cdc$DEFN_TIME <- apply(DEFN_cdc[,c("BC_TIME","ABX_TIME","ORGAN_DYS_TIME")], 1, FUN=min) #calculate the time of sepsis based on the minimum of blood culture order, initiation of qualifying antibiotic days, or organ dysfunction
DEFN_cdc$DEFN_TIME <- as.POSIXct(DEFN_cdc$DEFN_TIME, format = "%Y-%m-%d %H:%M:%S")



#### Remove all other variables
rm(list=setdiff(ls(), ls(pattern="^DEFN")))

#save workspace
save.image("P:/dihi_qi/sepsis_outcome_validation/Data/Cleaned/2019_02_24 CDC Defn.RData")

