### Sensitivity and Specificity of Treatment Page to CDC Definition
### Date Created: 3/2/2019
### Date Updated: 4/1/2019
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

#### Load preprocessed data elements
load("P:/dihi_qi/sepsis_outcome_validation/Data/Cleaned/2019_02_24 Cleaned Pilot Data.RData")
load("P:/dihi_qi/sepsis_outcome_validation/Data/Cleaned/2019_02_24 CDC Defn.RData")

#### Subset data to just those patients moved to the treatment page
sw_treatment <- DAT_sw_status[STATUS == 4]
sw_status_3_4 <- DAT_sw_status[STATUS == 3 | STATUS == 4]

#### Identify triggering criteria for CDC Definition in pilot data
DEFN_cdc <- DEFN_cdc[order(ENCOUNTER_ID, DEFN_TIME)]
first_defn <- DEFN_cdc[!duplicated(DEFN_cdc$ENCOUNTER_ID)]
table <- as.data.frame(table(first_defn$ORGAN_DYS))

percentages <- round(table$Freq/length(unique(DEFN_cdc$ENCOUNTER_ID))*100, 1)
labels <- paste(table$Var1, "\n", percentages, "%", sep="")

pie(table$Freq, labels, main="Triggering Criteria for CDC Definition")


#### Sensitivity/Specificity analysis with CDC definition as the gold standard
# N.B. we are using the first time someone met the CDC defininition as the gold standard for time of sepsis

DEFN_cdc <- DEFN_cdc[order(ENCOUNTER_ID, DEFN_TIME, ORGAN_DYS_TIME)] #order the CDC definition properly for this analysis

gold_standard_defn <- DEFN_cdc
evaluation <- sw_treatment

evaluation <- evaluation[order(ENCOUNTER_ID, STATUS_TIME)]

first_gold <- gold_standard_defn[!duplicated(gold_standard_defn$ENCOUNTER_ID)]
first_eval <- evaluation[!duplicated(evaluation$ENCOUNTER_ID)]

eval_before_gold <- merge(first_eval, first_gold, by="ENCOUNTER_ID", all=TRUE)

TP <- eval_before_gold[STATUS_TIME <= DEFN_TIME]
FP <- eval_before_gold[is.na(DEFN_TIME)]
FN <- eval_before_gold[is.na(STATUS_TIME)]

too_late <- eval_before_gold[STATUS_TIME > DEFN_TIME]
too_late$DIFF_TIME <- difftime(too_late$STATUS_TIME, too_late$DEFN_TIME, unit = "hours")

#x <- too_late[STATUS_TIME <= ceiling_date(DEFN_TIME, "days")] #counts any STATUS_TIME on same day as DEFN_TIME as a TP
x <- too_late[DIFF_TIME <= 24] #counts any STATUS_TIME within 24 hours of DEFN_TIME as a TP
y <- too_late[! ENCOUNTER_ID %in% x$ENCOUNTER_ID]

TP <- rbind(TP, x[,1:12])
FN <- rbind(FN, y[,1:12])

# Adjust for Sepsis Watch not running inpatient; so for FN cases, if CDC definition was only met after inpatient admission, count that as a TN
tmp <- merge(FN, DAT_encounters[,.(ENCOUNTER_ID, INPATIENT_ADMIT_TIME)], by="ENCOUNTER_ID", all.x = TRUE)

FN <- tmp[is.na(INPATIENT_ADMIT_TIME) | DEFN_TIME <= INPATIENT_ADMIT_TIME]

# Count up all the TN cases
all_encounters <- DAT_encounters$ENCOUNTER_ID

all_encounters <- union(all_encounters, DEFN_cdc$ENCOUNTER_ID)
#TODO: for some reason, the raw data has ENCOUNTER_ID's in the analytes and meds tables that don't show up in the encounters table

TN <- all_encounters[! all_encounters %in% c(TP$ENCOUNTER_ID, FP$ENCOUNTER_ID, FN$ENCOUNTER_ID)]

tp <- nrow(TP)
fp <- nrow(FP)
fn <- nrow(FN)
tn <- length(TN)

sensitivity <- tp/(tp+fn)
specificity <- tn/(tn+fp)
ppv <- tp/(tp+fp)
npv <- tn/(tn+fn)







#### Print out FP and FN tables
x <- merge(FP, DAT_encounters[,.(ENCOUNTER_ID, PATIENT_MRN)], by="ENCOUNTER_ID", all.x = TRUE)
x <- x[,.(ENCOUNTER_ID, PATIENT_MRN.y, STATUS_TIME, STATUS, REMINDER)]

y <- merge(FN, DAT_encounters[,.(ENCOUNTER_ID, PATIENT_MRN)], by="ENCOUNTER_ID", all.x = TRUE)
y <- y[,.(ENCOUNTER_ID, PATIENT_MRN.y, BC_TIME, ABX_TIME, ORGAN_DYS_TIME, ORGAN_DYS, DEFN_TIME)]


write.csv(x, "C:/Users/all42/Desktop/FP.csv")
write.csv(y, "C:/Users/all42/Desktop/FN.csv")




