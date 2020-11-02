#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#
#     Country:
#     tanzania
#
#############################################

# Install pkgs

# Load utility functions
source("~/data-cleaning/notebooks/utils/utils.R")


# --------------------------------------
# initialize
clean_columns = list()

columns_used = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = function(df) format(as.character(df[,"Case ID"]))
# Anonymized patient ID, numeric

clean_columns[['report_date']] = keep_empty

clean_columns[['name_first']] = keep_empty

clean_columns[['name_last']] = keep_empty

clean_columns[['patinfo_ageonset_years']] = function(df) format(as.numeric(as.character(df[,'Age'])))
# Age (yrs)

clean_columns[['patinfo_ageonset_months']] = keep_empty

clean_columns[['patcourse_status']] = keep_empty

clean_columns[['patinfo_sex']] = function(df) toupper(substr(df[,'Sex'], 1, 1))
# Sex (M/F)

clean_columns[['patinfo_resadmin1']] = keep_empty
# Patient residence (province), Standardize names to all uppercase, factor
# Province missing for many lines, fill in from districts

clean_columns[['patinfo_resadmin2']] = keep_empty
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = keep_empty
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = keep_empty
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = keep_empty
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = keep_empty
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = keep_empty
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = keep_empty
# columns_used = append(columns_used,"pat_symptomatic")
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

# clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = keep_empty
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')
# *** This is currently identical to report_date.

clean_columns[['expo_sourcecaseids']] = keep_empty
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# columns_used = append(columns_used,"patcourse_severity")
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = keep_empty
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = keep_empty
# Date of Death for decesased patients, character, YYYY-MM-DD, NA if Alive

clean_columns[['patinfo_resadmin3']] = keep_empty
# Place of residence admin level 3 (Health Zone/Town), factor

clean_columns[['patinfo_resadmin4']] = keep_empty
# Place of residence admin level 4 (Village), factor

clean_columns[['report_orginst']] = keep_empty
# Reporting health facility/institution, factor

clean_columns[['patinfo_idadmin1']] = map_to("Location of client at time of sample collection...9")
# Where the case was diagnosed, admin level 1 (Province), factor

clean_columns[['patinfo_idadmin2']] = map_to("Location of client at time of sample collection...10")
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = function(df) ifelse(df[,'Location of client at time of sample collection...10'] == "Port of Entry"
                                                             , "Y"
                                                             , "N")
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = keep_empty
# Date detected at point of entry, character, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = keep_empty
# Date of first consultation at this Health Facility, character, YYYY-MM-DD

clean_columns[['patcourse_admit']] = function(df) ifelse(is.na(df[,"Date of Hospitalization"])==T
                                            , "N"
                                            , "Y")
# # Admission to hospital?, Y/N, factor


clean_columns[['patcourse_presHCF']] = function(df) as.Date(df[, "Date of Hospitalization"], "%d/%m/%y")
# clean_columns[['report_pointofentry_date']] = function(df) ifelse(is.na(df[,'Date of Hospitalization'])
#                                                                   , NA
#                                                                   , as.Date(df[, "Date of Hospitalization"], "%d/%m/%y"))
# For this episode, date first admitted to hospital, character, YYYY-MM-DD

clean_columns[['patcourse_comp']] = keep_empty
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = keep_empty
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = keep_empty
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = keep_empty
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = keep_empty
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = keep_empty
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = keep_empty
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = keep_empty
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = keep_empty
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = keep_empty
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = keep_empty
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = keep_empty
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = keep_empty
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = keep_empty
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = keep_empty
# ID of confirmed or probable case 1, numeric

clean_columns[['expo_ID2']] = keep_empty
# ID of confirmed or probable case 2, numeric

clean_columns[['expo_ID3']] = keep_empty
# ID of confirmed or probable case 3, numeric

clean_columns[['expo_ID4']] = keep_empty
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = keep_empty
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = keep_empty
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = keep_empty
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = keep_empty
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

clean_columns[['Lab_performed']] = keep_empty
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df) return("POSITIVE")
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

# clean_columns[['Lab_resdate']] = function(df) as.Date(sapply(df[, "Date of confirmation of COVID"], substr, 1, 10), "%d/%m/%y") 
clean_columns[['Lab_resdate']] = function(df) as.Date(sapply(df[, "Date of confirmation of COVID"], substr, 1, 10), "%Y-%m-%d") 
# Lab Results Date (yyyy-mm-dd)
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

clean_columns[['Lab_other']] = keep_empty
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_otherres']] = keep_empty
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = function(df) as.Date(sapply(df[, "Date of Discharge"], substr, 1, 10), "%Y-%m-%d")
# clean_columns[['patcourse_datedischarge']] = function(df) ifelse(is.na(df[,'Date of Discharge'])
#                                                                  , NA
#                                                                  , as.Date(sapply(df[, "Date of Discharge"], substr, 1, 10), "%d/%m/%y"))
# Date of discharge (If alive and hospitalized)






