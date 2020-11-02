#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Country:
#     nigeria
#
#############################################

# Install pkgs

# Load utility functions
source("~/data-cleaning/notebooks/utils/utils.R")

# Female/Male to F/M
sex_2_mf = function(raw_column) {
  return(function(df) ifelse(tolower(df[, raw_column]) == 'male'
                             , 'M'
                             , ifelse(tolower(df[, raw_column]) == 'female'
                                      , 'F'
                                      , NA)))
}


# --------------------------------------
# initialize
clean_columns = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = function(df) as.character(df$Cases)
# Anonymized patient ID, convert to character string (some countries include letters in the ID)

clean_columns[['report_date']] = clean_dates_1('Date_reported')
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) ifelse(format(as.numeric(as.character(df[,'Age']))) < 1
                                                                , 0
                                                                , format(as.numeric(as.character(df[,'Age']))))
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = keep_empty
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be

clean_columns[['patcourse_status']] =  function(df) ifelse(tolower(df[, 'Current Status']) == 'died'
                                                           , 'DEAD'
                                                           , ifelse(tolower(df[, 'Current Status']) == 'unknown'
                                                                    , 'UNKNOWN'
                                                                    , 'ALIVE'))
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED/UNKNOWN, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = sex_2_mf('Sex')
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) toupper(df[,'State'])
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[['patinfo_resadmin2']] = keep_empty
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = yes_no_clean('HealthcareWorker')
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = keep_empty
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = yes_no_clean('Travel History')
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = map_to('Brief travel history')
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = keep_empty
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = keep_empty
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

clean_columns[['pat_contact']] = yes_no_clean('IsCaseAContact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = keep_empty
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')

clean_columns[['expo_sourcecaseids']] = keep_empty
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

clean_columns[['patcourse_severity']] = keep_empty
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

clean_columns[['patinfo_idadmin1']] = keep_empty
# Where the case was diagnosed, admin level 1 (Province), factor

clean_columns[['patinfo_idadmin2']] = keep_empty
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = keep_empty
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = keep_empty
# Date detected at point of entry, character, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = keep_empty
# Date of first consultation at this Health Facility, character, YYYY-MM-DD

clean_columns[['patcourse_admit']] = function(df) ifelse(tolower(df[, 'Initial Status']) %like% '%admi%' 
                                                         |tolower(df[, 'Initial Status']) %like% '%treat%'
                                                         , 'YES'
                                                         , ifelse(tolower(df[, 'Initial Status']) %like% '%unknown%'
                                                                  , 'UNKNOWN'
                                                                  , 'NO'))
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = keep_empty
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

clean_columns[['expo_contact_case']] = yes_no_clean('IsCaseAContact')
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

#IAMHERE
clean_columns[['Lab_coll']] = function(df) ifelse(!is.na(df[, 'Date confirmed'])
                                                  , 'YES'
                                                  , NA)
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = keep_empty
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = keep_empty
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

clean_columns[['Lab_performed']] = keep_empty
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df) ifelse(!is.na(df[, 'Date confirmed'])
                                                    , 'POSITIVE'
                                                    , NA)
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = clean_dates_1('Date confirmed')
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

clean_columns[['Lab_other']] = keep_empty
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_otherres']] = keep_empty
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = keep_empty
# Date when patient was discharged (if alive and hospitalized), character, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized
