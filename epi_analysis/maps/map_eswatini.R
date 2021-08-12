#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Country:
#     eswatini
#
#############################################

# Install pkgs

# Load utility functions
source("~/data-cleaning/notebooks/utils/utils.R")

# Function to change litteral N/A to NA
clean_na = function(raw_column){
  return (function(df) toupper(ifelse(is.na(df[, raw_column]), NA
                                      , ifelse(tolower(df[, raw_column]) == "n/a"
                                               , NA
                                               , df[, raw_column]))))
}

# --------------------------------------
# initialize
clean_columns = list()

columns_used = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = function(df) ifelse(is.na(as.numeric(df[, 'patinfo_ID']))
                                       , NA
                                       , as.numeric(df[, 'patinfo_ID']))
columns_used = append(columns_used,"patinfo_ID")
# Anonymized patient ID, numeric

clean_columns[['report_date']] = map_to('report_date')
columns_used = append(columns_used,"report_date")

clean_columns[['patinfo_first_name']] = keep_empty
clean_columns[['patinfo_last_name']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) ifelse(is.na(as.numeric(df[, 'patinfo_ageonset']))
                                                                , ifelse(!is.na(as.numeric(df[, 'patinfo_sex']))
                                                                         , df[, 'patinfo_sex'], NA)
                                                                , df[, 'patinfo_ageonset'])
columns_used = append(columns_used,"patinfo_ageonset")
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) ifelse(is.na(df[,'patinfo_ageonsetunit'])==T & as.numeric(as.character(df[,'patinfo_ageonset'])) == 0
                                                                 , 0, format(as.numeric(as.character(df[,'patinfo_ageonsetunit']))))  
columns_used = append(columns_used,"patinfo_ageonsetunit")
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be

clean_columns[['patcourse_status']] = function(df) ifelse(is.na(df[, 'Column2'])
                                             , toupper(df[, 'patcourse_status'])
                                             , ifelse(grepl("disc", tolower(df[, 'Column2']), fixed = T)
                                                      , 'RECOVERED'
                                                      , ifelse(tolower(df[, 'Column2']) == 'dead' | !is.na(df[, 'patcourse_datedeath'])
                                                               , "DEAD"
                                                               , toupper(df[, 'patcourse_status']))))
columns_used = append(columns_used,"patcourse_status")
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = function(df) toupper(ifelse(is.na(as.numeric(df[, 'patinfo_sex']))
                                                     , df[, 'patinfo_sex']
                                                     , ifelse(is.na(as.numeric(df[, 'patinfo_ageonset']))
                                                                        , df[, 'patinfo_ageonset']
                                                                        , NA)))
columns_used = append(columns_used,"patinfo_sex")
# M/F, factor


clean_columns[['patinfo_resadmin1']] = uppercase('patinfo_resadmin1')
columns_used = append(columns_used,"patinfo_resadmin1")
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[['patinfo_resadmin2']] = uppercase('patinfo_resadmin2')
columns_used = append(columns_used,"patinfo_resadmin2")
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = yes_no_clean('patinfo_occus')
columns_used = append(columns_used,"patinfo_occus")
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = keep_empty
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = yes_no_clean('expo_travel')
columns_used = append(columns_used,"expo_travel")
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = map_to('expo_travel_country')
columns_used = append(columns_used,"expo_travel_country")
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = ifelse(is.null(multiple_date_types('expo_date_departure')) | is.na(multiple_date_types('expo_date_departure'))
                                                , NA
                                                , multiple_date_types('expo_date_departure'))
columns_used = append(columns_used,"expo_date_departure")
# Date departed from country visited / Date retuned from country visited, date, DD/MM/YYYY
# Getting errors from it

clean_columns[['pat_symptomatic']] = function(df) toupper(ifelse(grepl('yes', tolower(df[, 'pat_symptomatic']), fixed = T) 
                                                                | grepl('dis', tolower(df[, 'pat_symptomatic']), fixed = T)
                                                                , 'y'
                                                                , ifelse(grepl('asym', tolower(df[, 'pat_symptomatic']), fixed = T)
                                                                         | grepl('no', tolower(df[, 'pat_symptomatic']), fixed = T)
                                                                         , 'n'
                                                                         , NA)))
columns_used = append(columns_used,"pat_symptomatic")
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

clean_columns[['pat_contact']] = yes_no_clean('pat_contact')
columns_used = append(columns_used,"pat_contact")
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = ifelse(is.null(multiple_date_types('patcourse_dateonset')) | is.na(multiple_date_types('patcourse_dateonset'))
                                                , NA
                                                , multiple_date_types('patcourse_dateonset'))
columns_used = append(columns_used,"patcourse_dateonset")
# Date of onset of symptoms,  date, DD/MM/YYYY
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')

clean_columns[['expo_sourcecaseids']] = function(df) paste(df[,'expo_ID1'],df[,'expo_ID2'],df[,'expo_ID3'],df[,'expo_ID4'],sep=",")
columns_used = append(columns_used,"expo_ID1")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

clean_columns[['patcourse_severity']] = function(df) toupper(ifelse(grepl('mild', tolower(df[, 'patcourse_severity']), fixed = T)
                                                                    , 'mild'
                                                                    , ifelse(grepl('mod', tolower(df[, 'patcourse_severity']), fixed = T)
                                                                             , 'moderate'
                                                                             , ifelse(grepl('sev', tolower(df[, 'patcourse_severity']), fixed = T)
                                                                                      , 'severe'
                                                                                      , ifelse(grepl('crit', tolower(df[, 'patcourse_severity']), fixed = T)
                                                                                               , 'critical'
                                                                                               , NA)))))
columns_used = append(columns_used,"patcourse_severity")
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = uppercase('report_classif')
columns_used = append(columns_used,"report_classif")
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = multiple_date_types('patcourse_datedeath')
columns_used = append(columns_used,"patcourse_datedeath")
# Date of Death for decesased patients, date, DD/MM/YYYY, NA if Alive

clean_columns[['patinfo_resadmin3']] = map_to('patinfo_resadmin3')
columns_used = append(columns_used,"patinfo_resadmin3")
# Place of residence admin level 3 (Health Zone/Town), factor

clean_columns[['patinfo_resadmin4']] = map_to('patinfo_resadmin4')
columns_used = append(columns_used,"patinfo_resadmin4")
# Place of residence admin level 4 (Village), factor

clean_columns[['report_orginst']] = map_to('report_orginst')
columns_used = append(columns_used,"report_orginst")
# Reporting health facility/institution, factor

clean_columns[['patinfo_idadmin1']] = map_to('patinfo_idadmin1')
columns_used = append(columns_used,"patinfo_idadmin1")
# Where the case was diagnosed, admin level 1 (Province), factor

clean_columns[['patinfo_idadmin2']] = map_to('patinfo_idadmin2')
columns_used = append(columns_used,"patinfo_idadmin2")
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = yes_no_clean('report_pointofentry')
columns_used = append(columns_used,"report_pointofentry")
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = clean_na('report_pointofentry_date')
columns_used = append(columns_used,"report_pointofentry_date")
# Date detected at point of entry, date, DD/MM/YYYY

clean_columns[['consultation_dateHF']] = multiple_date_types('consultation_dateHF')
columns_used = append(columns_used,"consultation_dateHF")
# Date of first consultation at this Health Facility, date, DD/MM/YYYY

clean_columns[['patcourse_admit']] = function(df) toupper(ifelse(is.na(df[, 'patcourse_admit'])
                                                         , NA
                                                         , ifelse(tolower(df[, 'patcourse_admit']) == 'yes'
                                                                  | grepl('adm', tolower(df[, 'patcourse_admit']), fixed = T)
                                                                  , 'y'
                                                                  , 'n')))
columns_used = append(columns_used,"patcourse_admit")
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = ifelse(is.null(multiple_date_types('patcourse_presHCF')) | is.na(multiple_date_types('patcourse_presHCF'))
                                              , NA
                                              , multiple_date_types('patcourse_presHCF'))
columns_used = append(columns_used,"patcourse_presHCF")
# For this episode, date first admitted to hospital, date, DD/MM/YYYY

clean_columns[['patcourse_comp']] = map_to('patcourse_comp')
columns_used = append(columns_used,"patcourse_comp")
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = function(df) toupper(ifelse(is.na(df[, 'patsympt_fever']) 
                                                                | tolower(df[, 'patsympt_fever']) == 'n/a'
                                                                | grepl('kno', tolower(df[, 'patsympt_fever']), fixed = T)
                                                                , NA
                                                                , ifelse(tolower(df[, 'patsympt_fever']) == 'no'
                                                                         , 'n'
                                                                         , ifelse(tolower(df[, 'patsympt_fever']) == 'yes'
                                                                                  , 'y'
                                                                                  , NA))))
columns_used = append(columns_used,"patsympt_fever")
                                                                
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = function(df) toupper(ifelse(is.na(df[, 'patsympt_sorethroat']) 
                                                                     | tolower(df[, 'patsympt_sorethroat']) == 'n/a'
                                                                     | grepl('kno', tolower(df[, 'patsympt_sorethroat']), fixed = T)
                                                                     , NA
                                                                     , ifelse(tolower(df[, 'patsympt_sorethroat']) == 'no'
                                                                              , 'n'
                                                                              , ifelse(tolower(df[, 'patsympt_sorethroat']) == 'yes'
                                                                                      , 'y'
                                                                                      , NA))))
columns_used = append(columns_used,"patsympt_sorethroat")
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = function(df) toupper(ifelse(is.na(df[, 'patsympt_cough']) 
                                                                | tolower(df[, 'patsympt_cough']) == 'n/a'
                                                                | grepl('kno', tolower(df[, 'patsympt_cough']), fixed = T)
                                                                , NA
                                                                , ifelse(tolower(df[, 'patsympt_cough']) == 'no'
                                                                        , 'n'
                                                                        , ifelse(tolower(df[, 'patsympt_cough']) == 'yes'
                                                                                 , 'y'
                                                                                 , NA))))
columns_used = append(columns_used,"patsympt_cough")
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = function(df) toupper(ifelse(is.na(df[, 'patsympt_runnynose']) 
                                                                    | tolower(df[, 'patsympt_runnynose']) == 'n/a'
                                                                    | grepl('kno', tolower(df[, 'patsympt_runnynose']), fixed = T)
                                                                    , NA
                                                                    , ifelse(tolower(df[, 'patsympt_runnynose']) == 'no'
                                                                             , 'n'
                                                                             , ifelse(tolower(df[, 'patsympt_runnynose']) == 'yes'
                                                                                      , 'y'
                                                                                      , NA))))
columns_used = append(columns_used,"patsympt_runnynose")
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = function(df) toupper(ifelse(is.na(df[, 'patsympt_short']) 
                                                                | tolower(df[, 'patsympt_short']) == 'n/a'
                                                                | grepl('kno', tolower(df[, 'patsympt_short']), fixed = T)
                                                                , NA
                                                                , ifelse(tolower(df[, 'patsympt_short']) == 'no'
                                                                         , 'n'
                                                                         , ifelse(tolower(df[, 'patsympt_short']) == 'yes'
                                                                                  , 'y'
                                                                                  , NA))))
columns_used = append(columns_used,"patsympt_short")
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = yes_no_clean('patsympt_other')
columns_used = append(columns_used,"patsympt_other")
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = function(df) toupper(ifelse(is.na(df[, 'Comcond_preexist1']) 
                                                                   | grepl('kno', tolower(df[, 'Comcond_preexist1']), fixed = T)
                                                                   , NA
                                                                   , ifelse(tolower(df[, 'Comcond_preexist1']) == 'no'
                                                                            | tolower(df[, 'Comcond_preexist1']) == 'none' 
                                                                            , 'n'
                                                                            , ifelse(tolower(df[, 'Comcond_preexist1']) == 'yes'
                                                                                     , 'y'
                                                                                     , NA))))
columns_used = append(columns_used,"Comcond_preexist1")
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = function(df) ifelse(tolower(df[, 'Comcond_preexist1']) == 'yes'
                                                          , toupper(df[, 'Comcond_preexist'])
                                                          , NA)
columns_used = append(columns_used,"Comcond_preexist")
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = yes_no_clean('expo_visit_healthcare')
columns_used = append(columns_used,"expo_visit_healthcare")
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = yes_no_clean('expo_ari')
columns_used = append(columns_used,"expo_ari")
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = clean_na('expo_aricontsetting')
columns_used = append(columns_used,"expo_aricontsetting")
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = map_to('expo_other')
columns_used = append(columns_used,"expo_other")
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = yes_no_clean('expo_contact_case')
columns_used = append(columns_used,"expo_contact_case")
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = clean_na('expo_ID1')
columns_used = append(columns_used,"expo_ID1")
# ID of confirmed or probable case 1, numeric

clean_columns[['expo_ID2']] = clean_na('expo_ID2')
columns_used = append(columns_used,"expo_ID2")
# ID of confirmed or probable case 2, numeric

clean_columns[['expo_ID3']] = clean_na('expo_ID3')
columns_used = append(columns_used,"expo_ID3")
# ID of confirmed or probable case 3, numeric

clean_columns[['expo_ID4']] = clean_na('expo_ID4')
columns_used = append(columns_used,"expo_ID4")
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = clean_na('expo_arisetting')
columns_used = append(columns_used,"expo_arisetting")
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = yes_no_clean('Lab_coll')
columns_used = append(columns_used,"Lab_coll")
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = uppercase('Lab_type')
columns_used = append(columns_used,"Lab_type")
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL, factor

clean_columns[['Lab_datetaken']] = multiple_date_types('Lab_datetaken')
columns_used = append(columns_used,"Lab_datetaken")
# Date when COVID19 Lab sample was taken, date, MM/DD/YYYY

clean_columns[['Lab_performed']] = uppercase('Lab_performed')
columns_used = append(columns_used,"Lab_performed")
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df) toupper(ifelse(is.na(df[, 'Lab_result'])
                                                            , NA
                                                            , ifelse(grepl('pos', tolower(df[, 'Lab_result']), fixed = T)
                                                                     , 'positive'
                                                                     , ifelse(grepl('neg', tolower(df[, 'Lab_result']), fixed = T)
                                                                              , 'negative'
                                                                              , NA))))
columns_used = append(columns_used,"Lab_result")
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = multiple_date_types('Lab_resdate')
columns_used = append(columns_used,"Lab_resdate")
# Date when COVID19 Lab result was returned, date, MM/DD/YYYY

clean_columns[['Lab_other']] = map_to('Lab_other')
columns_used = append(columns_used,"Lab_other")
# Other lab sample(s), character string (comma-separated list)

clean_columns[['Lab_otherres']] = function(df) toupper(ifelse(is.na(df[, 'Lab_otherres']) | tolower(df[, 'Lab_otherres']) == 'none'
                                                                               , NA
                                                                               , ifelse(grepl('pos', tolower(df[, 'Lab_otherres']), fixed = T)
                                                                                        , 'positive'
                                                                                        , ifelse(grepl('neg', tolower(df[, 'Lab_otherres']), fixed = T)
                                                                                                 , 'negative'
                                                                                                 , NA))))
columns_used = append(columns_used,"Lab_otherres")
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = keep_empty
# Date when patient was discharged (if alive and hospitalized), date, MM/DD/YYYY
# The only viable information we have here is the date of death. So we map this column to date of death
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized

# --------------------------------------
# generate colname lists for qualilty control

# remove duplicate column entries in the list of used columns (simply for conciseness)
columns_used = unique(columns_used)

# hardcode columns which are intentionally NOT used
columns_unused = list('Column8', 'patcourse_datedischarge')

# You can use the following line of code to produce the list of intentionally unused columns
# given that you have a raw dataframe of which you already use all columns you want:
# colnames(raw)[!colnames(raw) %in% columns_used]
# BUT THIS HAS TO BE HARDCODED the unused columns!
# the following code puts this list in a format you can copy to hardcode above:
# cat(paste(paste0('"', colnames(raw)[!colnames(raw) %in% columns_used], '"'), collapse = ", "), "\n")
