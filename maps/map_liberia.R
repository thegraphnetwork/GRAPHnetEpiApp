#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Country:
#     liberia
#
#############################################

# Install pkgs

# Load utility functions
source("~/data-cleaning/notebooks/utils/utils.R")

# clean_date = function(raw_column){
#   return(function(df) ifelse(is.null(as.Date(df[,raw_column], '%d.%m.%Y'))
#                              , NA
#                              , format(as.Date(df[,raw_column], '%d.%m.%Y'), '%d/%m/%Y')))
# }
# --------------------------------------
# initialize
clean_columns = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = function(df) as.character(df[, 'Case ID'])
# Anonymized patient ID, convert to character string (some countries include letters in the ID)

clean_columns[['report_date']] = clean_numeric_dates('Reporting Date')
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) ifelse(df[, "Age Type"] == "Months"
                                                                , 0
                                                                , ifelse(df[, "Age Type"] == "Years" | df[, "Age Type"] == "NP"
                                                                         , as.numeric(df[,'Age'])
                                                                         , NA))
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) ifelse(df[, "Age Type"] == "Months"
                                                                 , as.numeric(df[,'Age'])
                                                                 , ifelse(df[, "Age Type"] == "Years" | df[, "Age Type"] == "NP"
                                                                          , 0
                                                                          , NA))  
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be

clean_columns[['patcourse_status']] =  function(df) toupper(df[,'Current Status'])
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = function(df) ifelse(df[, "Sex at Birth"] == "Male"
                                                     , "M"
                                                     , "F")
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) toupper(df[,'County of Residence'])
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,'District of Residence'])
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df) ifelse(is.na(df[,'Health care worker']) | df[,'Health care worker'] == "N/A"
                                                       , NA
                                                       , ifelse(df[,'Health care worker'] == "No"
                                                               , "N"
                                                               , "Y"))
# Patient occupation is healthcare worker?, Y/N, factor

# Code from previous linelist
# clean_columns[['patinfo_occus_specify']] = function(df) ifelse(df[, "Other Occupation, specify"] == "N/A"
#                                                                , NA
                                                               # , toupper(df[, "Other Occupation, specify"]))
clean_columns[['patinfo_occus_specify']] = yes_no_clean("Other, specify...86")
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = yes_no_clean('Has the patient travelled in the 14 days prior to symptom onset')
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = function(df) ifelse(df[, 'Has the patient travelled in the 14 days prior to symptom onset?'] == 'Yes'
                                                             , df[, 'Specify country travelled to 1']
                                                             , NA)
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = keep_empty
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = yes_no_clean('Patient Asymptomatic')
# Does the patient present with current or past history of symptoms?, Y/N, factor

clean_columns[['pat_contact']] = map_to('Has the patient had contact with a probable or confirmed cases?')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = clean_numeric_dates('Date of symptoms onset')
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')

clean_columns[['expo_sourcecaseids']] = function(df) paste(df[,'ID number of confirmed or probable case 1'], df[,'ID number of confimred or probable case 2']
                                                           , df[,'ID number of confimred or probable case 3'], df[,'ID number of confimred or probable case 4'], sep=",")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

clean_columns[['patcourse_severity']] = keep_empty
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

# Code from previous file
# clean_columns[['report_classif']] = function(df) ifelse(df[,'1st Test Result']=="Pending","RESULTS PENDING",toupper(df[,'Final Classification']))
clean_columns[['report_classif']] = uppercase('Final Classification')

# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = clean_numeric_dates('Date of Death')
# Date of Death for decesased patients, character, YYYY-MM-DD, NA if Alive

clean_columns[['patinfo_resadmin3']] = map_to('Zone of Residence')
# Place of residence admin level 3 (Health Zone/Town), factor

clean_columns[['patinfo_resadmin4']] = map_to('Community of Residence')
# Place of residence admin level 4 (Village), factor

# Code from previous file
#clean_columns[['report_orginst']] = map_to('Reporting Health Facility')
clean_columns[['report_orginst']] = map_to('Health Facility')
# Reporting health facility/institution, factor

clean_columns[['patinfo_idadmin1']] = map_to('Reporting County')
# Where the case was diagnosed, admin level 1 (Province), factor

clean_columns[['patinfo_idadmin2']] = map_to('Reporting District')
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = keep_empty
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = keep_empty
# Date detected at point of entry, character, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = clean_numeric_dates('Date seen at Reporting Health Facility')
# Date of first consultation at this Health Facility, character, YYYY-MM-DD

clean_columns[['patcourse_admit']] = yes_no_clean('Admitted to Hospital')
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = clean_numeric_dates('Date 1st Admitted in Hospital')
# For this episode, date first admitted to hospital, character, YYYY-MM-DD

# Code from previous file
# clean_columns[['patcourse_comp']] = map_to('Other, specify')
clean_columns[['patcourse_comp']] = map_to('Other, specify...59')
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = map_to('History of fever/chills')
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = map_to('Sore throat')
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = map_to('Cough')
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = map_to('Runny nose')
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = map_to('Shortness of breath')
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = map_to('Other signs, specify')
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = keep_empty
# Patient has pre-existing conditions?, Y/N, factor

# Code from previous file
# clean_columns[['Comcond_preexist']] = map_to('Other, specify')
clean_columns[['Comcond_preexist']] = map_to('Other, specify...81')
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = yes_no_clean('Has the patient visited any health care facilities in the 14 days prior to symptom onset')
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = yes_no_clean('Has the patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset')
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = keep_empty
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = map_to('Likely location/city/country for exposure to case?')
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = map_to('Has the patient had contact with a probable or confirmed cases?')
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = map_to('ID number of confirmed or probable case 1')
# ID of confirmed or probable case 1, numeric

clean_columns[['expo_ID2']] = map_to('ID number of confimred or probable case 2')
# ID of confirmed or probable case 2, numeric

clean_columns[['expo_ID3']] = map_to('ID number of confimred or probable case 3')
# ID of confirmed or probable case 3, numeric

clean_columns[['expo_ID4']] = map_to('ID number of confimred or probable case 4')
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = keep_empty
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = function(df) ifelse(is.na(clean_numeric_dates('Date of 1st specimen collection'))
                                                  , NA
                                                  , "YES")
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = keep_empty
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL, factor

clean_columns[['Lab_datetaken']] = clean_numeric_dates('Date of 1st specimen collection')
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

clean_columns[['Lab_performed']] = keep_empty
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

# Previous code
# clean_columns[['Lab_result']] = function(df) toupper(df[,'1st Test Result'])
clean_columns[['Lab_result']] = keep_empty
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = clean_numeric_dates('Date 1st result received from the Lab')
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

clean_columns[['Lab_other']] = keep_empty
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_other_samples_result_list']] = keep_empty
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = clean_numeric_dates('Date of Discharge')
# Date when patient was discharged (if alive and hospitalized), character, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized
