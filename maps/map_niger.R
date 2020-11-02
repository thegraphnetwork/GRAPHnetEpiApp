#######################
#
# Map & Transform Data
# raw -> clean
#
# Country:
# niger
#
#######################

# Load utility functions
source("~/data-cleaning/scripts/utils_csv.R")

# --------------------------------------
# initialize
clean_columns = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = map_to('patinfo_ID')
# Anonymized patient ID, numeric

clean_columns[['report_date']] = map_to('report_date')
# Date of notification (line), date, DD/MM/YYYY

clean_columns[['patinfo_first_name']] = keep_empty
clean_columns[['patinfo_last_name']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = map_to('patinfo_ageonset')
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) ifelse(is.na(df[,'patinfo_ageonsetunit'])==T & as.numeric(as.character(df[,'patinfo_ageonset']))==0,0,format(as.numeric(as.character(df[,'patinfo_ageonsetunit']))))  
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric

clean_columns[['patcourse_status']] =  function(df) toupper(ifelse(is.na(df[,'patcourse_status'])
                                                                   , NA 
                                                                   , ifelse(df[, 'patcourse_status'] == 'Décédé'
                                                                            , "Dead"
                                                                            , ifelse(df[, 'patcourse_status'] == "Vivant"
                                                                                     , "Alive"
                                                                                     , "RECOVERED"))))
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = function(df) toupper(df[, 'patinfo_sex'])
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) toupper(df[,'patinfo_resadmin1'])
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,'patinfo_resadmin2'])
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = health_occup('patinfo_occus')
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['expo_travel']] = ouinon2en('expo_travel')
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = travel_country('expo_travel_country')
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = map_to('expo_date_departure')
# Date departed from country visited / Date retuned from country visited, date, DD/MM/YYYY

clean_columns[['pat_symptomatic']] = function(df) ifelse(df[,'patsympt_fever']==1 |df[,'patsympt_sorethroat']==1|df[,'patsympt_cough']==1|df[,'patsympt_runnyNonse']==1|df[,'patsympt_short']==1|df[,'patsympt_other']==1,"Y",ifelse(df[,'patsympt_fever']==0 &df[,'patsympt_sorethroat']==0 & df[,'patsympt_cough']==0 & df[,'patsympt_runnyNonse']==0 & df[,'patsympt_short']==0 & df[,'patsympt_other']==0,"N",NA))
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

#clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = map_to('patcourse_dateonset')
# Date of onset of symptoms,  date, DD/MM/YYYY
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')

clean_columns[['expo_sourcecaseids']] = function(df) paste(df[,'expo_ID1'],df[,'expo_ID2'],df[,'expo_ID3'],df[,'expo_ID4'],sep=",")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = function(df) toupper(ifelse(df[,'report_classif'] == "Confirmé", "Confirmed", "Not a case"))
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = map_to('patcourse_datedeath')
# Date of Death for decesased patients, date, DD/MM/YYYY, NA if Alive

clean_columns[['patinfo_resadmin3']] = map_to('patinfo_resadmin3')
# Place of residence admin level 3 (Health Zone/Town), factor

clean_columns[['patinfo_resadmin4']] = map_to('patinfo_resadmin4')
# Place of residence admin level 4 (Village), factor

clean_columns[['report_orginst']] = map_to('report_orginst')
# Reporting health facility/institution, factor

clean_columns[['patinfo_idadmin1']] = map_to('patinfo_idadmin1')
# Where the case was diagnosed, admin level 1 (Province), factor

clean_columns[['patinfo_idadmin2']] = map_to('patinfo_idadmin2')
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = ouinon2en('report_pointofentry')
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = map_to('report_pointofentry_date')
# Date detected at point of entry, date, DD/MM/YYYY

#clean_columns[['consultation_dateHF']] = function(df) format(as.Date(sub("/", ".", df[,'consultation_dateHF'])),"%d/%m/%Y")
# Date of first consultation at this Health Facility, date, DD/MM/YYYY

clean_columns[['patcourse_admit']] = ouinon2en('patcourse_admit')
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = map_to('patcourse_presHCF')
# For this episode, date first admitted to hospital, date, DD/MM/YYYY

clean_columns[['patcourse_comp']] = map_to('patcourse_comp')
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = ouinon2en('patsympt_fever')
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = ouinon2en('patsympt_sorethroat')
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = ouinon2en('patsympt_cough')
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = ouinon2en('patsympt_runnyNonse')
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = ouinon2en('patsympt_short')
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = map_to('patsympt_other')
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = ouinon2en('Comcond_preexist1')
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = map_to('Comcond_preexist')
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = ouinon2en('expo_visit_healthcare')
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = ouinon2en('expo_ari')
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = map_to('expo_aricontsetting')
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = map_to('expo_other')
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = ouinon2en('expo_contact_case')
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = map_to('expo_ID1')
# ID of confirmed or probable case 1, numeric

clean_columns[['expo_ID2']] = map_to('expo_ID2')
# ID of confirmed or probable case 2, numeric

clean_columns[['expo_ID3']] = map_to('expo_ID3')
# ID of confirmed or probable case 3, numeric

clean_columns[['expo_ID4']] = map_to('expo_ID4')
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = map_to('expo_arisetting')
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = ouinon2en('Lab_coll')
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = test_type('Lab_type')
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL, factor

clean_columns[['Lab_datetaken']] = map_to('Lab_datetaken')
# Date when COVID19 Lab sample was taken, date, MM/DD/YYYY

clean_columns[['Lab_performed']] = function(df) toupper(df[,'Lab_performed']) 
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = pos_neg('Lab_result')
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = map_to('Lab_resdate')
# Date when COVID19 Lab result was returned, date, MM/DD/YYYY

clean_columns[['Lab_other']] = map_to('Lab_other')
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_other_samples_result_list']] = map_to('Lab_otherres')
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = map_to('patcourse_datedischarge')
# Date when patient was discharged (if alive and hospitalized), date, MM/DD/YYYY
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized
