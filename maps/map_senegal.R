#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Exemplary Model to Follow
#
#     Country:
#     Senegal
#
#############################################

# Install pkgs

# Load utility functions
source("~/data-cleaning/notebooks/utils/utils.R")


# --------------------------------------
# initialize
clean_columns = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = function(df) as.character(df$patinfo_ID)
# Anonymized patient ID, convert to character string (some countries include letters in the ID)

clean_columns[['report_date']] = function(df) as.Date(df[,'report_date'], '%Y-%m-%d')
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) ifelse(is.na(df[,'patinfo_ageonset'])==T & is.na(df[,'patinfo_ageonsetunit'])==F & as.numeric(as.character(df[,'patinfo_ageonsetunit'])) > 23,format(floor(as.numeric(as.character(df[,'patinfo_ageonsetunit']))/12)),format(as.numeric(as.character(df[,'patinfo_ageonset']))))
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) ifelse(is.na(df[,'patinfo_ageonsetunit'])==T & as.numeric(as.character(df[,'patinfo_ageonset']))==0,0,format(as.numeric(as.character(df[,'patinfo_ageonsetunit']))))  
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be

clean_columns[['patcourse_status']] =  function(df) ifelse(tolower(df[,'patcourse_status']) == 'vivant'
                                                           , 'ALIVE'
                                                           , ifelse(tolower(df[,'patcourse_status']) == 'décedé'
                                                                    , 'DEAD'
                                                                    , NA))
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = map_to('patinfo_sex')
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) toupper(df[,'patinfo_resadmin1'])
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,'patinfo_resadmin2'])
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df) ifelse(is.na(df[,'patinfo_occus']),NA,ifelse((df[,'patinfo_occus']=="Healthcare worker")|(df[,'patinfo_occus']=="HCW"),"Y","N"))
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = map_to('patinfo_occus')
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = yes_no_clean('expo_travel')
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = map_to('expo_travel_country')
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = function(df) as.Date(df[,'expo_date_departure'], '%Y-%m-%d')
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = function(df) ifelse(df[,'patsympt_fever']=='Oui' 
                                                         |df[,'patsympt_sorethroat']=='Oui'
                                                         |df[,'patsympt_cough']=='Oui'
                                                         |df[,'patsympt_runnynose']=='Oui'
                                                         |df[,'patsympt_short']=='Oui'
                                                         |df[,'patsympt_other']=='Oui'
                                                         , "Y"
                                                         , ifelse(df[,'patsympt_fever']=='Non' 
                                                                  & df[,'patsympt_sorethroat']=='Non' 
                                                                  & df[,'patsympt_cough']=='Non' 
                                                                  & df[,'patsympt_runnynose']=='Non' 
                                                                  & df[,'patsympt_short']=='Non' 
                                                                  & df[,'patsympt_other']=='Non'
                                                                  , "N"
                                                                  , NA))
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

#clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = function(df) as.Date(df[,'patcourse_dateonset'], '%Y-%m-%d')
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')

clean_columns[['expo_sourcecaseids']] = function(df) gsub("aucun", ""
                                                          , paste(df[,'expo_ID1'],df[,'expo_ID2'],df[,'expo_ID3'],df[,'expo_ID4'],sep=","), fixed=T)
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template
# => anonymisation problem: some contact cass are given by names, not by ID !!!!

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = function(df) ifelse(df[,'Lab_result']=="Pending","RESULTS PENDING",
                                                        ifelse(tolower(df[,'report_classif'])=='confirmé',"CONFIRMED",
                                                               toupper(df[,'report_classif'])))
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = function(df) as.Date(df[,'patcourse_datedeath'], '%Y-%m-%d')
# Date of Death for decesased patients, character, YYYY-MM-DD, NA if Alive

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

clean_columns[['report_pointofentry']] = yes_no_clean('report_pointofentry')
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = clean_numeric_dates('report_pointofentry_date')
# Date detected at point of entry, character, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = function(df) as.Date(df[,'consultation_dateHF'], '%Y-%m-%d')
# Date of first consultation at this Health Facility, character, YYYY-MM-DD

clean_columns[['patcourse_admit']] = yes_no_clean('patcourse_admit')
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = function(df) as.Date(df[,'patcourse_presHCF'], '%Y-%m-%d')
# For this episode, date first admitted to hospital, character, YYYY-MM-DD

clean_columns[['patcourse_comp']] = map_to('patcourse_comp')
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = yes_no_clean('patsympt_fever')
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = yes_no_clean('patsympt_sorethroat')
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = yes_no_clean('patsympt_cough')
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = yes_no_clean('patsympt_runnynose')
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = yes_no_clean('patsympt_short')
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = map_to('patsympt_other')
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = yes_no_clean('Comcond_preexist1')
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = map_to('Comcond_preexist')
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = yes_no_clean('expo_visit_healthcare')
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = yes_no_clean('expo_ari')
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = map_to('expo_aricontsetting')
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = map_to('expo_other')
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = yes_no_clean('expo_contact_case')
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = function(df) ifelse(tolower(df[, 'expo_ID1']) %like% '%sen%'
                                                  , df[, 'expo_ID1']
                                                  , NA)
# ID of confirmed or probable case 1, numeric

clean_columns[['expo_ID2']] = function(df) ifelse(tolower(df[, 'expo_ID2']) %like% '%sen%'
                                                  , df[, 'expo_ID2']
                                                  , NA)
# ID of confirmed or probable case 2, numeric

clean_columns[['expo_ID3']] = function(df) ifelse(tolower(df[, 'expo_ID3']) %like% '%sen%'
                                                  , df[, 'expo_ID3']
                                                  , NA)
# ID of confirmed or probable case 3, numeric

clean_columns[['expo_ID4']] = function(df) ifelse(tolower(df[, 'expo_ID4']) != 'aucun'
                                                  , df[, 'expo_ID4']
                                                  , NA)
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = map_to('expo_arisetting')
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = yes_no_clean('Lab_coll')
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = function(df) toupper(df[,'Lab_type'])
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = function(df) as.Date(df[,'Lab_datetaken'], '%Y-%m-%d')
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

clean_columns[['Lab_performed']] = function(df) toupper(df[,'Lab_performed']) 
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df) ifelse(tolower(df[, 'Lab_result']) == 'positif'
                                                    , "POSITIVE"
                                                    , ifelse(tolower(df[, 'Lab_result']) == 'négatif'
                                                             , "NEGATIVE"
                                                             , NA))
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = clean_numeric_dates('Lab_resdate')
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

clean_columns[['Lab_other']] = function(df) ifelse(df[, 'Lab_other'] != 'aucun'
                                                   , df[, 'Lab_other']
                                                   , NA)
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_otherres']] = function(df) ifelse(df[, 'Lab_otherres'] != 'aucun'
                                                      , df[, 'Lab_otherres']
                                                      , NA)
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = function(df) as.Date(df[,'patcourse_datedischarge'], '%Y-%m-%d')
# Date when patient was discharged (if alive and hospitalized), character, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized
