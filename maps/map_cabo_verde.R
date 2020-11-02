#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Exemplary Model to Follow
#
#     Country:
#     capo_verde
#
#############################################
# first go trough 25.07.2020
# chexk directly about the missing values in the first file to see of normal
# 5 boa vista, only one or problem in function
# no mistakes, lot of empty columns

# Install pkgs
library(lubridate)
# attach(raw)

# Load utility functions
source("~/data-cleaning/notebooks/utils/utils.R")


# --------------------------------------
# initialize
clean_columns = list()

# --------------------------------------
# map variables

clean_columns[['patinfo_ID']] = function(df) as.character(df$patinfo_ID)
# Anonymized patient ID, convert to character string (some countries include letters in the ID)

clean_columns[['report_date']] = multiple_date_types('report_date')
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) {
  return(as.numeric(df$patinfo_ageonset))
}
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) {
  return(df$patinfo_ageonsetunit)
}
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# TODO verify values, bad numbers, shoud convert from the numbers ?
clean_columns[['patcourse_status']] =  function(df) toupper(df$patcourse_status)
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification
# 
clean_columns[['patinfo_sex']] = function(df) return(toupper(df$patinfo_sex))
# # M/F, factor
# 
clean_columns[['patinfo_resadmin1']] = function(df) return(toupper(df[,'patinfo_resadmin1']))
# # Patient residence (province), Standardize names to all uppercase, factor
# 

clean_columns[['patinfo_resadmin2']] = function(df) return(toupper(df[,'patinfo_resadmin2']))
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df) {
  medicalterms <- "medical|health|doctor|physician"
  # any occupations containing medical terms coded as Y
  cleaned <- ifelse(grepl(medicalterms, df$patinfo_occus, ignore.case = T), "Y", df$patinfo_occus)
  # all other filled-in occupations as N. NAs will remain NAs
  cleaned <- ifelse(grepl("[A-Za-z]", df$patinfo_occus) & # filled in
                      !grepl(medicalterms, df$patinfo_occus, ignore.case = T) # but no medical terms
                    ,"N", cleaned)
  return(cleaned)
}
# # Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = function(df) return(df$patinfo_occus)
# # Patient occupation, character string (factor)
# 
clean_columns[['expo_travel']] = yes_no_clean('expo_travel')
# # Patient history of travel?, Y/N, factor
# 
clean_columns[['expo_travel_country']] = function(df) return(df$expo_travel_country)
# # Country(ies) patient travelled to, character string (comma-separated list)
# 
clean_columns[['expo_date_departure']] = 
  # Date departed from country visited / Date retuned from country visited, date, YYYY-MM-DD

  
  clean_columns[['pat_symptomatic']] = function(df) {
    ifelse(df$patsympt_fever== "Yes" | df$patsympt_sorethroat=="Yes"|
             df$patsympt_cough=="Yes"|df$patsympt_runnynose=="Yes"|
             df$patsympt_short=="Yes"| !is.na(df$patsympt_other),"Y", # if any symptoms are marked, return Y
           ifelse(df$patsympt_fever== "No" & df$patsympt_sorethroat=="No" &
                    df$patsympt_cough=="No" & df$patsympt_runnynose=="No" &
                    df$patsympt_short=="No" & is.na(df$patsympt_other),"N", # if no symptoms are marked, return N
                  NA))}
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)


# clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case';
# But it is ambiguous, so prioritize 'expo_contact_case
clean_columns[['patcourse_dateonset']] =  multiple_date_types('patcourse_dateonset')
# Date of onset of symptoms,  date, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')


clean_columns[['expo_sourcecaseids']] = function(df) paste(df$expo_ID1, df$expo_ID2,df$expo_ID3,df$expo_ID4,sep=",")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

clean_columns[['patcourse_severity']] = function(df){return(NA)}
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = function(df) {
  return ( ifelse(df$Lab_result == "Pending", "RESULTS PENDING",toupper(df$report_classif)))
}
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING,
# factor


clean_columns[['patcourse_datedeath']] = function(df) return(as.Date(df$patcourse_datedischarge))
# Date of Death for deceased patients, date, YYYY-MM-DD, NA if Alive


clean_columns[['patinfo_resadmin3']] = function(df){return(NA)}
# Place of residence admin level 3 (Health Zone/Town), factor


clean_columns[['patinfo_resadmin4']] = function(df){return(NA)}
# Place of residence admin level 4 (Village), factor

clean_columns[['report_orginst']] = function(df){return(df$report_orginst)}
# Reporting health facility/institution, factor

clean_columns[['patinfo_idadmin1']] = function(df){return(df$patinfo_idadmin1)}
# Where the case was diagnosed, admin level 1 (Province), factor
# TODO only boa vista, is the same 5 data that have the occupancy of the poatient ?


clean_columns[['patinfo_idadmin2']] = function(df){return(df$patinfo_idadmin2)}
# Where the case was diagnosed, admin level 2 (District), factor


clean_columns[['report_pointofentry']] = yes_no_clean('report_pointofentry')
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor


clean_columns[['report_pointofentry_date']] = multiple_date_types('report_pointofentry_date')
# Date detected at point of entry, date, YYYY-MM-DD


clean_columns[['consultation_dateHF']] =  multiple_date_types('consultation_dateHF')
# Date of first consultation at this Health Facility, date, YYYY-MM-DD

clean_columns[['patcourse_admit']] = yes_no_clean('patcourse_admit')
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = clean_numeric_dates('patcourse_presHCF')


clean_columns[['patcourse_comp']] = function(df){return(df$patcourse_comp)}
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = function(df){
  cleaned <- yes_no_clean('patsympt_fever')(df)
  return(cleaned)
}

# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = function(df){
  cleaned <- yes_no_clean('patsympt_sorethroat')(df)
  cleaned[which(cleaned=="LEKMA")] <- NA
  return(cleaned)
}
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = function(df){
  cleaned <- yes_no_clean('patsympt_cough')(df)
  cleaned[which(cleaned=="LEKMA")] <- NA 
  return(cleaned)
}
# History of cough?, Y/N, factor
clean_columns[['patsympt_runnynose']] = function(df){
  cleaned <- yes_no_clean('patsympt_runnynose')(df)
  cleaned[which(cleaned=="LEKMA")] <- NA 
  return(cleaned)
}
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = function(df){
  cleaned <- yes_no_clean('patsympt_short')(df)
  cleaned[which(cleaned=="LEKMA")] <- NA 
  return(cleaned)
}
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = function(df){
  cleaned <- yes_no_clean('patsympt_other')(df)
  cleaned[which(cleaned %in% c("N/A","NA"))] <- NA
  return(cleaned)
}
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = yes_no_clean('Comcond_preexist1')
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = function(df){return(df$Comcond_preexist)}
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = yes_no_clean('expo_visit_healthcare')
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor
# TODO data empty

clean_columns[['expo_ari']] = yes_no_clean('expo_ari')
# Has patient had close contact with a person with acute respiratory infection
# in the 14 days prior to symptom onset?, Y/N, factor


clean_columns[['expo_aricontsetting']] = function(df){
  cleaned <- yes_no_clean('expo_aricontsetting')(df)
  cleaned[which(cleaned == "N")] <- NA #** No is a nonsensical response here
  return(cleaned)
}
# Setting where the patient had close contact with a person
# with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = function(df){return(df$expo_other)}
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = yes_no_clean('expo_contact_case')
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = function(df){return(df$expo_ID1)}
# ID of confirmed or probable case 1, numeric

clean_columns[['expo_ID2']] = function(df){return(df$expo_ID2)}
# ID of confirmed or probable case 2, numeric

clean_columns[['expo_ID3']] = function(df){return(df$expo_ID3)}
# ID of confirmed or probable case 3, numeric

clean_columns[['expo_ID4']] = function(df){return(df$expo_ID3)}
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = function(df){
  #define search terms
  healthcareterms <- "health|hospital|clinic"
  hometerms <- "family|home|tenant|flat mate|flatmate"
  communityterms <- "community|shop|market"
  schoolterms <- "school|university|class"
  workterms <- "work"
  #recode based on search terms
  cleaned <- df$expo_arisetting
  cleaned <- ifelse(grepl(pattern = healthcareterms, x = cleaned, ignore.case = T), "healthcare", cleaned)
  cleaned <- ifelse(grepl(pattern = hometerms, x = cleaned, ignore.case = T), "home", cleaned)
  cleaned <- ifelse(grepl(pattern = communityterms, x = cleaned, ignore.case = T), "community", cleaned)
  cleaned <- ifelse(grepl(pattern = schoolterms, x = cleaned, ignore.case = T), "school", cleaned)
  cleaned <- ifelse(grepl(pattern = workterms, x = cleaned, ignore.case = T), "work", cleaned)
  # all other filled-in settings will remain as they are. NAs will remain NAs
  return(toupper(cleaned))
}
# Setting where exposure to confirmed or probable case(s) occurred,
# character string (comma-separated list)
#** this column is a mess in Ghana. This is my attempt to make sense of it

clean_columns[['Lab_coll']] = yes_no_clean('Lab_coll')
# COVID19 lab sample collected?, Y/N, factor
# TODO missing No, all na ?

clean_columns[['Lab_type']] = function(df){return(toupper(df$Lab_type))}
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL, factor

clean_columns[['Lab_datetaken']] = multiple_date_types('Lab_datetaken')
# Date when COVID19 Lab sample was taken, date, MM/DD/YYYY

clean_columns[['Lab_performed']] = function(df){return(toupper(df$Lab_performed))}
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df){return(toupper(df$Lab_result))}
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = multiple_date_types('Lab_resdate')
# Date when COVID19 Lab result was returned, date, MM/DD/YYYY

clean_columns[['Lab_other']] = function(df){return(df$Lab_other)}
# Other lab sample(s), character string (comma-separated list)
# TODO data empty


clean_columns[['lab_other_samples_result_list']] = function(df){return(df$lab_other_samples_result_list)}
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = multiple_date_types('patcourse_datedischarge')
# Date when patient was discharged (if alive and hospitalized), date, MM/DD/YYYY
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized
