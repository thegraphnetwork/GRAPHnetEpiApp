#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#
#     Country:
#     Guinea_bissau
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

clean_columns[['patinfo_ID']] = map_to('patinfo_id')
# Anonymized patient ID, numeric

clean_columns[['report_date']] = clean_dates_1('report_date')
# Date of notification (line), date, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) { 
  ageinyears <- ifelse(df$patinfo_ageonsetunit == "MONTHS", 0, NA)
  ageinyears <- ifelse(df$patinfo_ageonsetunit == "YEARS", df$patinfo_ageonset, ageinyears)
  return(ageinyears)
}
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) {
  ageinmonths <- ifelse(df$patinfo_ageonsetunit == "MONTHS", df$patinfo_ageonset, NA)
  return(ageinmonths)
}
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric

clean_columns[['patcourse_status']] =  function(df){ 
  status <- ifelse(df$outcome_patcourse_status_other == "Stable", "ALIVE", df$outcome_patcourse_status_other)
  return(status)
}
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification
# ** It is unclear how Guinea bissau is treating this variable

clean_columns[['patinfo_sex']] = function(df) {
  sexabbrev <- ifelse(df$patinfo_sex == "MALE", "M", df$patinfo_sex )
  sexabbrev <- ifelse(df$patinfo_sex == "FEMALE", "F", sexabbrev )
  return(sexabbrev)
}
# M/F, factor

clean_columns[['patinfo_resadmin1']] = uppercase('Region_residence')
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[['patinfo_resadmin2']] = keep_empty
# Patient residence (district), Standardize names to all uppercase, factor
# ** Guinea bissau appears not to be filling this out

# clean_columns[['patinfo_occus']] = map_to('patinfo_occus')
clean_columns[['patinfo_occus']] = keep_empty
# Patient occupation
#** Keeping this empty for now as it is not clear how they code healthcareworker

clean_columns[['patinfo_occus_specify']] = keep_empty
# Patient occupation, character string (factor)
# ** Nothing about occupation in Guinea bissau's data so far

clean_columns[['expo_travel']] = yes_no_clean('expo_travel')
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = function(df){
  country1 <- ifelse(df$expo_travel_country1 == "N/A", NA, df$expo_travel_country1 ) #**remove anticipated weird N/A text entries
  country2 <- ifelse(df$expo_travel_country2 == "N/A", NA, df$expo_travel_country2 ) #**remove weird N/A text entries
  country3 <- ifelse(df$expo_travel_country3 == "N/A", NA, df$expo_travel_country3 ) #**remove anticipated weird N/A text entries
  countries <- paste(country1, country2, country3, sep = ",")
  return(countries)
}
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = function(df) {
  dates <- paste(df$expo_travel_date1, df$expo_travel_date2, df$expo_travel_date3, sep = ",")
  return(dates)
}
# Date departed from country visited / Date retuned from country visited, date, YYYY-MM-DD
#** Modified to include multiple dates for Guinea Bissau

clean_columns[['pat_symptomatic']] = function(df) { 
  cleaned <- ifelse(grepl("asymp", df$report_test_reason_other, ignore.case = T), "N",df$report_test_reason_other)
  #** all those labelled asymp should be mapped to N. The short form is to dodge the typos they have in there  
  cleaned <- ifelse(grepl("febre|fever|tosse|cough|sore|throat", df$report_test_reason_other, ignore.case = T), "Y", cleaned )
  #** all those with described symptoms should be mapped to Yes. Everyone else, return the vector as is.
    return(cleaned)
}
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)
#** Guinea Bissau is recording a single column that notes either a) the individual's symptoms, or b) "asymptomatic

#clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = clean_dates_1('patcourse_dateonset')
# Date of onset of symptoms,  date, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')


clean_columns[['expo_sourcecaseids']] = keep_empty
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template
# ** It is not clear what Guinea bissau is doing here

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor
# **does not exist for Guinea bissau 

clean_columns[['report_classif']] = keep_empty
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor
# ** Not being recorded for Guinea bissau

clean_columns[['patcourse_datedeath']] = keep_empty
# Date of Death for decesased patients, date, YYYY-MM-DD, NA if Alive
# ** It is not clear what Guinea Bissau is doing here

clean_columns[['patinfo_resadmin3']] = keep_empty
# Place of residence admin level 3 (Health Zone/Town), factor

clean_columns[['patinfo_resadmin4']] = keep_empty
# Place of residence admin level 4 (Village), factor

clean_columns[['report_orginst']] = keep_empty
# Reporting health facility/institution, factor
# ** Not reported by Guinea Bissau

clean_columns[['patinfo_idadmin1']] = keep_empty
# Where the case was diagnosed, admin level 1 (Province), factor
# ** Not reported by Guinea Bissau

clean_columns[['patinfo_idadmin2']] = keep_empty
# Where the case was diagnosed, admin level 2 (District), factor
# ** Not reported by Guinea Bissau

clean_columns[['report_pointofentry']] = keep_empty
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 
# ** Not reported by Guinea Bissau

clean_columns[['report_pointofentry_date']] = keep_empty
# Date detected at point of entry, date, YYYY-MM-DD
# ** Not reported by Guinea Bissau

clean_columns[['consultation_dateHF']] = keep_empty
# Date of first consultation at this Health Facility, date, YYYY-MM-DD
# ** Not reported by Guinea Bissau

clean_columns[['patcourse_admit']] = keep_empty
# Admission to hospital?, Y/N, factor
# ** Not reported by Guinea Bissau

clean_columns[['patcourse_presHCF']] = keep_empty
# For this episode, date first admitted to hospital, character, YYYY-MM-DD
# ** Not reported by Guinea Bissau

clean_columns[['patcourse_comp']] = keep_empty
# Other clinical complications, character string (comma-separated list)
# ** Not reported by Guinea Bissau


clean_columns[['patsympt_fever']] = function(df) { 
  withfever <- grepl("fever|febre", df$report_test_reason_other, ignore.case = T) # all with fever are coded as TRUE
  withfever <- ifelse(withfever == T, "Y", "N") # recode TRUE as "Y", otherwise, "N"
  return(withfever)
}
# History of fever or chills?, Y/N, factor
# ** Guinea Bissau is reporting all symptoms in a single column, "report_test_reason_other"

clean_columns[['patsympt_sorethroat']] = function(df) { 
  withfever <- grepl("sore throat", df$report_test_reason_other, ignore.case = T) # all with sore throat are coded as TRUE
  withfever <- ifelse(withfever == T, "Y", "N") # recode TRUE as "Y", otherwise, "N"
  return(withfever)
}
# History of sore throat?, Y/N, factor
# ** No one has been coded with sore throat so far, so not sure if this will be relevant

clean_columns[['patsympt_cough']] = function(df) { 
  withfever <- grepl("cough|tosse", df$report_test_reason_other, ignore.case = T) # all with cough are coded as TRUE
  withfever <- ifelse(withfever == T, "Y", "N") # recode TRUE as "Y", otherwise, "N"
  return(withfever)
}
# History of cough?, Y/N, factor
# ** Guinea Bissau is reporting all symptoms in a single column, "report_test_reason_other"

clean_columns[['patsympt_runnynose']] = keep_empty
# History of runny nose?, Y/N, factor
# ** Guinea Bissau is really only reporting fever and cough


clean_columns[['patsympt_short']] = keep_empty
# History of shortness of breath?, Y/N, factor
# ** Guinea Bissau is really only reporting fever and cough

clean_columns[['patsympt_other']] = keep_empty
# Other signs or symptoms, character string (comma-separated list)
# ** Guinea Bissau is really only reporting fever and cough

clean_columns[['Comcond_preexist1']] = keep_empty
# Patient has pre-existing conditions?, Y/N, factor
# ** Unclear with Guinea Bissau's file


clean_columns[['Comcond_preexist']] = keep_empty
# Patient's pre-existing conditions, character string (comma-separated list)
# ** Unclear with Guinea Bissau's file

clean_columns[['expo_visit_healthcare']] = keep_empty
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor
# ** Unclear with Guinea Bissau's file

clean_columns[['expo_ari']] =keep_empty
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor
# ** Unclear with Guinea Bissau's file


clean_columns[['expo_aricontsetting']] = keep_empty
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor
# ** Unclear with Guinea Bissau's file. Perhaps could be manually extracted from report_test_reason_other


clean_columns[['expo_other']] =  keep_empty
# Other exposures, character string (comma-separated list)
# ** Unclear with Guinea Bissau's file. Perhaps could be manually extracted from report_test_reason_other

clean_columns[['expo_contact_case']] = function(df) return(ifelse(df$report_test_reason == "Contact", "Y","N" ) )
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = keep_empty
# ID of confirmed or probable case 1, numeric
#** It is unclear what Guinea Bissau is doing here

clean_columns[['expo_ID2']] = keep_empty
# ID of confirmed or probable case 2, numeric
#** It is unclear what Guinea Bissau is doing here

clean_columns[['expo_ID3']] = keep_empty
# ID of confirmed or probable case 3, numeric
#** It is unclear what Guinea Bissau is doing here

clean_columns[['expo_ID4']] = keep_empty
# ID of confirmed or probable case 4, numeric
#** It is unclear what Guinea Bissau is doing here

clean_columns[['expo_arisetting']] = keep_empty
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)
#** Not being recorded for Guinea Bissau

clean_columns[['Lab_coll']] = keep_empty
# COVID19 lab sample collected?, Y/N, factor
#** Not being recorded for Guinea Bissau

clean_columns[['Lab_type']] = keep_empty
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL, factor
#** Not being recorded for Guinea Bissau

clean_columns[['Lab_datetaken']] = clean_dates_1('lab_date1')
# Date when COVID19 Lab sample was taken, date, YYYY-MM-DD
#** This is assumed. It is unclear whether the date provided is the date taken or date returned

clean_columns[['Lab_performed']] = keep_empty
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor
#** Not being recorded for Guinea Bissau

clean_columns[['Lab_result']] =keep_empty
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor
# ** NA for Guinea Bissau

clean_columns[['Lab_resdate']] = keep_empty
# Date when COVID19 Lab result was returned, date, YYYY-MM-DD
# ** NA for Guinea Bissau

clean_columns[['Lab_other']] = keep_empty
# Other lab sample(s), character string (comma-separated list)
# ** NA for Guinea Bissau

clean_columns[['lab_other_samples_result_list']] = keep_empty
# Other lab sample result(s), character string (comma-separated list)
# ** NA for Guinea Bissau

clean_columns[['patcourse_datedischarge']] = keep_empty
# Date when patient was discharged (if alive and hospitalized), date, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized

# NOTES: It is unclear what the following variables mean: patcourse_dateiso, outcome_submitted_date



#############################################
# Quality checking data
#############################################


# for(var in 2:length(names(clean))){
#   print(names(clean)[var])
#   print(table(as.factor(as.character(clean[,var])),useNA="always"))
# }
