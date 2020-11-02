#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Exemplary Model to Follow
#
#     Country:
#     The Gambia
#
#############################################

# Install pkgs

# Load utility functions
source("~/data-cleaning/notebooks/utils/utils_csv.R")


# --------------------------------------
# initialize
clean_columns = list()

columns_used = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = function(df) as.character(df[, "Unique case ID"])

columns_used = append(columns_used,"Unique case ID")

# Anonymized patient ID, convert to character string (some countries include letters in the ID)

clean_columns[['report_date']] = function(df) lubridate::ymd(df[,'Reporting Date (dd/mm/yyyy)'])
columns_used = append(columns_used,'Reporting Date (dd/mm/yyyy)')
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) ifelse(!is.na(df[,'Age (months)']) & is.na(df[,'Age (yrs)']), 0, as.numeric(df[,'Age (yrs)']))
columns_used = append(columns_used,'Age (yrs)')
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) ifelse(as.numeric(as.character(df[,"Age (yrs)"]))==0| is.na(df[,"Age (yrs)"]), as.numeric(df[,'Age (months)']), NA_real_)  
columns_used = append(columns_used,'Age (months)')
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be


clean_columns[['patcourse_status']] =  function(df) dplyr::case_when(
  is.na(df[,'Date of  death (If died) (dd/mm/yyyy)']) & !is.na(df[,"Date of discharge (if alive or hospitalised)"]) ~ "RECOVERED",
  is.na(df[,"Date of discharge (if alive or hospitalised)"]) & !is.na(df[,"Date of  death (If died) (dd/mm/yyyy)"]) ~ "DEAD",
  is.na(df[, "Date of discharge (if alive or hospitalised)"]) & is.na(df[, "Date of  death (If died) (dd/mm/yyyy)"])~ "ALIVE",
  TRUE ~ NA_character_
  )
columns_used = append(columns_used,"Date of discharge (if alive or hospitalised)")
columns_used = append(columns_used,'Date of  death (If died) (dd/mm/yyyy)')
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = map_to('Gender (M/F)')
columns_used = append(columns_used,'Gender (M/F)')
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) {
  return(toupper(ifelse(grepl("Bank", df[,"Health Region Epi-Classification"]), stringr::str_replace(df[, "Health Region Epi-Classification"], "West", ""),
                       stringr::str_replace_all(df[, "Health Region Epi-Classification"], "Region|\\d", ""))))
         }
columns_used = append(columns_used,"Health Region Epi-Classification")
# Patient residence (province), Standardize names to all uppercase, factor
# Province missing for many lines, fill in from districts

#clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,'patinfo_resadmin2'])
#columns_used = append(columns_used,"patinfo_resadmin2")
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df) ifelse(df[,'Is Patient a healthcare worker?'] == "Yes", "Y", "N")
columns_used = append(columns_used,'Is Patient a healthcare worker?')
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] =  function(df){ 
  raw_column <- 'Patient occupation (specify)'
  return (toupper(df[, raw_column]))
}
  
columns_used = append(columns_used,'Patient occupation (specify)')
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = function(df){
  return(ifelse(df[, "Transmission"] == "Local", "N", "Y"))
}
columns_used = append(columns_used,"Transmission")
# Patient history of travel?, Y/N, factor

#clean_columns[['expo_travel_country']] = map_to('expo_travel_country')
#columns_used = append(columns_used,"expo_travel_country")
# Country(ies) patient travelled to, character string (comma-separated list)

#clean_columns[['expo_date_departure']] = function(df) as.Date(df[,'expo_date_departure'], '%Y-%m-%d')
#columns_used = append(columns_used,"expo_date_departure")
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = function(df){
  ifelse(grepl("Yes", df[,"Is Patient Symptomatic?"], ignore.case = TRUE), "Y", "N")
}
 columns_used = append(columns_used,"Is Patient Symptomatic?")
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

 clean_columns[['pat_contact']] =  function(df){ 
   raw_column <- 'Has the patient had contact with a probable or confirmed cases?'
   return (
     dplyr::case_when(
       df[, raw_column] == "Yes"~ "Y",
       df[, raw_column] == "no" ~ "N",
       TRUE ~ NA_character_
       )
     )
 }
 
 columns_used = append(columns_used,'Has the patient had contact with a probable or confirmed cases?')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = function(df) lubridate::ymd(df[,'Date of onset of first symptoms (dd/mm/yyyy)'])
columns_used = append(columns_used,'Date of onset of first symptoms (dd/mm/yyyy)')
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')
# *** This is currently identical to report_date.

clean_columns[['expo_sourcecaseids']] = function(df) return(stringr::str_remove_all(df[,"ID number of confirmed case linked to"], "[A-Z]|[a-z]"))
columns_used = append(columns_used,'ID number of confirmed case linked to')
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# columns_used = append(columns_used,"patcourse_severity")
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

#clean_columns[['report_classif']] = function(df) ifelse(df[,'Lab_result']=="Pending","RESULTS PENDING",toupper(df[,'report_classif']))
#columns_used = append(columns_used,"report_classif")
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = function(df) as.Date(df[,'Date of  death (If died) (dd/mm/yyyy)'], '%Y-%m-%d')
columns_used = append(columns_used,"Date of  death (If died) (dd/mm/yyyy)")
# Date of Death for decesased patients, character, YYYY-MM-DD, NA if Alive

#clean_columns[['patinfo_resadmin3']] = map_to('patinfo_resadmin3')
#columns_used = append(columns_used,"patinfo_resadmin3")
# Place of residence admin level 3 (Health Zone/Town), factor

clean_columns[['patinfo_resadmin4']] = function(df){ 
  raw_column <- 'Residential Address OR Where case was picked (for PoE/Quarantine)'
  return (toupper(df[, raw_column]))
}
columns_used = append(columns_used,'Residential Address OR Where case was picked (for PoE/Quarantine)')
# Place of residence admin level 4 (Village), factor

clean_columns[['report_orginst']] = function(df){ 
  raw_column <- "Specific Facility/Address Where Case Was Diagnosed OR Quarantine Address"
  return (toupper(df[, raw_column]))
}
columns_used = append(columns_used,"Specific Facility/Address Where Case Was Diagnosed OR Quarantine Address")
# Reporting health facility/institution, factor

clean_columns[['patinfo_idadmin1']] = function(df) {
  return(toupper(ifelse(grepl("Bank", df[,"Health Region Epi-Classification"]), stringr::str_replace(df[, "Health Region Epi-Classification"], "West", ""),
                        stringr::str_replace_all(df[, "Health Region Epi-Classification"], "Region|\\d", ""))))
}
columns_used = append(columns_used,"Health Region Epi-Classification")
# Where the case was diagnosed, admin level 1 (Province), factor

#clean_columns[['patinfo_idadmin2']] = map_to('patinfo_idadmin2')
#columns_used = append(columns_used,"patinfo_idadmin2")
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = function(df){
  return(ifelse(grepl("ENTRY",df[,"Method By Which Case Was Reported?"]), "Y", "N"))
}
columns_used = append(columns_used,"Method By Which Case Was Reported?")
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

#clean_columns[['report_pointofentry_date']] = function(df) as.Date(df[,'report_pointofentry_date'], '%Y-%m-%d')
#columns_used = append(columns_used,"report_pointofentry_date")
# Date detected at point of entry, character, YYYY-MM-DD

#clean_columns[['consultation_dateHF']] = function(df) as.Date(df[,'Date of Admission into treatment centre (dd/mm/yyyy)'], '%Y-%m-%d')
#columns_used = append(columns_used,'Date of Admission into treatment centre (dd/mm/yyyy)')
# Date of first consultation at this Health Facility, character, YYYY-MM-DD

clean_columns[['patcourse_admit']] = function(df){
  return(ifelse(is.na(df[,'Date of Admission into treatment centre (dd/mm/yyyy)']), "N", "Y"))
}
columns_used = append(columns_used,'Date of Admission into treatment centre (dd/mm/yyyy)')
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = function(df) as.Date(df[,'Date of Admission into treatment centre (dd/mm/yyyy)'], '%Y-%m-%d')
columns_used = append(columns_used,'Date of Admission into treatment centre (dd/mm/yyyy)')
# For this episode, date first admitted to hospital, character, YYYY-MM-DD

#clean_columns[['patcourse_comp']] = map_to('patcourse_comp')
#columns_used = append(columns_used,"patcourse_comp")
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = function(df){
  return(ifelse(grepl("Yes",df[,'History of fever/chills'], ignore.case = T), "Y", "N"))
}
columns_used = append(columns_used,'History of fever/chills')
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = function(df){
  return(ifelse(grepl("Yes",df[,'Sore throat'], ignore.case = T), "Y", "N"))
}
columns_used = append(columns_used,"Sore throat")
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] =function(df){
  return(ifelse(grepl("Yes",df[,'Cough'], ignore.case = T), "Y", "N"))
}
columns_used = append(columns_used,"Cough")
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = function(df){
  return(ifelse(grepl("Yes",df[,'Runny nose'], ignore.case = T), "Y", "N"))
}
columns_used = append(columns_used,"Runny nose")
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = function(df){
  return(ifelse(grepl("Yes",df[,'Shortness of breath'], ignore.case = T), "Y", "N"))
}
columns_used = append(columns_used,"Shortness of breath")
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = map_to('Other sign/symptoms, specify')
columns_used = append(columns_used,"Other sign/symptoms, specify")
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = function(df){
  return(ifelse(df[,"Patient pre-existing conditions (specify)"] == "N/A", "N", "Y"))
}
columns_used = append(columns_used,"Patient pre-existing conditions (specify)")
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = function(df){
  return(ifelse(df[,"Patient pre-existing conditions (specify)"] != "N/A", df[,"Patient pre-existing conditions (specify)"], NA_character_))
}
columns_used = append(columns_used,"Patient pre-existing conditions (specify)")
# Patient's pre-existing conditions, character string (comma-separated list)

#clean_columns[['expo_visit_healthcare']] = map_to('expo_visit_healthcare')
#columns_used = append(columns_used,"expo_visit_healthcare")
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

#clean_columns[['expo_ari']] = map_to('expo_ari')
#columns_used = append(columns_used,"expo_ari")
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

#clean_columns[['expo_aricontsetting']] = map_to('expo_aricontsetting')
#columns_used = append(columns_used,"expo_aricontsetting")
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

#clean_columns[['expo_other']] = map_to('expo_other')
#columns_used = append(columns_used,"expo_other")
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = function(df){
  return(
    dplyr::case_when(
      df[,"Has the patient had contact with a probable or confirmed cases?"] == "Yes"~ "Y",
      df[,"Has the patient had contact with a probable or confirmed cases?"] == "no"~ "N",
      TRUE ~ NA_character_
      
    )
  )
}
columns_used = append(columns_used,"Has the patient had contact with a probable or confirmed cases?")
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

#clean_columns[['expo_ID1']] = map_to('expo_ID1')
#columns_used = append(columns_used,"expo_ID1")
# ID of confirmed or probable case 1, numeric

#clean_columns[['expo_ID2']] = map_to('expo_ID2')
#columns_used = append(columns_used,"expo_ID2")
# ID of confirmed or probable case 2, numeric

#clean_columns[['expo_ID3']] = map_to('expo_ID3')
#columns_used = append(columns_used,"expo_ID3")
# ID of confirmed or probable case 3, numeric

#clean_columns[['expo_ID4']] = map_to('expo_ID4')
#columns_used = append(columns_used,"expo_ID4")
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = map_to('Specify close contact setting')
columns_used = append(columns_used,"Specify close contact setting")
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = function(df){
  return("Y")
}
#columns_used = append(columns_used,"Lab_coll")
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = function(df){
  return("NASOPHARYNGEAL SWAB")
}
columns_used = append(columns_used,"Sample type")
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = function(df) as.Date(df[,'Lab Results Date (dd/mm/yyyy)'], '%Y-%m-%d')
columns_used = append(columns_used,"Lab Results Date (dd/mm/yyyy)")
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

clean_columns[['Lab_performed']] = function(df){return("PCR")}
#columns_used = append(columns_used,"Lab_performed")
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df){
  return("POSITIVE")
  }
#columns_used = append(columns_used,"Lab_result")
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = function(df) as.Date(df[,'Lab Results Date (dd/mm/yyyy)'], '%Y-%m-%d')
columns_used = append(columns_used,"Lab Results Date (dd/mm/yyyy)")
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

#clean_columns[['Lab_other']] = map_to('Lab_other')
#columns_used = append(columns_used,"Lab_other")
# Other lab sample(s), character string (comma-separated list)

#clean_columns[['lab_otherres']] = map_to('Lab_otherres')
#columns_used = append(columns_used,"Lab_otherres")
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = function(df) as.Date(df[,'Date of discharge (if alive or hospitalised)'], '%Y-%m-%d')
columns_used = append(columns_used,"Date of discharge (if alive or hospitalised)")
# Date when patient was discharged (if alive and hospitalized), character, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized

# --------------------------------------
# generate colname lists for qualilty control

# remove duplicate column entries in the list of used columns (simply for conciseness)
columns_used = unique(columns_used)

# hardcode columns which are intentionally NOT used
columns_unused = list(colnames(raw)[!colnames(raw) %in% columns_used])

# You can use the following line of code to produce the list of intentionally unused columns
# given that you have a raw dataframe of which you already use all columns you want:
# colnames(raw)[!colnames(raw) %in% columns_used]
# BUT THIS HAS TO BE HARDCODED the unused columns!
# the following code puts this list in a format you can copy to hardcode above:
cat(paste(paste0('"', colnames(raw)[!colnames(raw) %in% columns_used], '"'), collapse = ", "), "\n")
