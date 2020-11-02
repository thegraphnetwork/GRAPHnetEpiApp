#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#
#     Country:
#     Mauritius
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

clean_columns[['patinfo_ID']] = map_to('patinfo_ID')
# Anonymized patient ID, numeric

clean_columns[['report_date']] = function(df) {
  cleaned <- ifelse(grepl("[A-Za-z]", df$report_date) == T, NA, df$report_date) # replace text columns with NA
  cleaned <- as.Date(as.numeric(cleaned), origin = "1899-12-30") # convert numeric dates to standard format
  return(cleaned)
}
# Date of notification (line), date, YYYY-MM-DD
# ** There are some annoying cells with text in this column at the bottom of mauritius' xlsx file.


clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = map_to('patinfo_ageonset')
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) ifelse(is.na(df$patinfo_ageonsetunit) & as.numeric(as.character(df$patinfo_ageonset))==0,0,format(as.numeric(as.character(df$patinfo_ageonsetunit))))  
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be

clean_columns[['patcourse_status']] =  uppercase('patcourse_status')
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = map_to('patinfo_sex')
# M/F, factor

clean_columns[['patinfo_resadmin1']] = keep_empty
# Patient residence (province), Standardize names to all uppercase, factor
#** Not being filled in the Mauritius dataset

clean_columns[['patinfo_resadmin2']] = uppercase('patinfo_resadmin2')
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df) { 
  cleaned <- ifelse(df$patinfo_occus == "No", "N", df$patinfo_occus) #* replce no's with N
  cleaned <- ifelse(grepl("medical|moh|rapid", df$patinfo_occus, ignore.case = T), "Y", cleaned) # replace certain categories of medical workers with yes
  return(cleaned) 
}
# Patient occupation, whether or not healthcare worker
# ** In the Mauritius file, healthcare workers of different types get specific job descriptions.
# ** All of those that currently exist are remapped to Yes. This list may need to be updated as new data comes in
# ** In the file, non-healthcare workers are marked as "No"

clean_columns[['patinfo_occus_specify']] = function(df) ifelse(df$patinfo_occus == "No", "Unspecified", df$patinfo_occus) 
# Patient occupation, character string (factor)
# ** For Mauritius, only healthcare work occupations are specified


clean_columns[['expo_travel']] = function(df) { 
  cleaned <- ifelse(df$expo_travel == "No", "N",df$expo_travel) # ** replace No with N
  cleaned <- ifelse(grepl("imported", df$expo_travel, ignore.case = T), "Y", cleaned) # ** replace Imported with Y
  return(cleaned)
}
# Patient history of travel?, Y/N, factor


clean_columns[['expo_travel_country']] = map_to('expo_travel_country')
# Country(ies) patient travelled to, character string (comma-separated list)


clean_columns[['expo_date_departure']] = clean_numeric_dates('expo_date_departure')
# Date departed from country visited / Date retuned from country visited, date, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = yes_no_clean('pat_symptomatic')
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)
# ** Mauritius is not entering any of the individual symptoms. 
# ** They directly enter whether or not the patient was symptomatic


clean_columns[['pat_contact']] = yes_no_clean('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = clean_dates_1('patcourse_dateonset')
# Date of onset of symptoms,  date, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')


clean_columns[['expo_sourcecaseids']] = keep_empty
# clean_columns[['expo_sourcecaseids']] = function(df) paste(df$expo_ID1,df$expo_ID2,df$expo_ID3,df$expo_ID4,sep=",") 
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = function(df) ifelse(df$Lab_result == "Pending", "RESULTS PENDING",toupper(df$report_classif))
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = clean_dates_1('patcourse_datedeath')
# Date of Death for decesased patients, date, YYYY-MM-DD, NA if Alive

clean_columns[['patinfo_resadmin3']] = keep_empty
# Place of residence admin level 3 (Health Zone/Town), factor
# ** Not available in Mauritius file

clean_columns[['patinfo_resadmin4']] = keep_empty
# Place of residence admin level 4 (Village), factor
# ** Not available in Mauritius file

clean_columns[['report_orginst']] = keep_empty
# Reporting health facility/institution, factor
# ** Not available in Mauritius file

clean_columns[['patinfo_idadmin1']] = keep_empty
# Where the case was diagnosed, admin level 1 (Province), factor
# ** Not available in Mauritius file


clean_columns[['patinfo_idadmin2']] = keep_empty
# Where the case was diagnosed, admin level 2 (District), factor
# ** Not available in Mauritius file


clean_columns[['report_pointofentry']] = keep_empty
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 
# ** Not available in Mauritius file


clean_columns[['report_pointofentry_date']] = keep_empty
# Date detected at point of entry, date, YYYY-MM-DD
# ** Not available in Mauritius file


clean_columns[['consultation_dateHF']] = keep_empty
# Date of first consultation at this Health Facility, date, YYYY-MM-DD
# ** Not available in Mauritius file


clean_columns[['patcourse_admit']] = keep_empty
# Admission to hospital?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['patcourse_presHCF']] = keep_empty
# ** Not available in Mauritius file


clean_columns[['patcourse_comp']] = keep_empty
# Other clinical complications, character string (comma-separated list)
# ** Not available in Mauritius file


clean_columns[['patsympt_fever']] = keep_empty
# History of fever or chills?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['patsympt_sorethroat']] = keep_empty
# History of sore throat?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['patsympt_cough']] = keep_empty
# History of cough?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['patsympt_runnynose']] = keep_empty
# History of runny nose?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['patsympt_short']] = keep_empty
# History of shortness of breath?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['patsympt_other']] = keep_empty
# Other signs or symptoms, character string (comma-separated list)
# ** Not available in Mauritius file


clean_columns[['Comcond_preexist1']] = keep_empty
# Patient has pre-existing conditions?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['Comcond_preexist']] = keep_empty
# Patient's pre-existing conditions, character string (comma-separated list)
# ** Not available in Mauritius file

clean_columns[['expo_visit_healthcare']] = keep_empty
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['expo_ari']] = keep_empty
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['expo_aricontsetting']] = keep_empty
# clean_columns[['expo_aricontsetting']] = function(df) { 
#  cleaned <- case_when(df$expo_aricontsetting == "Inconnu" ~ "UNKNOWN", #** change Inconnu to UNKNOWN
#                       TRUE ~ as.character(df$expo_aricontsetting))  # otherwise, leave as is
#  return(cleaned)}
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor
# ** Not available in Mauritius file


clean_columns[['expo_other']] = keep_empty
# clean_columns[['expo_other']] =  function(df) { cleaned <- case_when(df$expo_aricontsetting == "Inconnu" ~ "UNKNOWN",
#                                                                     TRUE ~ as.character(df$expo_aricontsetting))
# return(cleaned)}
# Other exposures, character string (comma-separated list)
# ** Not available in Mauritius file


clean_columns[['expo_contact_case']] = yes_no_clean('pat_contact')
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = keep_empty
# ID of confirmed or probable case 1, numeric
# ** Not available in Mauritius file

clean_columns[['expo_ID2']] = keep_empty
# ID of confirmed or probable case 2, numeric
# ** Not available in Mauritius file


clean_columns[['expo_ID3']] = keep_empty
# ID of confirmed or probable case 3, numeric
# ** Not available in Mauritius file


clean_columns[['expo_ID4']] = keep_empty
# ID of confirmed or probable case 4, numeric
# ** Not available in Mauritius file


clean_columns[['expo_arisetting']] = keep_empty
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)
# ** Not available in Mauritius file


clean_columns[['Lab_coll']] = keep_empty
# COVID19 lab sample collected?, Y/N, factor
# ** Not available in Mauritius file


clean_columns[['Lab_type']] = keep_empty
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL, factor
# ** Not available in Mauritius file


clean_columns[['Lab_datetaken']] = keep_empty
# Date when COVID19 Lab sample was taken, date, MM/DD/YYYY
# ** Not available in Mauritius file


clean_columns[['Lab_performed']] = keep_empty
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor
# ** Not available in Mauritius file


clean_columns[['Lab_result']] = keep_empty
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor
# ** Not available in Mauritius file


clean_columns[['Lab_resdate']] = keep_empty
# Date when COVID19 Lab result was returned, date, MM/DD/YYYY
# ** Not available in Mauritius file


clean_columns[['Lab_other']] = keep_empty
# Other lab sample(s), character string (comma-separated list)
# ** Not available in Mauritius file


clean_columns[['lab_other_samples_result_list']] = keep_empty
# Other lab sample result(s), character string (comma-separated list)
# ** Not available in Mauritius file


clean_columns[['patcourse_datedischarge']] = keep_empty
# Date when patient was discharged (if alive and hospitalized), date, MM/DD/YYYY
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized
# ** Not available in Mauritius file
