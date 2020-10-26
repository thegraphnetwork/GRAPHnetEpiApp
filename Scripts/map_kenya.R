#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Exemplary Model to Follow
#
#     Country:
#     kenya
#
#############################################

# Install pkgs

# Load utility functions
library(here)
source(here("Scripts","utils.R"))


# --------------------------------------
# initialize
clean_columns = list()

columns_used = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = function(df) toupper(as.character(df[, "Case ID"]))
columns_used = append(columns_used,"Case ID")
# Anonymized patient ID, convert to character string (some countries include letters in the ID)

#clean_columns[['report_date']] = function(df) as.Date(df[,'report_date'], '%Y-%m-%d')
#columns_used = append(columns_used,"report_date")
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df){
  cleaned <- dplyr::case_when(
    
    nchar(df[,"Age (Years)"]) > 4 ~ "0",
    nchar(df[,"Age (Years)"]) <= 2 ~ as.character(df[,"Age (Years)"]),
    TRUE ~ NA_character_)
  return(as.integer(cleaned))
}
columns_used = append(columns_used,"Age (Years)")
# Age at intake in years, 0 for infants < 12 months, numeric

#clean_columns[['patinfo_ageonset_months']] = function(df) ifelse(is.na(df[,'patinfo_ageonsetunit'])==T & as.numeric(as.character(df[,'patinfo_ageonset']))==0,0,format(as.numeric(as.character(df[,'patinfo_ageonsetunit']))))  
#columns_used = append(columns_used,"patinfo_ageonsetunit")
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be

clean_columns[['patcourse_status']] =  function(df){
  return(dplyr::case_when(
    grepl("Dischar", df[, "Outcome(Death/Discharge/Still in Hospital)"], ignore.case = T) ~ "RECOVERED",
    grepl("DEAD", df[, "Outcome(Death/Discharge/Still in Hospital)"], ignore.case = T) ~ "DEAD",
    grepl("STILL", df[, "Outcome(Death/Discharge/Still in Hospital)"], ignore.case = T) ~ "ALIVE",
    TRUE ~ NA_character_
    
  ))
} 
columns_used = append(columns_used,"Outcome(Death/Discharge/Still in Hospital)")
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = function(df){
  return(
    dplyr::case_when(
      grepl("m|M",df[, "Sex"])~ "M",
      grepl("f|F",df[, "Sex"])~ "F",
      TRUE ~ NA_character_
    )
  )
}
columns_used = append(columns_used,"Sex")
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) {
  
  
  Coast = c("Mombasa", "Kwale", "Kilifi", "Tana River", "Lamu", "Taita Taveta", "MOMBASA", "Taita taveta", "Tana river", "Tana River")
  
  North_Eastern <- c("Garissa", "Wajir", "Mandera", "WAJIR")
  
  Eastern <- c("Marsabit", "Isiolo", "Meru", "Tharaka-Nithi",
               "Embu", "Kitui", "Machakos", "Makueni", "MACHAkos", "Tharaka Nithi")
  
  Central <- c("Nyandarua", "Nyeri", "Kirinyaga", "Muranga", "Kiambu", "Murang'a", "KIAMBU", "NYERI")
  
  Rift_Valley <- c("Turkana", "West Pokot", "Samburu", "Trans-Nzoia", "Uasin Gishu",
                   "Elgeyo-Marakwet", "Nandi", "Baringo", "Laikipia", "Nakuru", "Narok",
                   "Kajiado", "KAJIADO", "Kericho", "Bomet", "Trans Nzoia", "Elgeyo Marakwet", "NAKURU", "Trans Nzoia") 
  
  Western <- c("Kakamega", "Vihiga", "Bungoma", "Busia", "BUSIA")
  
  Nyanza <- c("Siaya", "Kisumu", "Homa Bay", "Migori", "Kisii", "Nyamira", "Homa bay", "KISUMU", "MIGORI")
  
  Nairobi <- c("Nairobi", "NAIROBI")
  
  return(
    dplyr::case_when(
      df[,"County of Residence"] %in% Nairobi ~ "NAIROBI",
      df[,"County of Residence"] %in% Coast ~ "COAST",
      df[,"County of Residence"] %in% North_Eastern ~ "NORTH EASTERN",
      df[,"County of Residence"] %in% Eastern ~ "EASTERN",
      df[,"County of Residence"] %in% Central ~ "CENTRAL",
      df[,"County of Residence"] %in% Rift_Valley ~ "RIFT VALLEY",
      df[,"County of Residence"] %in% Western ~ "WESTERN",
      df[,"County of Residence"] %in% Nyanza ~ "NYANZA",
      TRUE ~ NA_character_
      
    )
  )
}
columns_used = append(columns_used,"County of Residence")
# Patient residence (province), Standardize names to all uppercase, factor
# Province missing for many lines, fill in from districts

clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,"County of Residence"])
columns_used = append(columns_used,"County of Residence")
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df){
  return(
    ifelse(grepl("HCW|clinical|doctor|hosp|health",df[, "Occupation(Sector/Job Title/Employer)"], ignore.case = T), "Y", "N")
  )
}
columns_used = append(columns_used,"Occupation(Sector/Job Title/Employer)")
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = map_to("Occupation(Sector/Job Title/Employer)")
columns_used = append(columns_used,"Occupation(Sector/Job Title/Employer)")
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = function(df){
  return(ifelse(grepl("imported", df[,"Local/Imported"], ignore.case = T), "Y", "N"))
}
columns_used = append(columns_used,"Local/Imported")
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = function(df){
  return(ifelse(df[, "Where from"] == "N/A", NA_character_, toupper(df[, "Where from"])))
}
columns_used = append(columns_used,"Where from")
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = function(df) {
  
  cleaned <- lubridate::ymd(df[,"Date of arrival"])
  return(cleaned)
}
columns_used = append(columns_used,"Date of arrival")
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = function(df) ifelse(grepl("y|Y", df[,"Symptoms(Yes/No)"]), "Y", "N")
columns_used = append(columns_used,"Symptoms(Yes/No)")
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

clean_columns[['pat_contact']] = function(df){
  no_contact <- c("N/A", "no", "No", "NO", "No Data", "MOMBASA", "Multiple", "None", "Operates Ideal medical centre Eastleigh", 
                  "Staff at Boma Hotel, MQF", "Staff at KPA", "Uknown", "unknown", "Unknown", "UNKNOWN", "Unkown", "unkwown", "Workplace",
                  "works at Eastleigh", "Works at KPA", "44111") 
  return(ifelse(df[,"History of Contact with Covid-19 case"] %in% no_contact| is.na(df[,"History of Contact with Covid-19 case"]), "N", "Y" ))
}

columns_used = append(columns_used, "History of Contact with Covid-19 case")
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = function(df){
  
  return(lubridate::ymd(df[,"Date of onset of symptoms"]))
} 
columns_used = append(columns_used,"Date of onset of symptoms")
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')
# *** This is currently identical to report_date.

clean_columns[['expo_sourcecaseids']] = function(df) {
  no_contact <- c("N/A", "no", "No", "NO", "No Data", "MOMBASA", "Multiple", "None", "Operates Ideal medical centre Eastleigh", 
                  "Staff at Boma Hotel, MQF", "Staff at KPA", "Uknown", "unknown", "Unknown", "UNKNOWN", "Unkown", "unkwown", "Workplace",
                  "works at Eastleigh", "Works at KPA", "44111", "Yes", "y", "Y", "Yes (Not been able to eastablish any contact information)",
                  "Yes Global Tea", "YES", "Works in Boma Hotel former quarantine site", "Yes (Not been able to eastablish any contact information)",
                  "Workplace", "works at Eastleigh","44111", "A Pathologist in Mombasa","Case",
                  "Contact of a taxi driver?", "Contact with suspect case from Dubai",
                  "Daughter", "Family Setting", "Has been working in isolation ward",
                  "Health worker Nganjoni Dispensary", "Health worker taking care of Boma,Quarantine site", 
                  "Husband works in KPA", "KQ 604 SEAT 26A","MOMBASA", "Multiple", "Operates Ideal medical centre Eastleigh",
                  "Quarantine site KMTC", "SELF QUARANTINE", "Staff at Boma Hotel, MQF",
                  "Staff at KPA", "Staff at KUTRH quarantine site", "Staff at Mbagathi Hospital IDU quarantine site",
                  "Staff in KUTRH", "Workplace", "works at Eastleigh", "Works at KPA",
                  "Works in KPA Mombasa Beach", "Does Business in Eastleigh","N",
                  "Watchman manning KMTC Quarantine site", "yes", "Yes Father", "Yes Husband", "Contact of the Ngara Health care worker", "Manager safari park, quarantine site")
  cleaned <- ifelse(df[,"History of Contact with Covid-19 case"] %in% no_contact, NA_character_, df[,"History of Contact with Covid-19 case"])
  library(stringr)
  library(stringr)
  
  
  cleaned <- str_to_sentence(str_remove_all(cleaned, "yes|YES|Yes|yes|YEs|Y"))
  
  cleaned  <- str_remove_all(cleaned, "Collegue|collegue|[[:punct:]]")
  
  return(cleaned)
}
columns_used = append(columns_used,"History of Contact with Covid-19 case")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# columns_used = append(columns_used,"patcourse_severity")
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = function(df) {return(ifelse(df[,"Lab results"] %in% c("POSITIVE", "Positive"), "CONFIRMED", "RESULTS PENDING"))}
columns_used = append(columns_used,"Lab results")
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = function(df){
  cleaned <- dplyr::case_when(
    grepl("dead", df[,"Outcome(Death/Discharge/Still in Hospital)"], ignore.case = T) ~ lubridate::ymd(df[,"Date of outcome"]),
    TRUE ~ NA_real_
  )
  
  return(cleaned)
}
columns_used = append(columns_used,"Date of outcome")
# Date of Death for decesased patients, character, YYYY-MM-DD, NA if Alive

clean_columns[['patinfo_resadmin3']] = function(df){
  no_name <- c("N/A", "N", "No Data", "No data", "Not indicated")
  ifelse(df[,"Sub county"] %in% no_name, NA_character_, toupper(df[,"Sub county"]))
}
columns_used = append(columns_used,"Sub county")
# Place of residence admin level 3 (Health Zone/Town), factor

clean_columns[['patinfo_resadmin4']] = function(df){
  no_name <- c("N/A", "N", "No Data", "No data", "Not indicated",
               "HARDEEP@FWWL.COM")
  ifelse(df[,"Village/Estate"] %in% no_name, NA_character_, toupper(df[,"Village/Estate"]))
}

columns_used = append(columns_used,"Village/Estate")
# Place of residence admin level 4 (Village), factor

clean_columns[['report_orginst']] = function(df){
  no_name <- c("no", "No", "NO", "No Data", "18/07/2020", "DEAD", 
               "Died at home", "Died at Home", "died on the way to hospital",
               "Discharged", "In quarantine", "n", "N", "n/a", "N/A", "Outspan isolation", "paedetrician",
               "Sef Isolation", "self isolating", "self isolation", 
               "self Isolation", "Self isolation", "Self Isolation", "YES")
  ifelse(df[,"Health facility Name"] %in% no_name, NA_character_, toupper(df[,"Health facility Name"]))
  
}
columns_used = append(columns_used,"Health facility Name")
# Reporting health facility/institution, factor

clean_columns[['patinfo_idadmin1']] = function(df){
  Coast = c("Mombasa", "Kwale", "Kilifi", "Tana River", "Lamu", "Taita Taveta", "MOMBASA", "Taita taveta", "Tana river", "Tana River")
  
  North_Eastern <- c("Garissa", "Wajir", "Mandera", "WAJIR")
  
  Eastern <- c("Marsabit", "Isiolo", "Meru", "Tharaka-Nithi",
               "Embu", "Kitui", "Machakos", "Makueni", "MACHAkos", "Tharaka Nithi")
  
  Central <- c("Nyandarua", "Nyeri", "Kirinyaga", "Muranga", "Kiambu", "Murang'a", "KIAMBU", "NYERI")
  
  Rift_Valley <- c("Turkana", "West Pokot", "Samburu", "Trans-Nzoia", "Uasin Gishu",
                   "Elgeyo-Marakwet", "Nandi", "Baringo", "Laikipia", "Nakuru", "Narok",
                   "Kajiado", "KAJIADO", "Kericho", "Bomet", "Trans Nzoia", "Elgeyo Marakwet", "NAKURU", "Trans Nzoia") 
  
  Western <- c("Kakamega", "Vihiga", "Bungoma", "Busia", "BUSIA")
  
  Nyanza <- c("Siaya", "Kisumu", "Homa Bay", "Migori", "Kisii", "Nyamira", "Homa bay", "KISUMU", "MIGORI")
  
  Nairobi <- c("Nairobi", "NAIROBI")
  
  
  return(
    dplyr::case_when(
      df[,"County where the case was Diagonised"] %in% Nairobi ~ "NAIROBI",
      df[,"County where the case was Diagonised"] %in% Coast ~ "COAST",
      df[,"County where the case was Diagonised"] %in% North_Eastern ~ "NORTH EASTERN",
      df[,"County where the case was Diagonised"] %in% Eastern ~ "EASTERN",
      df[,"County where the case was Diagonised"] %in% Central ~ "CENTRAL",
      df[,"County where the case was Diagonised"] %in% Rift_Valley ~ "RIFT VALLEY",
      df[,"County where the case was Diagonised"] %in% Western ~ "WESTERN",
      df[,"County where the case was Diagonised"] %in% Nyanza ~ "NYANZA",
      TRUE ~ NA_character_
      
    )
  )
}
columns_used = append(columns_used,"County where the case was Diagonised")
# Where the case was diagnosed, admin level 1 (Province), factor

clean_columns[['patinfo_idadmin2']] = function(df){
  return(toupper(df[,"County where the case was Diagonised"]))
}
columns_used = append(columns_used,"County where the case was Diagonised")
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = function(df){
  ifelse(!is.na(df[,"Where from"]),"Y", "N")
}
columns_used = append(columns_used,"Where from")
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = function(df){
  cleaned <- lubridate::ymd(df[, "Date of arrival"])
  return(cleaned)
}
columns_used = append(columns_used,"Date of arrival")
# Date detected at point of entry, character, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = function(df){
  
  
  return(lubridate::ymd(df[,"Date of visit (Hospital)"]))
}
columns_used = append(columns_used,"Date of visit (Hospital)")
# Date of first consultation at this Health Facility, character, YYYY-MM-DD

clean_columns[['patcourse_admit']] = function(df){
  x <- c("2nd test", "Consequtive test was negative",
         "Isolated at Hill Park", "n", "N", "n/a", "N/A", "no",
         "No", "NO", "none", "Not Yet", "Self-isolated", "Self-isolation",
         "self isolation", "Self Isolation", "Yes", "6684806")
  return (ifelse(is.na(df[, "DateAdmission"])|is.na(df[, "DateAdmission"]) %in% x , "N", "Y"))
}
columns_used = append(columns_used,"DateAdmission")
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = function(df) {
  x <- c("2nd test", "Consequtive test was negative",
         "Isolated at Hill Park", "n", "N", "n/a", "N/A", "no",
         "No", "NO", "none", "Not Yet", "Self-isolated", "Self-isolation",
         "self isolation", "Self Isolation", "Yes", "6684806", "Self-isolated")
  cleaned <- ifelse(df[,"DateAdmission"] %in% x, NA_character_, as.character(df[,"DateAdmission"]))
  cleaned2 <- dplyr::case_when(nchar(cleaned) == 5 ~ lubridate::as_date(as.numeric(cleaned), origin = "1899-12-30"), 
                               nchar(cleaned)  > 5 ~ lubridate::dmy(cleaned),
                               TRUE ~ NA_real_)
  return(cleaned2)
}  
columns_used = append(columns_used,"DateAdmission")
# For this episode, date first admitted to hospital, character, YYYY-MM-DD

#clean_columns[['patcourse_comp']] = map_to('patcourse_comp')
#columns_used = append(columns_used,"patcourse_comp")
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = function(df){
  return(
    dplyr::case_when(
      grepl("[yY]", df[,"Fever"]) & !is.na(df[,"Fever"]) ~ "Y",
      !is.na(df[,"Fever"]) ~ "N",
      TRUE ~ NA_character_
    )
  )
}
columns_used = append(columns_used,"Fever")
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = function(df){
  ifelse(grepl("throat",df[, "Others"], ignore.case = T), "Y", "F")
}
columns_used = append(columns_used,"Others")
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = function(df){
  return(
    dplyr::case_when(
      grepl("[yY]", df[,"Cough"]) & !is.na(df[,"Cough"]) ~ "Y",
      !is.na(df[,"Cough"]) ~ "N",
      TRUE ~ NA_character_
    )
  )
}
columns_used = append(columns_used,"Cough")
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = function(df){
  ifelse(grepl("nose",df[, "Others"], ignore.case = T), "Y", "F")
}
columns_used = append(columns_used,"Others")
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = function(df){
  return(
    dplyr::case_when(
      grepl("[yYTt]", df[,"Difficulty in breathing"]) & !is.na(df[,"Difficulty in breathing"]) ~ "Y",
      !is.na(df[,"Difficulty in breathing"]) ~ "N",
      TRUE ~ NA_character_
    )
  )
}
columns_used = append(columns_used,"Difficulty in breathing")
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = map_to('Others')
columns_used = append(columns_used,"Others")
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = function(df){
  no_comorb <- c("########", "0", "20-06-2020", "N", 
                 "N/A", "Ni", "Nil", "NIL", "no", "No",
                 "NO", "none", "None", "NONE", "Undisclosed", "Y", "Yes",
                 "Unknown")
  cleaned <- ifelse(df[,"Co-mobidity"] %in% no_comorb, NA_character_, df[,"Co-mobidity"])
  return(ifelse(is.na(cleaned), "N", "Y"))
}
columns_used = append(columns_used,"Co-mobidity")
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = function(df){
  no_comorb <- c("########", "0", "20-06-2020", "N", 
                 "N/A", "Ni", "Nil", "NIL", "no", "No",
                 "NO", "none", "None", "NONE", "Undisclosed", "Y", "Yes",
                 "Unknown")
  ifelse(df[,"Co-mobidity"] %in% no_comorb, NA_character_, df[,"Co-mobidity"])
}
columns_used = append(columns_used,"Co-mobidity")
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = function(df){
  no_hosp <- c(",sorethroat", "14/07/020", "18/7/2020", "43958",
               "43959", "43961", "43965", "43966", "43967", "43968",
               "43973", "44000", "44012", "Body weakness,Lots of sweating",
               "Collapsed on arrival", "General body weakness", "N/A",
               "N", "Nil", "no", "Nno", "no", "No", "NO", "Self isolation")
  cleaned <- ifelse(df[,"Visit Health facility"] %in% no_hosp, NA_character_, df[,"Visit Health facility"])
  return(ifelse(is.na(cleaned), "N", "Y"))
}
columns_used = append(columns_used,"Visit Health facility")
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
  no_contact <- c("N/A", "no", "No", "NO", "No Data", "MOMBASA", "Multiple", "None", "Operates Ideal medical centre Eastleigh", 
                  "Staff at Boma Hotel, MQF", "Staff at KPA", "Uknown", "unknown", "Unknown", "UNKNOWN", "Unkown", "unkwown", "Workplace",
                  "works at Eastleigh", "Works at KPA", "44111", "Yes", "y", "Y", "Yes (Not been able to eastablish any contact information)",
                  "Yes Global Tea", "YES", "Works in Boma Hotel former quarantine site", "Yes (Not been able to eastablish any contact information)",
                  "Workplace", "works at Eastleigh","44111", "A Pathologist in Mombasa","Case",
                  "Contact of a taxi driver?", "Contact with suspect case from Dubai",
                  "Daughter", "Family Setting", "Has been working in isolation ward",
                  "Health worker Nganjoni Dispensary", "Health worker taking care of Boma,Quarantine site", 
                  "Husband works in KPA", "KQ 604 SEAT 26A","MOMBASA", "Multiple", "Operates Ideal medical centre Eastleigh",
                  "Quarantine site KMTC", "SELF QUARANTINE", "Staff at Boma Hotel, MQF",
                  "Staff at KPA", "Staff at KUTRH quarantine site", "Staff at Mbagathi Hospital IDU quarantine site",
                  "Staff in KUTRH", "Workplace", "works at Eastleigh", "Works at KPA",
                  "Works in KPA Mombasa Beach", "Does Business in Eastleigh","N",
                  "Watchman manning KMTC Quarantine site", "yes", "Yes Father", "Yes Husband", "Contact of the Ngara Health care worker", "Manager safari park, quarantine site")
  cleaned <- ifelse(df[,"History of Contact with Covid-19 case"] %in% no_contact, NA_character_, df[,"History of Contact with Covid-19 case"])
  
  return(ifelse(is.na(cleaned), "N", "Y"))
}
columns_used = append(columns_used,"History of Contact with Covid-19 case")
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = function(df) {
  no_contact <- c("N/A", "no", "No", "NO", "No Data", "MOMBASA", "Multiple", "None", "Operates Ideal medical centre Eastleigh", 
                  "Staff at Boma Hotel, MQF", "Staff at KPA", "Uknown", "unknown", "Unknown", "UNKNOWN", "Unkown", "unkwown", "Workplace",
                  "works at Eastleigh", "Works at KPA", "44111", "Yes", "y", "Y", "Yes (Not been able to eastablish any contact information)",
                  "Yes Global Tea", "YES", "Works in Boma Hotel former quarantine site", "Yes (Not been able to eastablish any contact information)",
                  "Workplace", "works at Eastleigh","44111", "A Pathologist in Mombasa","Case",
                  "Contact of a taxi driver?", "Contact with suspect case from Dubai",
                  "Daughter", "Family Setting", "Has been working in isolation ward",
                  "Health worker Nganjoni Dispensary", "Health worker taking care of Boma,Quarantine site", 
                  "Husband works in KPA", "KQ 604 SEAT 26A","MOMBASA", "Multiple", "Operates Ideal medical centre Eastleigh",
                  "Quarantine site KMTC", "SELF QUARANTINE", "Staff at Boma Hotel, MQF",
                  "Staff at KPA", "Staff at KUTRH quarantine site", "Staff at Mbagathi Hospital IDU quarantine site",
                  "Staff in KUTRH", "Workplace", "works at Eastleigh", "Works at KPA",
                  "Works in KPA Mombasa Beach", "Does Business in Eastleigh","N",
                  "Watchman manning KMTC Quarantine site", "yes", "Yes Father", "Yes Husband", "Contact of the Ngara Health care worker", "Manager safari park, quarantine site")
  cleaned <- ifelse(df[,"History of Contact with Covid-19 case"] %in% no_contact, NA_character_, df[,"History of Contact with Covid-19 case"])
  library(stringr)
  library(stringr)
  
  
  cleaned <- str_to_sentence(str_remove_all(cleaned, "yes|YES|Yes|yes|YEs|Y"))
  
  cleaned  <- str_remove_all(cleaned, "Collegue|collegue|[[:punct:]]")
  
  return(cleaned)
}
columns_used = append(columns_used,"History of Contact with Covid-19 case")
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

#clean_columns[['expo_arisetting']] = map_to('expo_arisetting')
#columns_used = append(columns_used,"expo_arisetting")
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = function(df){
  ifelse(grepl("[yY]",df[,"Sample collected"]), "Y", NA_character_)
}
columns_used = append(columns_used,"Sample collected")
# COVID19 lab sample collected?, Y/N, factor

#clean_columns[['Lab_type']] = function(df) toupper(df[,'Lab_type'])
#columns_used = append(columns_used,"Lab_type")
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = function(df) as.Date(df[,'Date sample collected'], '%Y-%m-%d')
columns_used = append(columns_used,"Date sample collected")
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

#clean_columns[['Lab_performed']] = function(df) toupper(df[,'Lab_performed']) 
#columns_used = append(columns_used,"Lab_performed")
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df){
  ifelse(grepl("P", df[, "Lab results"], ignore.case = T), "POSITIVE", NA_character_)
}
columns_used = append(columns_used,"Lab results")
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = function(df) {
  
  return(lubridate::ymd(df[,'Date of lab confirmation']))
}
columns_used = append(columns_used,"Date of lab confirmation")
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

#clean_columns[['Lab_other']] = map_to('Lab_other')
#columns_used = append(columns_used,"Lab_other")
# Other lab sample(s), character string (comma-separated list)

#clean_columns[['lab_otherres']] = map_to('Lab_otherres')
#columns_used = append(columns_used,"Lab_otherres")
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = function(df){
  cleaned <- dplyr::case_when(
    grepl("dischar", df[,"Outcome(Death/Discharge/Still in Hospital)"], ignore.case = T) ~ lubridate::ymd(df[,"Date of outcome"]),
    TRUE ~ NA_real_
  )
  
  
  return(cleaned)
}
columns_used = append(columns_used,"Date of outcome")
# Date when patient was discharged (if alive and hospitalized), character, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized

# --------------------------------------
# generate colname lists for qualilty control

# remove duplicate column entries in the list of used columns (simply for conciseness)
columns_used = unique(columns_used)

# hardcode columns which are intentionally NOT used
columns_unused = list()

# You can use the following line of code to produce the list of intentionally unused columns
# given that you have a raw dataframe of which you already use all columns you want:
colnames(df)[!colnames(df) %in% columns_used]
# BUT THIS HAS TO BE HARDCODED the unused columns!
# the following code puts this list in a format you can copy to hardcode above:
cat(paste(paste0('"', colnames(df)[!colnames(df) %in% columns_used], '"'), collapse = ", "), "\n")
