#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Exemplary Model to Follow
#
#     Country:
#     rwanda
#
#############################################

# Install pkgs

# Load utility functions
source("~/data-cleaning/notebooks/utils/utils.R")


# --------------------------------------
# initialize
clean_columns = list()

columns_used = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = function(df) as.character(df$patinfo_ID)
columns_used = append(columns_used,"patinfo_ID")
# Anonymized patient ID, convert to character string (some countries include letters in the ID)

clean_columns[['report_date']] = function(df) as.Date(df[,'report_date'], '%Y-%m-%d')
columns_used = append(columns_used,"report_date")
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) {
  df$patinfo_ageonset <- ifelse(df$patinfo_ageonset < 1, 0, df$patinfo_ageonset)
  format(as.numeric(as.character(df[,'patinfo_ageonset'])))
  df$patinfo_ageonset <- as.numeric(as.character(df[,'patinfo_ageonset']))
  }
columns_used = append(columns_used,"patinfo_ageonset")
# Age at intake in years, 0 for infants < 12 months, numeric. Convert from character to numeric and adjusted age 

clean_columns[['patinfo_ageonset_months']] = function(df) { 
    df$patinfo_ageonsetunit <- as.numeric(as.character(df[,'patinfo_ageonsetunit']))
    df$patinfo_ageonsetunit <- as.numeric(df$patinfo_ageonset)*12
    df$patinfo_ageonsetunit <- ifelse(df$patinfo_ageonsetunit > 23, NA, df$patinfo_ageonsetunit)
    }
columns_used = append(columns_used,"patinfo_ageonsetunit")
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric. Convert from character to numeric and adjusted age 

clean_columns[['patcourse_status']] =  function(df) toupper(df[,'patcourse_status'])
columns_used = append(columns_used,"patcourse_status")
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = map_to('patinfo_sex')
columns_used = append(columns_used,"patinfo_sex")
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) 
{ST_districts<-c("ÁGUA GRANDE","CANTAGALO","CAUÉ","LEMBÁ","LOBATA","MÉ ZOCHI")
df$patinfo_resadmin1<-toupper(df$patinfo_resadmin1)
df$patinfo_resadmin2<-toupper(df$patinfo_resadmin2)
ifelse(is.na(df$patinfo_resadmin2)==TRUE,df$patinfo_resadmin1,
       ifelse(df$patinfo_resadmin2 %in% ST_districts, "SAO TOME",
              ifelse(df$patinfo_resadmin2=="RAP", "RAP", df$patinfo_resadmin1)))
}
columns_used = append(columns_used,"patinfo_resadmin1")
# Patient residence (province), Standardize names to all uppercase, factor
# Province missing for many lines, fill in from districts

clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,'patinfo_resadmin2'])
columns_used = append(columns_used,"patinfo_resadmin2")
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df){
  ifelse(is.na(df[,'patinfo_occus']),NA,ifelse(df[,'patinfo_occus']=="Y","Y",ifelse(df[,'patinfo_occus']=="N","N",NA)))
}
columns_used = append(columns_used,"patinfo_occus")
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = map_to('patinfo_occus')
columns_used = append(columns_used,"patinfo_occus")
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = function(df) {
  cleaned <- ifelse(df$expo_travel == "No", "N", df$expo_travel) 
  cleaned <- ifelse(df$expo_travel == "Yes", "Y", cleaned)
  return(cleaned)
}
columns_used = append(columns_used,"exp_travel")
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = map_to('expo_travel_country')
columns_used = append(columns_used,"expo_travel_country")
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = function(df) as.Date(df[,'expo_date_departure'], '%Y-%m-%d')
columns_used = append(columns_used,"expo_date_departure")
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = function(df) ifelse(df[,'patsympt_fever']==1 |df[,'patsympt_sorethroat']==1|df[,'patsympt_cough']==1|df[,'patsympt_runnynose']==1|df[,'patsympt_short']==1|df[,'patsympt_other']==1,"Y",ifelse(df[,'patsympt_fever']==0 &df[,'patsympt_sorethroat']==0 & df[,'patsympt_cough']==0 & df[,'patsympt_runnynose']==0 & df[,'patsympt_short']==0 & df[,'patsympt_other']==0,"N",NA))
columns_used = append(columns_used,"pat_symptomatic")
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

#clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = function(df) as.Date(df[,'patcourse_dateonset'], '%Y-%m-%d')
columns_used = append(columns_used,"patcourse_dateonset")
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')
# *** This is currently identical to report_date.

clean_columns[['expo_sourcecaseids']] = function(df) paste(df[,'expo_ID1'],df[,'expo_ID2'],df[,'expo_ID3'],df[,'expo_ID4'],sep=",")
columns_used = append(columns_used,"expo_ID1")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
#columns_used = append(columns_used,"patcourse_severity")
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = function(df) ifelse(df[,'Lab_result']=="Pending","RESULTS PENDING",toupper(df[,'report_classif']))
columns_used = append(columns_used,"report_classif")
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = function(df) as.Date(df[,'patcourse_datedeath'], '%Y-%m-%d')
columns_used = append(columns_used,"patcourse_datedeath")
# Date of Death for decesased patients, character, YYYY-MM-DD, NA if Alive

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

clean_columns[['report_pointofentry']] = map_to('report_pointofentry')
columns_used = append(columns_used,"report_pointofentry")
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = function(df) as.Date(df[,'report_pointofentry_date'], '%Y-%m-%d')
columns_used = append(columns_used,"report_pointofentry_date")
# Date detected at point of entry, character, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = function(df) as.Date(df[,'consultation_dateHF'], '%Y-%m-%d')
columns_used = append(columns_used,"consultation_dateHF")
# Date of first consultation at this Health Facility, character, YYYY-MM-DD

clean_columns[['patcourse_admit']] = function(df){
  yes_no_clean('patcourse_admit')(df)
}
columns_used = append(columns_used,"patcourse_admit")
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = function(df) as.Date(df[,'patcourse_presHCF'], '%Y-%m-%d')
columns_used = append(columns_used,"patcourse_presHCF")
# For this episode, date first admitted to hospital, character, YYYY-MM-DD

clean_columns[['patcourse_comp']] = map_to('patcourse_comp')
columns_used = append(columns_used,"patcourse_comp")
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = function(df){
  yes_no_clean('patsympt_fever')(df)
}
columns_used = append(columns_used,"patsympt_fever")
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = function(df){
  yes_no_clean('patsympt_sorethroat')(df)
}
columns_used = append(columns_used,"patsympt_sorethroat")
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = function(df){
  yes_no_clean('patsympt_cough')(df)
}
columns_used = append(columns_used,"patsympt_cough")
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = function(df){
  yes_no_clean('patsympt_runnynose')(df)
}
columns_used = append(columns_used,"patsympt_runnynose")
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = function(df){
  yes_no_clean('patsympt_short')(df)
}
columns_used = append(columns_used,"patsympt_short")
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = map_to('patsympt_other')
columns_used = append(columns_used,"patsympt_other")
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = function(df){
  yes_no_clean('Comcond_preexist1')(df)
}
columns_used = append(columns_used,"Comcond_preexist1")
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = map_to('Comcond_preexist')
columns_used = append(columns_used,"Comcond_preexist")
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = function(df){
  yes_no_clean('expo_visit_healthcare')(df)
}
columns_used = append(columns_used,"expo_visit_healthcare")
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = function(df){
  yes_no_clean('expo_ari')(df)
}
columns_used = append(columns_used,"expo_ari")
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = map_to('expo_aricontsetting')
columns_used = append(columns_used,"expo_aricontsetting")
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = map_to('expo_other')
columns_used = append(columns_used,"expo_other")
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] =function(df){
  yes_no_clean('expo_contact_case')(df)
}
columns_used = append(columns_used,"expo_contact_case")
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = map_to('expo_ID1')
columns_used = append(columns_used,"expo_ID1")
# ID of confirmed or probable case 1, numeric

clean_columns[['expo_ID2']] = map_to('expo_ID2')
columns_used = append(columns_used,"expo_ID2")
# ID of confirmed or probable case 2, numeric

clean_columns[['expo_ID3']] = map_to('expo_ID3')
columns_used = append(columns_used,"expo_ID3")
# ID of confirmed or probable case 3, numeric

clean_columns[['expo_ID4']] = map_to('expo_ID4')
columns_used = append(columns_used,"expo_ID4")
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = map_to('expo_arisetting')
columns_used = append(columns_used,"expo_arisetting")
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = function(df){
  yes_no_clean('Lab_coll')(df)
}
columns_used = append(columns_used,"Lab_coll")
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = function(df) toupper(df[,'Lab_type'])
columns_used = append(columns_used,"Lab_type")
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = function(df) as.Date(df[,'Lab_datetaken'], '%Y-%m-%d')
columns_used = append(columns_used,"Lab_datetaken")
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

clean_columns[['Lab_performed']] = function(df) toupper(df[,'Lab_performed']) 
columns_used = append(columns_used,"Lab_performed")
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df) toupper(df[,'Lab_result'])
columns_used = append(columns_used,"Lab_result")
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = function(df) as.Date(df[,'Lab_resdate'], '%Y-%m-%d')
columns_used = append(columns_used,"Lab_resdate")
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

clean_columns[['Lab_other']] = map_to('Lab_other')
columns_used = append(columns_used,"Lab_other")
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_otherres']] = map_to('Lab_otherres')
columns_used = append(columns_used,"Lab_otherres")
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = function(df) as.Date(df[,'patcourse_datedischarge'], '%Y-%m-%d')
columns_used = append(columns_used,"patcourse_datedischarge")
# Date when patient was discharged (if alive and hospitalized), character, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized

# --------------------------------------
# generate colname lists for qualilty control

# remove duplicate column entries in the list of used columns (simply for conciseness)
columns_used = unique(columns_used)

# hardcode columns which are intentionally NOT used
columns_unused = list("name_first", "name_last", "Presentation_Delay", "Epidemiological_Week_Of_Onset", "Epidemiological_Week_Of_Report", "Age_Group", "Age_In_Months", "Age_group_YearsandMonths", "IsAlive", "IsDead", "IsCase", "CFR", "Duration_Of_Stay_In_HealthFacility", "Alive_Or_Dead", "IsMale_IsFemale", "IsSuspected", "IsConfirmed", "IsProbable", "IsCaseDeaths", "MatchAddressLevel2", "AddressLevel1_&_AddressLevel2_Keys", "Match_AddressLevel1_&_AddressLevel2", "IsMale", "IsFemale", "NewSuspected", "NewConfirmed", "NewProbable", "NewDeaths", "IsMaleSuspected", "IsFemaleSuspected", "LabResultDelay", "IsPCRPositive", "IsPCRNegative", "IsPCRPending", "ISPCRUknown", "IS_PCR_Inconclusive", "Is_ELISA_Positive", "Is_ELISA_Negative", "Is_ELISA_Inconclusive", "Is_ELISA_Uknown", "Is_ELISA_Pending", "Is_IFA_Positive", "Is_IFA_Negative", "Is_IFA_Inconclusive", "Is_IFA_Uknown", "Is_IFA_Pending", "Is_RDT_Positive", "Is_RDT_Negative", "Is_RDT_Inconclusive", "Is_RDT_Uknown", "Is_RDT_Pending", "Is_VirusCulture_Positive", "Is_VirusCulture_Negative", "Is_VirusCulture_Inconclusive", "Is_VirusCulture_Uknown", "Is_VirusCulture_Pending", "Is_Other_Positive", "Is_Other_Negative", "Is_Other_Inconclusive", "Is_Other_Uknown", "Is_Other_Pending", "FirstName_LastName", "ID_Duplicate", "Number_Of_Contacts", "Active_Contacts", "Follow_up_completeness" )

# You can use the following line of code to produce the list of intentionally unused columns
# given that you have a raw dataframe of which you already use all columns you want:
# colnames(raw)[!colnames(raw) %in% columns_used]
# BUT THIS HAS TO BE HARDCODED the unused columns!
# the following code puts this list in a format you can copy to hardcode above:
# cat(paste(paste0('"', colnames(raw)[!colnames(raw) %in% columns_used], '"'), collapse = ", "), "\n")
