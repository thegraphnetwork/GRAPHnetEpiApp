#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Country:
#     sao_tome_e_principe
#
#############################################

# Install pkgs

# install.packages("lubridate")
# library(lubridate)

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

clean_columns[['report_date']] = map_to("report_date")
# clean_columns[['report_date']] = function(df) ymd(as.character(df[,"report_date"]))
columns_used = append(columns_used,"report_date")
#    function(df) {
#    # replace with proper date format
#    df$report_date<-ifelse(nchar(df$report_date)==5,format(as.Date(as.numeric(df$report_date), origin = "1904-01-01"), '%Y-%m-%d'),df$report_date)
#    # correct typos in dates
#    df$report_date[df$report_date=="03/07/202"]<-format(as.Date("2020-07-03"),format="%Y-%m-%d")
#    df$report_date
#    }
# columns_used = append(columns_used,"report_date")
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) format(as.numeric(as.character(df[,"patinfo_ageonset"])))
columns_used = append(columns_used,"patinfo_ageonset_years")
#    {
#    years<-ifelse(df[,'patinfo_ageonset']=="S.I" |df[,'patinfo_ageonset']=="S.I." |df[,'patinfo_ageonset']=="NA"  ,NA,df[,'patinfo_ageonset'])
#    format(as.numeric(as.character(years)))
#    }
# columns_used = append(columns_used,"patinfo_ageonset")
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) {
   months<-ifelse(df[,'patinfo_ageonsetunit']=="NA",NA,df[,'patinfo_ageonsetunit'])
   ifelse(is.na(months)==T & as.numeric(as.character(df[,'patinfo_ageonset']))==0,0,format(as.numeric(as.character(months))))
   }
columns_used = append(columns_used,"patinfo_ageonset_months")
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be

clean_columns[['patcourse_status']] =  function(df) toupper(df[,"patcourse_status"])
columns_used = append(columns_used,"patcourse_status")
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = function(df) toupper(df$patinfo_sex)
columns_used = append(columns_used,"patinfo_sex")
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) toupper(df[,"patinfo_resadmin1"])
columns_used = append(columns_used,"patinfo_resadmin1")  
#   {ST_districts<-c("ÁGUA GRANDE","CANTAGALO","CAUÉ","LEMBÁ","LOBATA","MÉ ZOCHI")
#    df$patinfo_resadmin1<-toupper(df$patinfo_resadmin1)
#    df$patinfo_resadmin2<-toupper(df$patinfo_resadmin2)
#    ifelse(is.na(df$patinfo_resadmin2)==TRUE,df$patinfo_resadmin1,
#    ifelse(df$patinfo_resadmin2 %in% ST_districts, "SAO TOME",
#      ifelse(df$patinfo_resadmin2=="RAP", "RAP", df$patinfo_resadmin1)))
# }
# Patient residence (province), Standardize names to all uppercase, factor
# Province missing for many lines, fill in from districts

clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,'patinfo_resadmin2'])
columns_used = append(columns_used,"patinfo_resadmin2")
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df) ifelse(is.na(df[,'patinfo_occus']),NA,ifelse(df[,'patinfo_occus']=="Y","Y",ifelse(df[,'patinfo_occus']=="N","N",NA)))
columns_used = append(columns_used,"patinfo_occus")
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = map_to('patinfo_occus')
columns_used = append(columns_used,"patinfo_occus_specify")
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = map_to('expo_travel')
columns_used = append(columns_used,"expo_travel")
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = map_to('expo_travel_country')
columns_used = append(columns_used,"expo_travel_country")
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = map_to("expo_date_departure")
columns_used = append(columns_used,"expo_date_departure")
   # {
   # df$expo_date_departure<-ifelse(nchar(df$expo_date_departure)==5,format(as.Date(as.numeric(df$expo_date_departure), origin = "1904-01-01"), '%Y-%m-%d'),df$expo_date_departure)
   # as.Date(df[,'expo_date_departure'], '%Y-%m-%d')
   # }
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = function(df) ifelse(df[,'patsympt_fever']=="Yes" |df[,'patsympt_sorethroat']=="Yes"|df[,'patsympt_cough']=="Yes"|df[,'patsympt_runnynose']=="Yes"|df[,'patsympt_short']=="Yes"|df[,'patsympt_other']=="Yes","Y",ifelse(df[,'patsympt_fever']=="No" &df[,'patsympt_sorethroat']=="No" & df[,'patsympt_cough']=="No" & df[,'patsympt_runnynose']=="No" & df[,'patsympt_short']=="No" & df[,'patsympt_other']=="No","N",NA))
columns_used = append(columns_used,"pat_symptomatic")
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

# clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = map_to("patcourse_dateonset")
columns_used = append(columns_used,"patcourse_dateonset")
   # function(df) {
   # df$patcourse_dateonset<-ifelse(nchar(df$patcourse_dateonset)==5,format(as.Date(as.numeric(df$patcourse_dateonset), origin = "1904-01-01"), '%Y-%m-%d'),df$patcourse_dateonset)
   # as.Date(df[,'patcourse_dateonset'], '%Y-%m-%d')
   # }
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')
# *** This is currently identical to report_date.

clean_columns[['expo_sourcecaseids']] = function(df) paste(df[,'expo_ID1'],df[,'expo_ID2'],df[,'expo_ID3'],df[,'expo_ID4'],sep=",")
columns_used = append(columns_used,"expo_sourcecaseids")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# columns_used = append(columns_used,"patcourse_severity")
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = function(df) ifelse(df[,'Lab_result']=="Don't know","RESULTS PENDING",toupper(df[,'report_classif']))
columns_used = append(columns_used,"report_classif")
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = map_to("patcourse_datedeath")
columns_used = append(columns_used,"patcourse_datedeath")
   # function(df) as.Date(df[,'patcourse_datedeath'], '%Y-%m-%d')
# Date of Death for decesased patients, character, YYYY-MM-DD, NA if Alive

# clean_columns[['patinfo_resadmin3']] = map_to('patinfo_resadmin3')
# columns_used = append(columns_used,"patinfo_resadmin3")
# Place of residence admin level 3 (Health Zone/Town), factor

# clean_columns[['patinfo_resadmin4']] = map_to('patinfo_resadmin4')
# columns_used = append(columns_used,"patinfo_resadmin4")
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

clean_columns[['report_pointofentry_date']] = map_to("report_pointofentry_date")
columns_used = append(columns_used,"report_pointofentry_date")
   # function(df) {
   # df$report_pointofentry_date<-ifelse(nchar(df$report_pointofentry_date)==5,format(as.Date(as.numeric(df$report_pointofentry_date), origin = "1904-01-01"), '%Y-%m-%d'),df$report_pointofentry_date)
   # as.Date(df[,'report_pointofentry_date'], '%Y-%m-%d')
   # }
# Date detected at point of entry, character, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = map_to("consultation_dateHF")
columns_used = append(columns_used,"consultation_dateHF")
   # function(df) {
   # df$consultation_dateHF<-ifelse(nchar(df$consultation_dateHF)==5,format(as.Date(as.numeric(df$consultation_dateHF), origin = "1904-01-01"), '%Y-%m-%d'),df$consultation_dateHF)
   # as.Date(df[,'consultation_dateHF'], '%Y-%m-%d')
   # }
# Date of first consultation at this Health Facility, character, YYYY-MM-DD

clean_columns[['patcourse_admit']] = yes_no_clean('patcourse_admit')
columns_used = append(columns_used,"patcourse_admit")
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = map_to('patcourse_presHCF')
columns_used = append(columns_used,"patcourse_presHCF")
   # function(df) {
   # df$patcourse_presHCF<-ifelse(nchar(df$patcourse_presHCF)==5,format(as.Date(as.numeric(df$patcourse_presHCF), origin = "1904-01-01"), '%Y-%m-%d'),df$patcourse_presHCF)
   # as.Date(df[,'patcourse_presHCF'], '%Y-%m-%d')
   # }
# For this episode, date first admitted to hospital, character, YYYY-MM-DD

clean_columns[['patcourse_comp']] = map_to('patcourse_comp')
columns_used = append(columns_used,"patcourse_comp")
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = yes_no_clean('patsympt_fever')
columns_used = append(columns_used,"patsympt_fever")
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = yes_no_clean('patsympt_sorethroat')
columns_used = append(columns_used,"patsympt_sorethroat")
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = yes_no_clean('patsympt_cough')
columns_used = append(columns_used,"patsympt_cough")
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = yes_no_clean('patsympt_runnynose')
columns_used = append(columns_used,"patsympt_runnynose")
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = yes_no_clean('patsympt_short')
columns_used = append(columns_used,"patsympt_short")
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = yes_no_clean('patsympt_other')
columns_used = append(columns_used,"patsympt_other")
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = yes_no_clean('Comcond_preexist1')
columns_used = append(columns_used,"Comcond_preexist1")
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = map_to('Comcond_preexist')
columns_used = append(columns_used,"Comcond_preexist")
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = map_to('expo_visit_healthcare')
columns_used = append(columns_used,"expo_visit_healthcare")
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = map_to('expo_ari')
columns_used = append(columns_used,"expo_ari")
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = map_to('expo_aricontsetting')
columns_used = append(columns_used,"expo_aricontsetting")
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = map_to('expo_other')
columns_used = append(columns_used,"expo_other")
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = map_to('expo_contact_case')
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

clean_columns[['Lab_coll']] = yes_no_clean('Lab_coll')
columns_used = append(columns_used,"Lab_coll")
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = function(df) toupper(df[,'Lab_type'])
columns_used = append(columns_used,"Lab_type")
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = map_to("Lab_datetaken")
columns_used = append(columns_used,"Lab_datetaken")
   # function(df) {
   # df$Lab_datetaken<-ifelse(nchar(df$Lab_datetaken)==5,format(as.Date(as.numeric(df$Lab_datetaken), origin = "1904-01-01"), '%Y-%m-%d'),df$Lab_datetaken)
   # as.Date(df[,'Lab_datetaken'], '%Y-%m-%d')
   # }
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

clean_columns[['Lab_performed']] = function(df) toupper(df[,'Lab_performed']) 
columns_used = append(columns_used,"Lab_performed")
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df) {
   ifelse(toupper(df[,'Lab_result'])=="PENDING","AWAITING RESULTS",toupper(df[,'Lab_result']))
   }
columns_used = append(columns_used,"Lab_result")
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = map_to("Lab_resdate")
columns_used = append(columns_used,"Lab_resdate")
   # function(df) {
   # df$Lab_resdate<-ifelse(nchar(df$Lab_resdate)==5,format(as.Date(as.numeric(df$Lab_resdate), origin = "1904-01-01"), '%Y-%m-%d'),df$Lab_resdate)
   # as.Date(df[,'Lab_resdate'], '%Y-%m-%d')
   # }
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

clean_columns[['Lab_other']] = map_to('Lab_other')
columns_used = append(columns_used,"Lab_other")
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_otherres']] = map_to('Lab_otherres')
columns_used = append(columns_used,"Lab_otherres")
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = map_to("patcourse_datedischarge")
columns_used = append(columns_used,"patcourse_datedischarge")
   # function(df) {
   # df$patcourse_datedischarge<-ifelse(nchar(df$patcourse_datedischarge)==5,format(as.Date(as.numeric(df$patcourse_datedischarge), origin = "1904-01-01"), '%Y-%m-%d'),df$patcourse_datedischarge)
   # as.Date(df[,'patcourse_datedischarge'], '%Y-%m-%d')
   # }
# Date when patient was discharged (if alive and hospitalized), character, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized

# --------------------------------------
# generate colname lists for qualilty control

# remove duplicate column entries in the list of used columns (simply for conciseness)
columns_used = unique(columns_used)

# hardcode columns which are intentionally NOT used
columns_unused = list("Colunas2", "name_first", "name_last", "patinfo_ageonset", "patinfo_ageonsetunit", "patinfo_resadmin3", "patinfo_resadmin4", "Presentation_Delay", "Epidemiological_Week_Of_Onset", "Epidemiological_Week_Of_Report", "Age_Group", "Age_In_Months", "Age_group_YearsandMonths", "IsAlive", "IsDead", "IsCase", "CFR", "Duration_Of_Stay_In_HealthFacility", "Alive_Or_Dead", "IsMale_IsFemale", "IsSuspected", "IsConfirmed", "IsProbable", "IsCaseDeaths", "MatchAddressLevel2", "AddressLevel1_&_AddressLevel2_Keys", "Match_AddressLevel1_&_AddressLevel2", "IsMale", "IsFemale", "NewSuspected", "NewConfirmed", "NewProbable", "NewDeaths", "IsMaleSuspected", "IsFemaleSuspected", "LabResultDelay", "IsPCRPositive", "IsPCRNegative", "IsPCRPending", "ISPCRUknown", "IS_PCR_Inconclusive", "Is_ELISA_Positive", "Is_ELISA_Negative", "Is_ELISA_Inconclusive", "Is_ELISA_Uknown", "Is_ELISA_Pending", "Is_IFA_Positive", "Is_IFA_Negative", "Is_IFA_Inconclusive", "Is_IFA_Uknown", "Is_IFA_Pending", "Is_RDT_Positive", "Is_RDT_Negative", "Is_RDT_Inconclusive", "Is_RDT_Uknown", "Is_RDT_Pending", "Is_VirusCulture_Positive", "Is_VirusCulture_Negative", "Is_VirusCulture_Inconclusive", "Is_VirusCulture_Uknown", "Is_VirusCulture_Pending", "Is_Other_Positive", "Is_Other_Negative", "Is_Other_Inconclusive", "Is_Other_Uknown", "Is_Other_Pending", "FirstName_LastName", "ID_Duplicate", "Number_Of_Contacts", "Active_Contacts", "Follow_up_completeness", "Estado", "Colunas1")

# You can use the following line of code to produce the list of intentionally unused columns
# given that you have a raw dataframe of which you already use all columns you want:
# colnames(raw)[!colnames(raw) %in% columns_used]
# BUT THIS HAS TO BE HARDCODED the unused columns!
# the following code puts this list in a format you can copy to hardcode above:
# cat(paste(paste0('"', colnames(raw)[!colnames(raw) %in% columns_used], '"'), collapse = ", "), "\n")
