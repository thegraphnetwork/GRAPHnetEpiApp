#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#
#     Country:
#     burundi
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

clean_columns[['report_date']] = clean_dates_1('report_date')
  # Date of notification (line), date, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = map_to('patinfo_ageonset')
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) ifelse(is.na(df$patinfo_ageonsetunit) & as.numeric(as.character(df$patinfo_ageonset))==0,0,format(as.numeric(as.character(df$patinfo_ageonsetunit))))  
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be

#** Completely empty for Burundi file at the moment

clean_columns[['patcourse_status']] =  uppercase('patcourse_status')
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = map_to('patinfo_sex')
# M/F, factor

clean_columns[['patinfo_resadmin1']] = uppercase('patinfo_resadmin1')
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[['patinfo_resadmin2']] = uppercase('patinfo_resadmin2')
# Patient residence (district), Standardize names to all uppercase, factor

# clean_columns[['patinfo_occus']] = map_to('patinfo_occus')
clean_columns[['patinfo_occus']] = keep_empty
# Patient occupation
#** Keeping this empty for now as it is not clear how they code healthcareworker

clean_columns[['patinfo_occus_specify']] = map_to('patinfo_occus')
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = yes_no_clean('expo_travel')
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = map_to('expo_travel_country')
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = clean_dates_1('expo_date_departure')
# Date departed from country visited / Date retuned from country visited, date, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = function(df) { 
  ifelse(df$patsympt_fever==1 | df$patsympt_sorethroat==1|df$patsympt_cough==1|df$patsympt_runnynose==1|df$patsympt_short==1|df$patsympt_other==1,"Y",
         ifelse(df$patsympt_fever==0 & df$patsympt_sorethroat==0 & df$patsympt_cough==0 & df$patsympt_runnynose==0 & df$patsympt_short==0 & df$patsympt_other==0,"N",
                NA))}
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

#clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

#OLD
#clean_columns[['patcourse_dateonset']] = function(df) format(as.Date(df[,'patcourse_dateonset']),"%d/%m/%Y")
# Takes in a date (guesses the original format),  spits it out in the format YYYY-MM-DD

# NEW
clean_columns[['patcourse_dateonset']] = clean_dates_1('patcourse_dateonset')
# Date of onset of symptoms,  date, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')


clean_columns[['expo_sourcecaseids']] = function(df) paste(df$expo_ID1,df$expo_ID2,df$expo_ID3,df$expo_ID4,sep=",") 
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor
# **does not exist in Burundi excel file 

clean_columns[['report_classif']] = function(df) ifelse(df$Lab_result == "Pending", "RESULTS PENDING",toupper(df$report_classif))
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = clean_dates_1('patcourse_datedeath')
# Date of Death for decesased patients, date, YYYY-MM-DD, NA if Alive

clean_columns[['patinfo_resadmin3']] = map_to('patinfo_resadmin3')
# Place of residence admin level 3 (Health Zone/Town), factor

clean_columns[['patinfo_resadmin4']] = map_to('patinfo_resadmin4')
# Place of residence admin level 4 (Village), factor

clean_columns[['report_orginst']] = map_to('report_orginst')
# Reporting health facility/institution, factor
# ** Burundi's file has the value "Contact etroit" in here, which means close contact. Not sure what that means in this context...

clean_columns[['patinfo_idadmin1']] = map_to('patinfo_idadmin1')
# Where the case was diagnosed, admin level 1 (Province), factor

clean_columns[['patinfo_idadmin2']] = map_to('patinfo_idadmin2')
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = yes_no_clean('report_pointofentry')
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = clean_dates_1('report_pointofentry_date')
# Date detected at point of entry, date, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = clean_dates_1('consultation_dateHF')
# Date of first consultation at this Health Facility, date, YYYY-MM-DD

clean_columns[['patcourse_admit']] = yes_no_clean('patcourse_admit')
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = clean_dates_1('patcourse_presHCF')

clean_columns[['patcourse_comp']] = yes_no_clean('patcourse_comp')
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
# ** Burundi is not filling in this column correctly. They are entering Yes/No/Don't Know values

clean_columns[['expo_visit_healthcare']] = yes_no_clean('expo_visit_healthcare')
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = yes_no_clean('expo_ari')
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = function(df) { 
  cleaned <- ifelse(df$expo_aricontsetting == "Inconnu", "UNKNOWN", df$expo_aricontsetting)
  return(cleaned)}
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor
# **Some illogical answers from Burundi here. For example, patinfo number BUU-BMA-ZSU-001C1, lets call him John, has an ID entry for expo_ID1. 
# **This means John has been in contact with another CoV2-positive person on the linelist, let's call her Jalanda.
# **Yet, for expo_aricontsetting, John is unable to specify WHERE he was in contact with Jalanda. 
# **He lists "Inconnu" in the expo_aricontsetting space. This does not make sense. Problem is replicated with a few patients. 

clean_columns[['expo_other']] =  function(df) { 
  cleaned <- ifelse(df$expo_other == "Inconnu", "UNKNOWN", df$expo_other)
   return(cleaned)}
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = yes_no_clean('expo_contact_case')
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

clean_columns[['Lab_coll']] = yes_no_clean('Lab_coll')
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = uppercase('Lab_type')
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL, factor

clean_columns[['Lab_datetaken']] = clean_dates_1('Lab_datetaken')
# Date when COVID19 Lab sample was taken, date, MM/DD/YYYY

clean_columns[['Lab_performed']] = uppercase('Lab_performed') 
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = uppercase('Lab_result')
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = clean_dates_1('Lab_resdate')
# Date when COVID19 Lab result was returned, date, MM/DD/YYYY

clean_columns[['Lab_other']] = map_to('Lab_other')
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_other_samples_result_list']] = map_to('Lab_otherres')
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = clean_dates_1('patcourse_datedischarge')
# Date when patient was discharged (if alive and hospitalized), date, MM/DD/YYYY
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized

#############################################
# Quality checking data
#############################################


# for(var in 2:length(names(clean))){
#   print(names(clean)[var])
#   print(table(as.factor(as.character(clean[,var])),useNA="always"))
# }
