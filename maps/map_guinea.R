#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Country:
#     guinea
#
#############################################

# Install pkgs
require("stringr")
require("dplyr")
require("lubridate")

# Load utility functions
source("~/data-cleaning/notebooks/utils/utils.R")


# --------------------------------------
# initialize
clean_columns = list()

columns_used = list()

# --------------------------------------
# map variables 

clean_columns[['patinfo_ID']] = function(df) as.character(paste(df[["N°"]],df[["ID Local Covid"]],sep="_"))
columns_used = append(columns_used,"ID Local Covid")
columns_used = append(columns_used,"N°")
# Anonymized patient ID, convert to character string (some countries include letters in the ID)

clean_columns[['report_date']] = function(df) as.Date(df$`Date de notification`, '%Y-%m-%d')
columns_used = append(columns_used,"Date de notification")
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) {
  df$'Age en annees'[which(as.numeric(df$'Age en annees')>115)]<-NA
  as.numeric(df$'Age en annees')
}
columns_used = append(columns_used,"Age en annees")
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = keep_empty
# Age given only in years

clean_columns[['patcourse_status']] =  function(df) 
  ifelse(df$'COVID-19 Cas Décédés'=="1" |df$'Statut du cas au moment de la notification'=="decede","DEAD",ifelse(df$'COVID-19 Cas Gueris'=="1","RECOVERED", ifelse(df$'Statut du cas au moment de la notification'=="vivant","ALIVE",NA)))
columns_used = append(columns_used,"COVID-19 Cas Décédés")
columns_used = append(columns_used,"Statut du cas au moment de la notification")
columns_used = append(columns_used,"COVID-19 Cas Gueris")
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification
# They are doing this in a very convoluted fashion. Be sure to check for new columns!

clean_columns[['patinfo_sex']] = function(df) ifelse(df$Sexe=="feminin", "F",ifelse(df$Sexe=="masculin","M",NA))
columns_used = append(columns_used,"Sexe")
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) toupper(df[,'Region'])
columns_used = append(columns_used,"Region")
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,'District sanitaire'])
columns_used = append(columns_used,"District sanitaire")
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df) 
{ HCW.list<-c("agent de santé","etudiant de santé","infirmier","médecin")
ifelse(df$Profession=="NA",NA,
       ifelse(df$Profession %in% HCW.list,"Y","N"))}
columns_used = append(columns_used,"Profession")
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = map_to('Profession')
columns_used = append(columns_used,"Profession")
# Patient occupation, character string (factor)

#clean_columns[['expo_travel']] = map_to('expo_travel')
# Patient history of travel?, Y/N, factor
# Currently no information

#clean_columns[['expo_travel_country']] = map_to('expo_travel_country')
# Country(ies) patient travelled to, character string (comma-separated list)
# Currently no information

#clean_columns[['expo_date_departure']] = function(df) as.Date(df[,'expo_date_departure'], '%Y-%m-%d')
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD
# Currently no information

#clean_columns[['pat_symptomatic']] = function(df) ifelse(df[,'patsympt_fever']==1 |df[,'patsympt_sorethroat']==1|df[,'patsympt_cough']==1|df[,'patsympt_runnynose']==1|df[,'patsympt_short']==1|df[,'patsympt_other']==1,"Y",ifelse(df[,'patsympt_fever']==0 &df[,'patsympt_sorethroat']==0 & df[,'patsympt_cough']==0 & df[,'patsympt_runnynose']==0 & df[,'patsympt_short']==0 & df[,'patsympt_other']==0,"N",NA))
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)
# Currently no information

#clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'
# Currently no information

#clean_columns[['patcourse_dateonset']] = function(df) as.Date(df[,'patcourse_dateonset'], '%Y-%m-%d')
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')
# Currently no information

#clean_columns[['expo_sourcecaseids']] = function(df) paste(df[,'expo_ID1'],df[,'expo_ID2'],df[,'expo_ID3'],df[,'expo_ID4'],sep=",")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template
# Currently no information

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor
# Currently no information

clean_columns[['report_classif']] = function(df) 
  ifelse(df[,'Co19 Classification finale du cas']=="Confirme par laboratoire","CONFIRMED",toupper(df[,'Co19 Classification finale du cas']))
columns_used = append(columns_used,"Co19 Classification finale du cas")
columns_used = append(columns_used,"Confirme par laboratoire")
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor
# For now, only confirmed cases listed

clean_columns[['patcourse_datedeath']] = function(df) 
{
  death.date<-as.Date(df$`Date de Décès a notification`, '%Y-%m-%d')
  rep.date<-as.Date(df$`Date de notification`, '%Y-%m-%d')
  as.Date(ifelse(((df$`Statut du cas au moment de la notification`=="decede"|df$'COVID-19 Cas Décédés'=="1") & is.na(death.date)==TRUE),as.character(rep.date),
         ifelse((df$`Statut du cas au moment de la notification`=="decede"|df$'COVID-19 Cas Décédés'=="1"),as.character(death.date),NA)))
}
columns_used = append(columns_used,"Date de Décès a notification")
columns_used = append(columns_used,"Statut du cas au moment de la notificatio")
columns_used = append(columns_used,"COVID-19 Cas Décédés")
# Date of Death for decesased patients, character, YYYY-MM-DD, NA if Alive

#clean_columns[['patinfo_resadmin3']] = map_to('patinfo_resadmin3')
# Place of residence admin level 3 (Health Zone/Town), factor
# sparse information (sub-prefecture)

#clean_columns[['patinfo_resadmin4']] = map_to('patinfo_resadmin4')
# Place of residence admin level 4 (Village), factor
# sparse information

clean_columns[['report_orginst']] = uppercase('Structure notifiant le cas')
columns_used = append(columns_used,"Structure notifiant le cas")
# Reporting health facility/institution, factor
# not fully standardized

clean_columns[['patinfo_idadmin1']] = map_to('Organisation unit name')
columns_used = append(columns_used,"Organisation unit name")
# Where the case was diagnosed, admin level 1 (Province), factor
# Not clear that this is the correct information

#clean_columns[['patinfo_idadmin2']] = map_to('patinfo_idadmin2')
# Where the case was diagnosed, admin level 2 (District), factor
# Not clear that this information is given

#clean_columns[['report_pointofentry']] = map_to('report_pointofentry')
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 
# Currently no information

#clean_columns[['report_pointofentry_date']] = function(df) as.Date(df[,'report_pointofentry_date'], '%Y-%m-%d')
# Date detected at point of entry, character, YYYY-MM-DD
# Currently no information

#clean_columns[['consultation_dateHF']] = function(df) as.Date(df[,'consultation_dateHF'], '%Y-%m-%d')
# Date of first consultation at this Health Facility, character, YYYY-MM-DD
# Currently no information

#clean_columns[['patcourse_admit']] = map_to('patcourse_admit')
# Admission to hospital?, Y/N, factor
# Currently no information

#clean_columns[['patcourse_presHCF']] = function(df) as.Date(df[,'patcourse_presHCF'], '%Y-%m-%d')
# For this episode, date first admitted to hospital, character, YYYY-MM-DD
# Currently no information

#clean_columns[['patcourse_comp']] = map_to('patcourse_comp')
# Other clinical complications, character string (comma-separated list)
# Currently no information

#clean_columns[['patsympt_fever']] = map_to('patsympt_fever')
# History of fever or chills?, Y/N, factor
# Currently no information

#clean_columns[['patsympt_sorethroat']] = map_to('patsympt_sorethroat')
# History of sore throat?, Y/N, factor
# Currently no information

#clean_columns[['patsympt_cough']] = map_to('patsympt_cough')
# History of cough?, Y/N, factor
# Currently no information

#clean_columns[['patsympt_runnynose']] = map_to('patsympt_runnynose')
# History of runny nose?, Y/N, factor
# Currently no information

#clean_columns[['patsympt_short']] = map_to('patsympt_short')
# History of shortness of breath?, Y/N, factor
# Currently no information

#clean_columns[['patsympt_other']] = map_to('patsympt_other')
# Other signs or symptoms, character string (comma-separated list)
# Currently no information

#clean_columns[['Comcond_preexist1']] = map_to('Comcond_preexist1')
# Patient has pre-existing conditions?, Y/N, factor
# Currently no information

#clean_columns[['Comcond_preexist']] = map_to('Comcond_preexist')
# Patient's pre-existing conditions, character string (comma-separated list)
# Currently no information

#clean_columns[['expo_visit_healthcare']] = map_to('expo_visit_healthcare')
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor
# Currently no information

#clean_columns[['expo_ari']] = map_to('expo_ari')
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor
# Currently no information

#clean_columns[['expo_aricontsetting']] = map_to('expo_aricontsetting')
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor
# Currently no information

#clean_columns[['expo_other']] = map_to('expo_other')
# Other exposures, character string (comma-separated list)
# Currently no information

#clean_columns[['expo_contact_case']] = map_to('expo_contact_case')
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')
# Currently no information

#clean_columns[['expo_ID1']] = map_to('expo_ID1')
# ID of confirmed or probable case 1, numeric
# Currently no information

#clean_columns[['expo_ID2']] = map_to('expo_ID2')
# ID of confirmed or probable case 2, numeric
# Currently no information

#clean_columns[['expo_ID3']] = map_to('expo_ID3')
# ID of confirmed or probable case 3, numeric
# Currently no information

#clean_columns[['expo_ID4']] = map_to('expo_ID4')
# ID of confirmed or probable case 4, numeric
# Currently no information

#clean_columns[['expo_arisetting']] = map_to('expo_arisetting')
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)
# Currently no information

clean_columns[['Lab_coll']] = function(df)
 ifelse(df$'Co19 Classification finale du cas'=="Confirme par laboratoire","Y",df$'Co19 Classification finale du cas')
columns_used = append(columns_used,"Co19 Classification finale du cas")
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = function(df) ifelse(is.na(df$'Co19 Date prelevement ecouvillon nasopharynge')==FALSE,"NASOPHARYNGEAL SWAB",NA)
columns_used = append(columns_used,"Co19 Date prelevement ecouvillon nasopharynge")
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = function(df) as.Date(df[,'Co19 Date prelevement ecouvillon nasopharynge'], '%Y-%m-%d')
columns_used = append(columns_used,"Co19 Date prelevement ecouvillon nasopharynge")
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

#clean_columns[['Lab_performed']] = function(df) toupper(df[,'Lab_performed']) 
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor
# Currently no information

clean_columns[['Lab_result']] = function(df) 
  ifelse(df[,'Co19 Classification finale du cas']=="Confirme par laboratoire","POSITIVE",df$'Co19 Classification finale du cas')
columns_used = append(columns_used,"Co19 Classification finale du cas")
columns_used = append(columns_used,"Confirme par laboratoire")
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = function(df) as.Date(df[,'Co19 Date résultat Laboratoire Covid-19'], '%Y-%m-%d')
columns_used = append(columns_used,"Co19 Date résultat Laboratoire Covid-19")
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

clean_columns[['Lab_other']] = function(df) ifelse(is.na(df$'Co19 ORF1ab (VIC)')==FALSE,"RT-PCR-Ct",NA)
columns_used = append(columns_used,"Co19 ORF1ab (VIC)")
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_otherres']] = map_to('Co19 ORF1ab (VIC)')
columns_used = append(columns_used,"Co19 ORF1ab (VIC)")
# Other lab sample result(s), character string (comma-separated list)

#clean_columns[['patcourse_datedischarge']] = function(df) as.Date(df[,'patcourse_datedischarge'], '%Y-%m-%d')
# Date when patient was discharged (if alive and hospitalized), character, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized
# Currently no information

# --------------------------------------
# generate colname lists for qualilty control

# remove duplicate column entries in the list of used columns (simply for conciseness)
columns_used = unique(columns_used)

# hardcode columns which are intentionally NOT used
columns_unused = list("Numero EpId", "Co19 Nom du laboratoire Prefecture-Region", "Nom", "Prenom", "Numéro de téléphone du cas/personne responsable", "Autre profession", "Sous-Prefecture", "Quartier/district", "Village/Secteur", "Co19 Date acheminem labo ref ecouvillon oropharyng", "Co19 Ctl Interne (Cy5)", "Co19 Date de feedback au district", "Co19 Date de feedback du labo a ANSS", "Co19 Date issue", "Cas investigue", "COVID-19 Cas Gueris, Masculin", "COVID-19 Cas Gueris, Inconnu", "COVID-19 Cas Gueris, Feminin", "COVID-19 Cas Perdus de Vue", "COVID-19 Cas Décédés, Masculin", "COVID-19 Cas Décédés, Feminin")

# You can use the following line of code to produce the list of intentionally unused columns
# given that you have a raw dataframe of which you already use all columns you want:
# colnames(raw)[!colnames(raw) %in% columns_used]
# BUT THIS HAS TO BE HARDCODED the unused columns!
# the following code puts this list in a format you can copy to hardcode above:
# cat(paste(paste0('"', colnames(raw)[!colnames(raw) %in% columns_used], '"'), collapse = ", "), "\n")
