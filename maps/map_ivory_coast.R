#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Country:
#     ivory_coast
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

# when real data are uploaded, be sure NAs are still "NA" (or not)

clean_columns[['patinfo_ID']] = function(df) {df$'ID  Unique du cas'}
columns_used = append(columns_used,"ID  Unique du cas")
columns_used = append(columns_used,"ID")
# Anonymized patient ID, convert to character string (some countries include letters in the ID)

clean_columns[['report_date']] = function(df) {
  # replace with proper date format
  df$`Date de raportage`<-ifelse(nchar(df$`Date de raportage`)==5,format(as.Date(as.numeric(df$`Date de raportage`), origin = "1899-12-30"), '%Y-%m-%d'),df$`Date de raportage`)
}
columns_used = append(columns_used,"Date de raportage")
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = map_to('NOMS')
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) 
{ df$'Age (années)'<-gsub("NA",NA,df$'Age (années)')
  df$'Age (années)'<-gsub("NO",NA,df$'Age (années)')
  df$'Age (années)'<-gsub("TOURE",NA,df$'Age (années)')
  df$'Age (années)'<-gsub("Missing",NA,df$'Age (années)')
  df$'Age (années)'<-gsub("1 6mois",1,df$'Age (années)')
  df$'Age (années)'<-gsub("6MOIS",0,df$'Age (années)')
  df$'Age (années)'<-gsub(-1,NA,df$'Age (années)')
  df$'Age (mois)'<-gsub("NA",NA,df$'Age (mois)')
  df$'Age (mois)'<-gsub("A",NA,df$'Age (mois)')
  df$'Age (mois)'[which(as.numeric(df$'Age (mois)')>35)]<-NA
  df$'Age (mois)'[which(df$'Age (années)'=="6MOIS")]<-6
  df$'Age (années)'[which(as.numeric(df$'Age (mois)')>23)]<-2
  df$'Age (années)'[which(as.numeric(df$'Age (mois)')>11)]<-1
  df$'Age (années)'[which(as.numeric(df$'Age (mois)')<12)]<-0
  df$'Age (mois)'[which(as.numeric(df$'Age (mois)')>23)]<-NA
  ifelse(df$'Age (années)'!="NA",as.numeric(df$'Age (années)'),ifelse(is.na(df$'Age (années)') & is.na(df$'Age (mois)'),NA,
                                                         ifelse(as.numeric(df$'Age (mois)')>0 & df$'Age (années)'!="1",0,1)))
  as.numeric(as.character(df$`Age (années)`))
}
columns_used = append(columns_used,"Age (années)")
columns_used = append(columns_used,"Age (mois)")
# Age at intake in years, 0 for infants < 12 months, numeric
# Need to change all "NA" to NA, and months > 0 to years=0
# table(raw$'Age (années)',raw$'Age (mois)',useNA = "always")

clean_columns[['patinfo_ageonset_months']] = function(df)
  { df$'Age (années)'<-gsub("NA",NA,df$'Age (années)')
  df$'Age (années)'<-gsub("NO",NA,df$'Age (années)')
  df$'Age (années)'<-gsub("TOURE",NA,df$'Age (années)')
  df$'Age (années)'<-gsub("Missing",NA,df$'Age (années)')
  df$'Age (années)'<-gsub("1 6mois",1,df$'Age (années)')
  df$'Age (années)'<-gsub("6MOIS",0,df$'Age (années)')
  df$'Age (années)'<-gsub(-1,NA,df$'Age (années)')
  df$'Age (mois)'<-gsub("NA",NA,df$'Age (mois)')
  df$'Age (mois)'<-gsub("A",NA,df$'Age (mois)')
  df$'Age (mois)'[which(as.numeric(df$'Age (mois)')>35)]<-NA
  df$'Age (mois)'[which(df$'Age (années)'=="6MOIS")]<-6
  df$'Age (années)'[which(as.numeric(df$'Age (mois)')>23)]<-2
  df$'Age (années)'[which(as.numeric(df$'Age (mois)')>11)]<-1
  df$'Age (années)'[which(as.numeric(df$'Age (mois)')<12)]<-0
  df$'Age (mois)'[which(as.numeric(df$'Age (mois)')>23)]<-NA
  ifelse((is.na(df$'Age (années)')==FALSE & as.numeric(df$'Age (années)')==1),
         as.numeric(df$'Age (mois)')+12,
         df$'Age (mois)')  
  as.numeric(df$'Age (mois)')
}
columns_used = append(columns_used,"Age (années)")
columns_used = append(columns_used,"Age (mois)")
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# table(raw$'Age (années)',raw$'Age (mois)',useNA = "always")

clean_columns[['patcourse_status']] =  function(df) 
{ df$'Résultat de la maladie (Alive / Dead)'<-gsub("Guérit","RECOVERED",df$'Résultat de la maladie (Alive / Dead)')
  df$'Résultat de la maladie (Alive / Dead)'<-gsub("Allive","ALIVE",df$'Résultat de la maladie (Alive / Dead)')
  toupper(df[,'Résultat de la maladie (Alive / Dead)'])
}
columns_used = append(columns_used,"Résultat de la maladie (Alive / Dead)")
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = function(df) 
{ SEXMF<-toupper(df$'Sexe (M/F)')
 SEXMF[which(SEXMF=="MISSING")]<-NA
ifelse(df$'Sexe (M/F)'=="NA",NA,SEXMF)
}
columns_used = append(columns_used,"Sexe (M/F)")
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) 
{ df$'Lieu de residence niveau admin 1 (province)'<-gsub("ABIDJAN 1","ABIDJAN1",df$'Lieu de residence niveau admin 1 (province)')
df$'Lieu de residence niveau admin 1 (province)'<-gsub("L'INDENIE DJUABLIN","INDENIE-DJUABLIN",df$'Lieu de residence niveau admin 1 (province)')
df$'Lieu de residence niveau admin 1 (province)'<-gsub("LôH -DJIBOUA","LôH-DJIBOUA",df$'Lieu de residence niveau admin 1 (province)')
df$'Lieu de residence niveau admin 1 (province)'<-toupper(df[,'Lieu de residence niveau admin 1 (province)'])
level_key <- c(ABIDAJN1 = "ABIDJAN 1", ABIDJAN1 = "ABIDJAN 1", ABIDJN1 = "ABIDJAN 1", 'ABIEDJAN 1'="ABIDJAN 1",'ABIDJAN  1'="ABIDJAN 1",
               ABIDAN2="ABIDJAN 2",'ABIDJAN  2'="ABIDJAN 2",ABIDJAN2="ABIDJAN 2",'ABIDLAN 2'="ABIDJAN 2",
               GÔH ="GOH", 
               'GRAND PONT'="GRANDS PONTS",'GRANDS-PONTS'="GRANDS PONTS",
               'HAUT-SASSANDRA'= "HAUT SASSANDRA",
               'LÔH-DJIBOUA'="LOH DJIBOUA",
               MÉ="ME",
               'SAN_PEDRO'="SAN PEDRO",
               'SUD COMEO'="SUD COMOE",'SUD-COMOE'="SUD COMOE",
               TONPKI="TONKPI")
df$'Lieu de residence niveau admin 1 (province)'<-recode(df$'Lieu de residence niveau admin 1 (province)', !!!level_key, .default=df$'Lieu de residence niveau admin 1 (province)')
df$'Lieu de residence niveau admin 1 (province)'
}
columns_used = append(columns_used,"Lieu de residence niveau admin 1 (province)")
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,'Lieu de residence niveau admin 2  (district)'])
columns_used = append(columns_used,"Lieu de residence niveau admin 2  (district)")
# Patient residence (district), Standardize names to all uppercase, factor
# currently empty

clean_columns[['patinfo_occus']] = keep_empty
# columns_used = append(columns_used,"Profession du patient  (specifier)")
# Patient occupation is healthcare worker?, Y/N, factor
# The list of occupations in healthcare is not completely exhaustive. Needs Quality Control.

clean_columns[['patinfo_occus_specify']] = map_to('Profession du patient  (specifier)')
columns_used = append(columns_used,"Profession du patient  (specifier)")
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = function(df) 
  {unknown<-c("ASSISTANTE DE DIRECTION","Don't Know","NA")
  df$`Le patient a-t-il voyagé à l'étranger au cours des 14 jours précédant l'apparition des symptômes?`[df$`Le patient a-t-il voyagé à l'étranger au cours des 14 jours précédant l'apparition des symptômes?` %in% unknown]<-NA
  df$`Le patient a-t-il voyagé à l'étranger au cours des 14 jours précédant l'apparition des symptômes?`[toupper(df$`Le patient a-t-il voyagé à l'étranger au cours des 14 jours précédant l'apparition des symptômes?`) =="YES"]<-"Y"
  df$`Le patient a-t-il voyagé à l'étranger au cours des 14 jours précédant l'apparition des symptômes?`[toupper(df$`Le patient a-t-il voyagé à l'étranger au cours des 14 jours précédant l'apparition des symptômes?`) =="NO"]<-"N"  
  df$`Le patient a-t-il voyagé à l'étranger au cours des 14 jours précédant l'apparition des symptômes?`
  }

columns_used = append(columns_used,"Le patient a-t-il voyagé à l'étranger au cours des 14 jours précédant l'apparition des symptômes?")
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = map_to('Précisez le pays visité')
columns_used = append(columns_used,"Précisez le pays visité")
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = 
  function(df) 
  { df$`Précisez la date de départ du pays (jj / mm / aaaa)`<-ifelse(nchar(df$`Précisez la date de départ du pays (jj / mm / aaaa)`) > 10,NA,df$`Précisez la date de départ du pays (jj / mm / aaaa)`)
    df$`Précisez la date de départ du pays (jj / mm / aaaa)`<-ifelse(is.na(df$`Précisez la date de départ du pays (jj / mm / aaaa)`),NA,
                                                              ifelse(nchar(df$`Précisez la date de départ du pays (jj / mm / aaaa)`) == 5 & as.numeric(df$`Précisez la date de départ du pays (jj / mm / aaaa)`) > 43000,format(as.Date(as.numeric(df$`Précisez la date de départ du pays (jj / mm / aaaa)`), origin = "1899-12-30"), '%Y-%m-%d'),
                                                              ifelse(nchar(df$`Précisez la date de départ du pays (jj / mm / aaaa)`) == 5 & as.numeric(df$`Précisez la date de départ du pays (jj / mm / aaaa)`) < 43000,format(as.Date(as.numeric(df$`Précisez la date de départ du pays (jj / mm / aaaa)`), origin = "1904-01-01"), '%Y-%m-%d'),
                                                              df$`Précisez la date de départ du pays (jj / mm / aaaa)`)))    
    df$`Précisez la date de départ du pays (jj / mm / aaaa)`[which(df$`Précisez la date de départ du pays (jj / mm / aaaa)`=="2020-07-18" & df$`ID  Unique du cas`=="20022904" )] <- "2020-06-18"
    df$`Précisez la date de départ du pays (jj / mm / aaaa)`[which(df$`Précisez la date de départ du pays (jj / mm / aaaa)`=="2020-07-19" & df$`ID  Unique du cas`=="20030261" )] <- "2020-06-19"
    df$`Précisez la date de départ du pays (jj / mm / aaaa)`[which(df$`Précisez la date de départ du pays (jj / mm / aaaa)`=="2020-08-02" & df$`ID  Unique du cas`=="20022109" )] <- "2020-07-02"
    df$`Précisez la date de départ du pays (jj / mm / aaaa)`[which(df$`Précisez la date de départ du pays (jj / mm / aaaa)`=="2021-06-21" & df$`ID  Unique du cas`=="20010413" )] <- "2020-06-21"    
    df$`Précisez la date de départ du pays (jj / mm / aaaa)`
    }
columns_used = append(columns_used,"Précisez la date de départ du pays (jj / mm / aaaa)")
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD
# *** This date gets converted to a number by EXCEL. Need to check with real data. 
# function(df) as.Date(df[,'Précisez la date de départ du pays (jj / mm / aaaa)\"'], '%m/%d/%Y')

clean_columns[['pat_symptomatic']] = function(df) 
  { df$`Autre signe / symptôme, précisez`<-toupper(df$`Autre signe / symptôme, précisez`)
    df$`Antécédents de fièvre / frissons`<-toupper(df$`Antécédents de fièvre / frissons`)
    df$`Mal de gorge`<-toupper(df$`Mal de gorge`)
    df$`Toux`<-toupper(df$`Toux`)
    df$`dyspnée`<-toupper(df$`dyspnée`)
    df$`rhinorrhée`<-toupper(df$`rhinorrhée`)
    sympt_level_key <- c(YES = "Y", OUI = "Y", 
                    FIEVRE="Y",`FIEVRE ET DOULEURS`="Y",`FRISSONS`="Y", 
                    TOUX="Y",
                 NO="N", NON="N",
                 `DON'T KNOW`=NA, "NA"=NA, RAS=NA,NSP=NA,"NE SAIS PAS"=NA, "INCONNU"=NA)
    df$`Autre signe / symptôme, précisez`<-toupper(df$`Autre signe / symptôme, précisez`)
    df$`Antécédents de fièvre / frissons` <- recode(df$`Antécédents de fièvre / frissons`, !!!sympt_level_key)
    df$`Mal de gorge` <- recode(df$`Mal de gorge`, !!!sympt_level_key)
    df$`Toux` <- recode(df$`Toux`, !!!sympt_level_key)
    df$`rhinorrhée` <- recode(df$`rhinorrhée`, !!!sympt_level_key)
    df$`dyspnée` <- recode(df$`dyspnée`, !!!sympt_level_key)

    df$symptomsYN<-ifelse(df$`Antécédents de fièvre / frissons`=="Y" | df$`Mal de gorge`=="Y" | df$Toux=="Y" | df$`rhinorrhée`=="Y" | df$`dyspnée`== "Y" | is.na(df$`Autre signe / symptôme, précisez`)==F,"Y",
       ifelse(df$`Antécédents de fièvre / frissons`== "N" & df$`Mal de gorge`== "N" & df$Toux== "N" & df$`rhinorrhée`== "N" & df$`dyspnée`== "N" & is.na(df$`Autre signe / symptôme, précisez`)==T,
              "N",NA))
     df$symptomsYN
}
columns_used = append(columns_used,"Autre signe / symptôme, précisez")

# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)
# sum(table(raw$'Autre signe / symptôme specify'))

clean_columns[['pat_contact']] = function(df)
  { level_key <- c(YES = "Y", yes = "Y", Yes = "Y", OUI="Y",Oui="Y",oui="Y",
                     NO="N", No="N",no="N", N0="N", NON = "N",Non="N",non="N",
                     `Don't Know`=NA,INCONNU=NA,NSP=NA,"NE SAIT PAS"=NA, 'NA'=NA,
                   PCR=NA)
  df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?` <- recode(df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?`, !!!level_key)
  df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?`
}
columns_used = append(columns_used,"Le patient a-t-il été en contact avec un cas probable ou confirmé?")
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'
# table(raw$`Le patient a-t-il été en contact avec un cas probable ou confirmé?`)

clean_columns[['patcourse_dateonset']] = function(df){
df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`[c(grep("[A-Z]",df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`))]<-NA
df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`<-ifelse(is.na(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`),NA,
                                                                       ifelse(nchar(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`) > 43000,format(as.Date(as.numeric(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`), origin = "1899-12-30"), '%Y-%m-%d'),
                                                                              ifelse(nchar(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`) < 43000,format(as.Date(as.numeric(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`), origin = "1904-01-01"), '%Y-%m-%d'),
                                                                                     NA))) 
df$`Date de raportage`<-ifelse(nchar(df$`Date de raportage`)==5,format(as.Date(as.numeric(df$`Date de raportage`), origin = "1899-12-30"), '%Y-%m-%d'),df$`Date de raportage`)
df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`[which(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`>df$`Date de raportage`)]<-NA
df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`[which(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`=="2018-05-20")]<-NA
df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`
}
# columns_used = append(columns_used,"Date d'apparition des premiers symptômes (jj / mm / aaaa)")
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')
# *** This date gets converted to a number by EXCEL. Need to check with real data. 
# function(df) as.Date(df$`Date d'apparition des premiers symptômes (jj / mm / aaaa)`, '%m/%d/%Y')

clean_columns[['expo_sourcecaseids']] = keep_empty
# function(df) paste(df$`Numéro d'identification du cas confirmé ou probable 1`,df$`Numéro d'identification du cas confirmé ou probable 2`,df$`Numéro d'identification du cas confirmé ou probable 3`,df$`Numéro d'identification du cas confirmé ou probable 4`,sep=",")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template
# Commented becuase these contain patient NAMES!

clean_columns[['patcourse_severity']] = function(df)
  { df$Gravité<-gsub("NA",NA,df$Gravité)
    df$Gravité
}
columns_used = append(columns_used,"Gravité")

# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor
# table(raw$Gravité), empty for now

clean_columns[['report_classif']] = function(df) 
  { df$'Classification'<-toupper(df$`Classification`)
    df$'Classification'<-gsub("NA",NA,df$`Classification`)
    df$'Classification'<-gsub("DON'T KNOW",NA,df$`Classification`)
    df$'Classification'[which(grepl("ALERT", df$'Classification',fixed=TRUE))]<-"ALERT"
    df$'Classification'[which(grepl("CONTACT", df$'Classification',fixed=TRUE))]<-"CONTACT"
    df$'Classification'[which(grepl("SUPS", df$'Classification',fixed=TRUE))]<-"SUSPECTED"
    df$'Classification'[which(grepl("CONTR", df$'Classification',fixed=TRUE))]<-"CONTROLE"
    df$`Laboratoire effectué` <- gsub("NA",NA,df$`Laboratoire effectué`)
    df$`Résultat de laboratoire pour COVID19` <- gsub("NA",NA,df$`Résultat de laboratoire pour COVID19`)
    ifelse(is.na(df$'Résultat de laboratoire pour COVID19') & is.na(df$Classification) & df$`Laboratoire effectué`=="PCR","RESULTS PENDING",toupper(df$`Classification`))
}
columns_used = append(columns_used,"Classification")
columns_used = append(columns_used,"Laboratoire effectué")
columns_used = append(columns_used,"Résultat de laboratoire pour COVID19")
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor
# table(df$`Classification`)
# *** This includes "ALERT" and "CONTACT", which are not accepted entries. Ask what to do.

clean_columns[['patcourse_datedeath']] = function(df) 
  { df$`Date de décès (en cas de décès) (jj / mm / aaaa)`[c(grep("[A-Z]",df$`Date de décès (en cas de décès) (jj / mm / aaaa)`))]<-NA
  df$`Date de décès (en cas de décès) (jj / mm / aaaa)`<-ifelse(is.na(df$`Date de décès (en cas de décès) (jj / mm / aaaa)`),NA,
                                                         ifelse(nchar(df$`Date de décès (en cas de décès) (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date de décès (en cas de décès) (jj / mm / aaaa)`) > 43000,format(as.Date(as.numeric(df$`Date de décès (en cas de décès) (jj / mm / aaaa)`), origin = "1899-12-30"), '%Y-%m-%d'),
                                                         ifelse(nchar(df$`Date de décès (en cas de décès) (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date de décès (en cas de décès) (jj / mm / aaaa)`) < 43000,format(as.Date(as.numeric(df$`Date de décès (en cas de décès) (jj / mm / aaaa)`), origin = "1904-01-01"), '%Y-%m-%d'),
                                                         NA))) 
  df$`Date de décès (en cas de décès) (jj / mm / aaaa)`
}
columns_used = append(columns_used,"Date de décès (en cas de décès) (jj / mm / aaaa)")
# Date of Death for decesased patients, character, YYYY-MM-DD, NA if Alive
# *** this has been converted manually, need to convert directly from excel (shit of one day)
# function(df) as.Date(df[,'patcourse_datedeath'], '%Y-%m-%d')

clean_columns[['patinfo_resadmin3']] = keep_empty
#   function(df) 
#   { df$`Lieu de residence niveau admin 3 ( Zone de Santée/Ville)`<-gsub("NA",NA,df$`Lieu de residence niveau admin 3 ( Zone de Santée/Ville)`)
#     df$`Lieu de residence niveau admin 3 ( Zone de Santée/Ville)`<-toupper(df$`Lieu de residence niveau admin 3 ( Zone de Santée/Ville)`)
#     df$`Lieu de residence niveau admin 3 ( Zone de Santée/Ville)`
# }
# columns_used = append(columns_used,"Lieu de residence niveau admin 3 ( Zone de Santée/Ville)")
# Place of residence admin level 3 (Health Zone/Town), factor
# *** empty for now
# map_to('`Lieu de residence niveau admin 3 ( Zone de Santée/Ville)`')

clean_columns[['patinfo_resadmin4']] = 
#   function(df) 
#   { df$`Lieu de residence niveau adm 4 (village)`<-gsub("NA",NA,df$`Lieu de residence niveau adm 4 (village)`)
#     df$`Lieu de residence niveau adm 4 (village)`
# }
# columns_used = append(columns_used,"Lieu de residence niveau adm 4 (village)")
# Place of residence admin level 4 (Village), factor
# *** empty for now
# map_to(`Lieu de residence niveau adm 4 (village)`)

clean_columns[['report_orginst']] = map_to('Établissement / établissement de santé déclarant')
columns_used = append(columns_used,"Établissement / établissement de santé déclarant")
# Reporting health facility/institution, factor
# table(raw$`Établissement / établissement de santé déclarant`)

clean_columns[['patinfo_idadmin1']] = function(df)
  { df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)` <-toupper(df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)` )
    df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)` <- gsub("ABIDJAN 1","ABIDJAN1",df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)`)
    df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)` <- gsub("LôH -DJIBOUA","LÔH-DJIBOUA",df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)`)
    df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)` <- gsub("LôH -DJIBOUA","LÔH-DJIBOUA",df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)`)
    df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)` <- gsub("SAN_PEDRO","SAN PEDRO",df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)`)
    level_key <- c(ABIDJAJN1="ABIDJAN",ABIDAN1="ABIDJAN",ABIADJAN1="ABIDJAN", ABIDAJN1 = "ABIDJAN", ABIDJAN1 = "ABIDJAN", ABIDJN1 = "ABIDJAN", 'ABIEDJAN'="ABIDJAN",
                   ABIDAN2="ABIDJAN",ABIDAN2="ABIDJAN",'ABIDJAN  2'="ABIDJAN",ABIDJAN2="ABIDJAN",'ABIDLAN 2'="ABIDJAN",
                   GÔH ="GOH", 
                   'GRAND PONT'="GRANDS PONTS",'GRANDS-PONTS'="GRANDS PONTS",
                   'HAUT-SASSANDRA'= "HAUT SASSANDRA",
                   'LÔH-DJIBOUA'="LOH DJIBOUA",
                   MÉ="ME",
                   'SAN_PEDRO'="SAN PEDRO",
                   'SUD COMEO'="SUD COMOE",'SUD-COMOE'="SUD COMOE",
                   TONPKI="TONKPI")
    df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)`<-recode(df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)`, !!!level_key, .default=df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)`)
    df$`Où le cas a été diagnostiqué,  niveau admin  1 (province)`
}
columns_used = append(columns_used,"Où le cas a été diagnostiqué,  niveau admin  1 (province)")
# Where the case was diagnosed, admin level 1 (Province), factor
# table(raw$`Où le cas a été diagnostiqué,  niveau admin  1 (province)`)


clean_columns[['patinfo_idadmin2']] = function(df) toupper(df$`Où le cas a été diagnostiqué, niveau admin  2 (district)`)
columns_used = append(columns_used,"Où le cas a été diagnostiqué, niveau admin  2 (district)")
# Where the case was diagnosed, admin level 2 (District), factor
# table(raw$`Où le cas a été diagnostiqué, niveau admin  2 (district)`)
# *** currently empty

clean_columns[['report_pointofentry']] = function(df)
  { level_key <- c(YES = "Y", yes = "Y", Yes = "Y", COCODY="Y", MARCORY="Y", 
                        NO="N", No="N",no="N",
                        `Don't Know`=NA, 'NA'=NA)
    df$`Détecté au point d'entrée` <- recode(df$`Détecté au point d'entrée`, !!!level_key)
    df$`Détecté au point d'entrée` 
}
columns_used = append(columns_used,"Détecté au point d'entrée")
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 
# table(raw$`Détecté au point d'entrée`)

clean_columns[['report_pointofentry_date']] = function(df) 
{df$`Date détecté au point d'entrée (jj/mm/années)`[c(grep("[A-Z]",df$`Date détecté au point d'entrée (jj/mm/années)`))]<-NA
df$`Date détecté au point d'entrée (jj/mm/années)`<-ifelse(is.na(df$`Date détecté au point d'entrée (jj/mm/années)`),NA,
                                                              ifelse(nchar(df$`Date détecté au point d'entrée (jj/mm/années)`) == 5 & as.numeric(df$`Date détecté au point d'entrée (jj/mm/années)`) > 43000,format(as.Date(as.numeric(df$`Date détecté au point d'entrée (jj/mm/années)`), origin = "1899-12-30"), '%Y-%m-%d'),
                                                                     ifelse(nchar(df$`Date détecté au point d'entrée (jj/mm/années)`) == 5 & as.numeric(df$`Date détecté au point d'entrée (jj/mm/années)`) < 43000,format(as.Date(as.numeric(df$`Date détecté au point d'entrée (jj/mm/années)`), origin = "1904-01-01"), '%Y-%m-%d'),
                                                                            NA))) 
df$`Date détecté au point d'entrée (jj/mm/années)`      
}
columns_used = append(columns_used,"Date détecté au point d'entrée (jj/mm/années)")
# Date detected at point of entry, character, YYYY-MM-DD
# *** empty for now
# table(raw$`Date détecté au point d'entrée (jj/mm/années)`)


clean_columns[['consultation_dateHF']] = function(df) 
{ df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`[c(grep("[A-Z]",df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`))]<-NA
df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`<-ifelse(is.na(df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`),NA,
                                                           ifelse(nchar(df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`) == 5 & as.numeric(df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`) > 43000,format(as.Date(as.numeric(df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`), origin = "1899-12-30"), '%Y-%m-%d'),
                                                                  ifelse(nchar(df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`) == 5 & as.numeric(df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`) < 43000,format(as.Date(as.numeric(df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`), origin = "1904-01-01"), '%Y-%m-%d'),
                                                                         NA))) 
df$`Date de raportage`<-ifelse(nchar(df$`Date de raportage`)==5,format(as.Date(as.numeric(df$`Date de raportage`), origin = "1899-12-30"), '%Y-%m-%d'),df$`Date de raportage`)
df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`[which(df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`>df$`Date de raportage`)]<-NA
df$`Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)`  
}
columns_used = append(columns_used,"Date de première consultation à cette Établissement de santé (jj / mm / aaaa)")
# Date of first consultation at this Health Facility, character, YYYY-MM-DD
# *** This date gets converted to a number by EXCEL. Need to check with real data. 
# function(df) as.Date(df[,`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`], '%Y-%m-%d')
# raw$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`

clean_columns[['patcourse_admit']] = function(df)
  {  df$`Admission à l'hôpital?`<-toupper( df$`Admission à l'hôpital?`)
  level_key <- c(YES = "Y", OUI="Y",
                   NO="N", NON="N",
                   `DON'T KNOW`=NA, 'NA'=NA,
                   '2016-05-15'="Y",'2016-05-17'="Y",TABAC=NA,
                   '42505'=NA, '42507'=NA,'42546'=NA)
  df$`Admission à l'hôpital?` <- recode(df$`Admission à l'hôpital?`, !!!level_key)
  df$`Admission à l'hôpital?` 
}
columns_used = append(columns_used,"Admission à l'hôpital?")
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = function(df) 
{ df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`[c(grep("[A-Z]",df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`))]<-NA
df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`<-ifelse(is.na(df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`),NA,
                                                                                           ifelse(nchar(df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`) > 43000,format(as.Date(as.numeric(df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`), origin = "1899-12-30"), '%Y-%m-%d'),
                                                                                                  ifelse(nchar(df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`) < 43000,format(as.Date(as.numeric(df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`), origin = "1904-01-01"), '%Y-%m-%d'),
                                                                                                         NA))) 
df$`Date de première consultation à cette Établissement de santé (jj / mm / aaaa)`  
}
columns_used = append(columns_used,"Pour cet épisode, date de première admission à l'hôpital (jj / mm / aaaa)")
# For this episode, date first admitted to hospital, character, YYYY-MM-DD
# function(df) as.Date(df$`Pour cet épisode date de première admission à l'hôpital (jj / mm / aaaa)`, '%Y-%m-%d')

clean_columns[['patcourse_comp']] = function(df) {df$`Autres complications cliniques (specifier)`}
columns_used = append(columns_used,"Autres complications cliniques (specifier)")
# Other clinical complications, character string (comma-separated list)
# raw$`Autres complications cliniques (specifier)`

clean_columns[['patsympt_fever']] = function(df)
  { df$`Antécédents de fièvre / frissons`<-toupper(df$`Antécédents de fièvre / frissons`)
  sympt_level_key <- c(YES = "Y", OUI="Y",FIEVRE="Y",`FIEVRE ET DOULEURS`="Y", 
                       NO="N",NON="N",
                       `DON'T KNOW`=NA, 'NA'=NA)
    df$`Antécédents de fièvre / frissons` <- recode(df$`Antécédents de fièvre / frissons`, !!!sympt_level_key)
    df$`Antécédents de fièvre / frissons`[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"FIEVRE"))]<-"Y"
    df$`Antécédents de fièvre / frissons`[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"FRISSON"))]<-"Y"    
    df$`Antécédents de fièvre / frissons`[which(str_detect(toupper(df$`Antécédents de fièvre / frissons`),"FIEVRE"))]<-"Y"
    df$`Antécédents de fièvre / frissons`[which(str_detect(toupper(df$`Antécédents de fièvre / frissons`),"FRISSON"))]<-"Y"
    df$`Antécédents de fièvre / frissons`<-ifelse(df$`Antécédents de fièvre / frissons`=="Y","Y",ifelse(df$`Antécédents de fièvre / frissons`=="N","N",NA))
    df$`Antécédents de fièvre / frissons`
}
columns_used = append(columns_used,"Antécédents de fièvre / frissons")
columns_used = append(columns_used,"Autre signe / symptôme, précisez")
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = function(df)
  { df$`Mal de gorge` <- toupper(df$`Mal de gorge`)
  sympt_level_key <- c(YES = "Y",OUI="Y", 
                       NO="N",NON="N",
                       `DON'T KNOW`=NA, 'NA'=NA)
    df$`Mal de gorge` <- recode(df$`Mal de gorge`, !!!sympt_level_key)
    df$`Mal de gorge`[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"GORGE"))]<-"Y"
    df$`Mal de gorge`
}
columns_used = append(columns_used,"Antécédents de fièvre / frissons")
columns_used = append(columns_used,"Autre signe / symptôme, précisez")
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = function(df)
  { df$`Toux` <- toupper(df$`Toux`)
    sympt_level_key <- c(YES = "Y",OUI="Y", 
                         NO="N", NON="N",
                       `DON'T KNOW`=NA, 'NA'=NA)
    df$`Toux` <- recode(df$`Toux`, !!!sympt_level_key)
    df$`Toux`[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"TOUX"))]<-"Y"
    df$`Toux`[which(str_detect(toupper(df$`Toux`),"TOUX"))]<-"Y"
    df$`Toux`
}
columns_used = append(columns_used,"Toux")
columns_used = append(columns_used,"Autre signe / symptôme, précisez")
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = function(df)
  { df$rhinorrhée<-toupper(df$rhinorrhée)
  sympt_level_key <- c(YES = "Y",OUI="Y", 
                           NO="N", NON="N",
                       `DON'T KNOW`=NA, 'NA'=NA,YN=NA)
    df$rhinorrhée <- recode(df$rhinorrhée, !!!sympt_level_key)
    df$rhinorrhée[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"RHINORRHEE"))]<-"Y"
    df$rhinorrhée[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"NASAL"))]<-"Y"
    df$rhinorrhée
  }
columns_used = append(columns_used,"rhinorrhée")
columns_used = append(columns_used,"Autre signe / symptôme, précisez")
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = function(df)
  { df$dyspnée<-toupper(df$dyspnée)
    sympt_level_key <- c(YES = "Y",OUI="Y", 
                         NO="N", NON="N",NN="N",
                       `DON'T KNOW`=NA, 'NA'=NA)
    df$dyspnée <- recode(df$dyspnée, !!!sympt_level_key)
    df$dyspnée[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"ESSOUF"))]<-"Y"
    df$dyspnée[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"ESS0UF"))]<-"Y"
    df$dyspnée[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"RESPIRATOIRE"))]<-"Y"
    df$dyspnée[which(str_detect(toupper(df$`Autre signe / symptôme, précisez`),"ASTHME"))]<-"Y"
    df$dyspnée[which(str_detect(toupper(df$`dyspnée`),"ESSOUF"))]<-"Y"
    df$dyspnée[which(str_detect(toupper(df$`dyspnée`),"ESS0UF"))]<-"Y"
    df$dyspnée[which(str_detect(toupper(df$`dyspnée`),"RESPIRATOIRE"))]<-"Y"
    df$dyspnée[which(str_detect(toupper(df$`dyspnée`),"ASTHME"))]<-"Y"
    df$dyspnée
  }
columns_used = append(columns_used,"dyspnée")
columns_used = append(columns_used,"Autre signe / symptôme, précisez")
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = function(df)
  { df$`Autre signe / symptôme, précisez`<-gsub("NA",NA,df$`Autre signe / symptôme, précisez`)
    df$`Autre signe / symptôme, précisez`<-gsub("RAS",NA,df$`Autre signe / symptôme, précisez`)
    df$`Autre signe / symptôme, précisez`
  }
columns_used = append(columns_used,"Autre signe / symptôme, précisez")
# Other signs or symptoms, character string (comma-separated list)
# raw$`Autre signe / symptôme, précisez`

clean_columns[['Comcond_preexist1']] = function(df)
{cond_level_key <- c(YES = "Y", yes = "Y", Yes = "Y", Oui="Y", OUI="Y", oui="Y", 
                      NO="N", No="N",no="N",NON="N",Non="N",non="N",
                      `Don't Know`=NA, 'NA'=NA, NSP="N","NE SAIT PAS"="N")
  df$`Le patient a t-il  des Antécédents  pathologiques préexistantes (Yes / No)` <- recode(df$`Le patient a t-il  des Antécédents  pathologiques préexistantes (Yes / No)`, !!!cond_level_key,.default="NA")
  df$`Le patient a t-il  des Antécédents  pathologiques préexistantes (Yes / No)` <- recode(df$`Le patient a t-il  des Antécédents  pathologiques préexistantes (Yes / No)`, !!!cond_level_key) 
  df$`Le patient a t-il  des Antécédents  pathologiques préexistantes (Yes / No)`
  }
columns_used = append(columns_used,"Le patient a t-il  des Antécédents  pathologiques préexistantes (Yes / No)")
# Patient has pre-existing conditions?, Y/N, factor
# *** Check to be sure Y always involves a condition
#table(raw$`Le patient a t-il  des Antécédents  pathologiques préexistantes (Yes / No)`)

clean_columns[['Comcond_preexist']] = function(df)
  { df$precondiz<-df$`Le patient a t-il  des conditions  pathologiques préexistantes (specifier)`
    df$precondiz<-toupper(df$precondiz)
    df$precondizYN<-df$`Le patient a t-il  des Antécédents  pathologiques préexistantes (Yes / No)`
    cond_level_key <- c(YES = "Y", yes = "Y", Yes = "Y", Oui="Y", OUI="Y", oui="Y", 
                        NO="N", No="N",no="N",NON="N",Non="N",non="N",
                        `Don't Know`=NA, 'NA'=NA, NSP="N","NE SAIT PAS"="N")
    df$precondizYN<-recode(df$precondizYN,!!!cond_level_key)
    df$precondiz<-gsub("NA",NA,df$precondiz)
    df$precondiz<-gsub("RAS",NA,df$precondiz)
    df$`Le patient a t-il  des conditions  pathologiques préexistantes (specifier)`<-ifelse((df$precondizYN=="OTHER" | df$precondizYN=="Y"),paste(df$`Le patient a t-il  des Antécédents  pathologiques préexistantes (Yes / No)`,df$precondiz,sep=","),df$precondiz)
    df$`Le patient a t-il  des conditions  pathologiques préexistantes (specifier)`
}
columns_used = append(columns_used,"Le patient a t-il  des conditions  pathologiques préexistantes (specifier)")
# Patient's pre-existing conditions, character string (comma-separated list)
# table(raw$`Le patient a t-il  des conditions  pathologiques préexistantes (specifier)`)

clean_columns[['expo_visit_healthcare']] = function(df) 
  { df$`Le patient a-t-il visité un établissement de santé au cours des 14 jours précédant l'apparition des symptômes` <- toupper(df$`Le patient a-t-il visité un établissement de santé au cours des 14 jours précédant l'apparition des symptômes`)
    cond_level_key <- c(YES = "Y", OUI="Y", 
                    NO="N", NON="N",
                    `DON'T KNOW`=NA, 'NA'=NA, NSP=NA,"NE SAIT PAS"=NA,
                    COCODY=NA)
    df$`Le patient a-t-il visité un établissement de santé au cours des 14 jours précédant l'apparition des symptômes`<-recode(df$`Le patient a-t-il visité un établissement de santé au cours des 14 jours précédant l'apparition des symptômes`,!!!cond_level_key)
    df$`Le patient a-t-il visité un établissement de santé au cours des 14 jours précédant l'apparition des symptômes`
     }
columns_used = append(columns_used,"Le patient a-t-il visité un établissement de santé au cours des 14 jours précédant l'apparition des symptômes")
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor
# *** currently empty

clean_columns[['expo_ari']] = function(df)
  { df$`Le patient a-t-il été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë au cours des 14 jours précédant l'apparition des symptômes`<-toupper(df$`Le patient a-t-il été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë au cours des 14 jours précédant l'apparition des symptômes`)
    sympt_level_key <- c(YES = "Y", yes = "Y", Yes = "Y", Oui="Y", OUI="Y", oui="Y", 
                      NO="N", No="N",no="N",NON="N",Non="N",non="N",
                      `Don't Know`=NA, 'NA'=NA, NSP="N","NE SAIT PAS"="N")
  df$`Le patient a-t-il été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë au cours des 14 jours précédant l'apparition des symptômes` <- recode(df$`Le patient a-t-il été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë au cours des 14 jours précédant l'apparition des symptômes`, !!!sympt_level_key,.default="NA")
  df$`Le patient a-t-il été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë au cours des 14 jours précédant l'apparition des symptômes` <- recode(df$`Le patient a-t-il été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë au cours des 14 jours précédant l'apparition des symptômes`, !!!sympt_level_key)
  df$`Le patient a-t-il été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë au cours des 14 jours précédant l'apparition des symptômes`
  }
columns_used = append(columns_used,"Le patient a-t-il été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë au cours des 14 jours précédant l'apparition des symptômes")
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = function(df){df$`Spécifiez où le patient a été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë`}
columns_used = append(columns_used,"Spécifiez où le patient a été en contact étroit avec une personne atteinte d'une infection respiratoire aiguë")
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = map_to('Autres expositions (specifier)')
columns_used = append(columns_used,"Autres expositions (specifier)")
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = function(df)
  { df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?`<-toupper(df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?`)
    level_key <- c(YES = "Y", yes = "Y", Yes = "Y", Oui="Y", OUI="Y", oui="Y", 
                       NO="N", No="N",no="N",NON="N",Non="N",non="N",
                       `Don't Know`=NA, 'NA'=NA, NSP="N","NE SAIT PAS"="N")
    df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?` <- recode(df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?`, !!!level_key, .default="NA")
    df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?` <- recode(df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?`, !!!level_key)
    df$`Le patient a-t-il été en contact avec un cas probable ou confirmé?`
    }
columns_used = append(columns_used,"Le patient a-t-il été en contact avec un cas probable ou confirmé?")
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = keep_empty
#function(df) {df$`Numéro d'identification du cas confirmé ou probable 1`}
columns_used = append(columns_used,"Numéro d'identification du cas confirmé ou probable 1")
# ID of confirmed or probable case 1, character

clean_columns[['expo_ID2']] = keep_empty
#function(df) {df$`Numéro d'identification du cas confirmé ou probable 2`}
columns_used = append(columns_used,"Numéro d'identification du cas confirmé ou probable 2")
# ID of confirmed or probable case 2, character

clean_columns[['expo_ID3']] = keep_empty
# function(df) {df$`Numéro d'identification du cas confirmé ou probable 3`}
columns_used = append(columns_used,"Numéro d'identification du cas confirmé ou probable 3")
# ID of confirmed or probable case 3, character

clean_columns[['expo_ID4']] = keep_empty
# function(df) {df$`Numéro d'identification du cas confirmé ou probable 4`}
columns_used = append(columns_used,"Numéro d'identification du cas confirmé ou probable 4")
# ID of confirmed or probable case 4, character

clean_columns[['expo_arisetting']] = map_to('Specifiez le lieu de contat proche')
columns_used = append(columns_used,"Specifiez le lieu de contat proche")
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = function(df)
{ level_key <- c(YES = "Y", yes = "Y", Yes = "Y", Oui="Y", OUI="Y", oui="Y",PCR="Y", 
                 NO="N", No="N",no="N",NON="N",Non="N",non="N",
                 `Don't Know`=NA, 'NA'=NA, NSP="N","NE SAIT PAS"="N")
df$`Échantillon prélevé` <- recode(df$`Échantillon prélevé`, !!!level_key)
}
columns_used = append(columns_used,"Échantillon prélevé")
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = function(df) 
  { df$`Type d'échantillons`<-toupper(df$`Type d'échantillons`)
    df$`Type d'échantillons`<-gsub("DON’T KNOW",NA,df$`Type d'échantillons`)
    df$`Type d'échantillons`<-gsub("POSITIVE",NA,df$`Type d'échantillons`)
    df$`Type d'échantillons`<-gsub("NASO-PHARYNGE","NASOPHARYNGEAL SWAB",df$`Type d'échantillons`)
    df$`Type d'échantillons`[which(grepl("NASO", df$`Type d'échantillons`,fixed=TRUE))]<-"NASOPHARYNGEAL SWAB"
    df$`Type d'échantillons`[which(grepl("AUTRE", df$`Type d'échantillons`,fixed=TRUE))]<-"OTHER"
    df$`Type d'échantillons`
  }
columns_used = append(columns_used,"Type d'échantillons")
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = function(df)
  { df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`[c(grep("[A-Z]",df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`))]<-NA
  df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`<-round(as.numeric(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`))
  df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`<-ifelse(is.na(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`),NA,
                                                                ifelse(nchar(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`) > 43000,format(as.Date(as.numeric(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`), origin = "1899-12-30"), '%Y-%m-%d'),
                                                                       ifelse(nchar(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`) < 43000,format(as.Date(as.numeric(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`), origin = "1904-01-01"), '%Y-%m-%d'),
                                                                              NA))) 
  df$`Date de raportage`<-ifelse(nchar(df$`Date de raportage`)==5,format(as.Date(as.numeric(df$`Date de raportage`), origin = "1899-12-30"), '%Y-%m-%d'),df$`Date de raportage`)
  df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`[which(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`>df$`Date de raportage`)]<-NA
  df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`
  }
columns_used = append(columns_used,"Date de prélèvement de l'échantillon (jj / mm / aaaa)")
# Date when COVID19 Lab sample was taken,  character, YYYY-MM-DD
# *** This date gets converted to a number by EXCEL. Need to check with real data. 
# function(df) as.Date(df$`Date de prélèvement de l'échantillon (jj / mm / aaaa)`, '%Y-%m-%d')

clean_columns[['Lab_performed']] = function(df) 
  { toupper(df$`Laboratoire effectué`)
  df$`Laboratoire effectué` <- gsub("NA",NA,df$`Laboratoire effectué`)
  df$`Laboratoire effectué` <- gsub("Yes","PCR",df$`Laboratoire effectué`)
  df$`Laboratoire effectué`
  }
columns_used = append(columns_used,"Laboratoire effectué")
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df) 
  { df$`Résultat de laboratoire pour COVID19`<-toupper(df$`Résultat de laboratoire pour COVID19`)
    df$`Résultat de laboratoire pour COVID19`<- gsub("NA",NA,df$`Résultat de laboratoire pour COVID19`)
    df$`Résultat de laboratoire pour COVID19`<- gsub("NEGATIF","NEGATIVE",df$`Résultat de laboratoire pour COVID19`)
    df$`Résultat de laboratoire pour COVID19`<- gsub("POSITIF","POSITIVE",df$`Résultat de laboratoire pour COVID19`)
    df$`Résultat de laboratoire pour COVID19`
}
columns_used = append(columns_used,"Résultat de laboratoire pour COVID19")
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = function(df) 
{ df$`Date des résultats de laboratoire (jj / mm / aaaa)`[c(grep("[A-Z]",df$`Date des résultats de laboratoire (jj / mm / aaaa)`))]<-NA
df$`Date des résultats de laboratoire (jj / mm / aaaa)`<-round(as.numeric(df$`Date des résultats de laboratoire (jj / mm / aaaa)`))
df$`Date des résultats de laboratoire (jj / mm / aaaa)`<-ifelse(is.na(df$`Date des résultats de laboratoire (jj / mm / aaaa)`),NA,
                                                                   ifelse(nchar(df$`Date des résultats de laboratoire (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date des résultats de laboratoire (jj / mm / aaaa)`) > 43000,format(as.Date(as.numeric(df$`Date des résultats de laboratoire (jj / mm / aaaa)`), origin = "1899-12-30"), '%Y-%m-%d'),
                                                                          ifelse(nchar(df$`Date des résultats de laboratoire (jj / mm / aaaa)`) == 5 & as.numeric(df$`Date des résultats de laboratoire (jj / mm / aaaa)`) < 43000,format(as.Date(as.numeric(df$`Date des résultats de laboratoire (jj / mm / aaaa)`), origin = "1904-01-01"), '%Y-%m-%d'),
                                                                                 NA))) 
df$`Date de raportage`<-ifelse(nchar(df$`Date de raportage`)==5,format(as.Date(as.numeric(df$`Date de raportage`), origin = "1899-12-30"), '%Y-%m-%d'),df$`Date de raportage`)
df$`Date des résultats de laboratoire (jj / mm / aaaa)`[which(df$`Date des résultats de laboratoire (jj / mm / aaaa)`>df$`Date de raportage`)]<-NA
df$`Date des résultats de laboratoire (jj / mm / aaaa)`
}


columns_used = append(columns_used,"Date des résultats de laboratoire (jj / mm / aaaa)")
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD
# *** This date gets converted to a number by EXCEL. Need to check with real data. 
# function(df) as.Date(df$`Date des résultats de laboratoire (jj / mm / aaaa)`, '%Y-%m-%d')

clean_columns[['Lab_other']] = function(df) {df$`Autres échantillons (précisez)`}
columns_used = append(columns_used,"Autres échantillons (précisez)")
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_otherres']] = function(df) {df$`Autres resultats d'echantillons`}
columns_used = append(columns_used,"Autres resultats d'echantillons")
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = keep_empty
#   function(df)
#   { df$`Date de sortie (si vivant et hospitalisé) (jj / mm / aaaa)` <- gsub("NA",NA,df$`Date de sortie (si vivant et hospitalisé) (jj / mm / aaaa)`)
#     df$`Date de sortie (si vivant et hospitalisé) (jj / mm / aaaa)`
# }
# columns_used = append(columns_used,"Date de sortie (si vivant et hospitalisé) (jj / mm / aaaa)")
# Date when patient was discharged (if alive and hospitalized), character, YYYY-MM-DD
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized
# *** This date gets converted to a number by EXCEL. Need to check with real data. 
# function(df) as.Date(df$`Date de sortie (si vivant et hospitalisé) (jj / mm / aaaa)`, '%Y-%m-%d')
# as of 2020-07-13, this variable no longer exists

# --------------------------------------
# generate colname lists for qualilty control

# remove duplicate column entries in the list of used columns (simply for conciseness)
columns_used = unique(columns_used)

# hardcode columns which are intentionally NOT used
columns_unused = list("prenom", "Nom", "Lieu de residence niveau admin 3 ( Zone de Santée/Ville):", "Lieu de residence niveau adm 4 (village):", "Où le cas a été diagnostiqué, niveau admin  2 (district):", "Mal de gorge", "Number_Of_Contacts", "Active_Contacts", "Follow_up_completeness", "Téléphone", "Enquêteur", "Observation", "Profil", "Opérateur", "PATIENT", "Cod_INHP", "C1_P", "C2_P", "C3_P", "C1", "C2", "C3", "Delai 1er Contrôle", "Delai 2ieme Contrôle", "Delai 3ieme Contrôle", "R1", "R2", "R3", "Dif_code", "Dif_Age", "Dif_date", "Nom&Prénom", "Nom_IPCI", "Age", "Code_INHP", "Code_IPCI", "Résultats", "Date_prél", "date_résult", "contrôle", "C_INHP", "C_IPCI", "Résultats2", "Date_prél3", "date_résult4", "contrôl", "P_INHP", "C_IPCI2", "Résultats3", "Date_prél4", "date_résult5", "Nom_P","Date de sortie (si vivant et hospitalisé) (jj / mm / aaaa)")

# You can use the following line of code to produce the list of intentionally unused columns
# given that you have a raw dataframe of which you already use all columns you want:
# colnames(raw)[!colnames(raw) %in% columns_used]
# BUT THIS HAS TO BE HARDCODED the unused columns!
# the following code puts this list in a format you can copy to hardcode above:
# cat(paste(paste0('"', colnames(raw)[!colnames(raw) %in% columns_used], '"'), collapse = ", "), "\n")
