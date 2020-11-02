#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Exemplary Model to Follow
#
#     Country:
#     Zimbabwe
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

clean_columns[['report_date']] = function(df) {
  df$cleaned <- as.character(df[["report_date"]])
  #identify different types of dates
  numericdateconditionwin <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar(df$cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar(df$cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  df$cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  df$cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  df$cleaned[which(eightchardatecondition)] <- format(as.Date(as.character(df$cleaned[which(eightchardatecondition)]), tryFormats = c("%d-%m-%y","%d/%m/%y","%Y/%m/%d", "%m/%d/%y")),'%Y-%m-%d')
  df$cleaned[which(tenchardatecondition)] <- format(as.Date(as.character(df$cleaned[which(tenchardatecondition)]), tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d", "%d-%m/%Y", "%m/%d/%Y" , "%m/%d/%Y/")),'%Y-%m-%d')
  df$cleaned<- as.character(df$cleaned)
  
  df$cleaned<-ifelse(df$cleaned == "2020-11-07" & df$patinfo_ID == "0202020A00011", "2020-07-11", df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2020-12-07" & df$patinfo_ID == "4040A00037", "2020-07-12", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "2020-12-07" & df$patinfo_ID == "4040A00036", "2020-07-12", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "2020-12-07" & df$patinfo_ID ==  "020107600012" , "2020-07-12", as.character(df$cleaned))
  
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID == "090A-105" , "2020-07-09", df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID == "090A-118" , "2020-07-09", df$cleaned)  
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID == "090A-119", "2020-07-09", df$cleaned)  
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID == "090A-121", "2020-07-09", df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID == "090A-122"  , "2020-07-09", df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID ==   "090A-123", "2020-07-09", df$cleaned)  
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID == "090A-125" , "2020-07-09", df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID =="090A-141", "2020-07-09", df$cleaned)  
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID =="05040B072020", "2020-07-09", df$cleaned)  
  df$cleaned<-ifelse(df$cleaned == "2020-09-07" & is.na(df$patinfo_ID) == T, "2020-07-09", df$cleaned)
  
  df$cleaned<-ifelse(df$cleaned == "2024-06-07" & df$patinfo_ID == "345", "2020-06-07", df$cleaned)
  
  df$cleaned<-ifelse(df$cleaned == "2021-07-25" & df$patinfo_ID == "871", "2020-07-25", df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2021-07-25" & df$patinfo_ID == "4020A00001", "2020-07-25", df$cleaned)
  
  df$cleaned<-ifelse(df$cleaned == "2020-10-08" & df$patinfo_ID == "6050099", "2020-08-10", df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2020-10-08" & df$patinfo_ID == "6050100", "2020-08-10", df$cleaned)
  
  df$cleaned<-ifelse(df$cleaned == "2020-10-10" & df$patinfo_ID == "6010322", NA, df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2020-11-07" & df$patinfo_ID == "6010322", "2020-08-10", df$cleaned)
  
  df$cleaned[1711:1723] <- "2020-07-11" # IDs from 6050106 to 6050118

  # these dates would not change format using as.Date
  cleaned<-ifelse(df$cleaned == "28/6/2020" , "2020-06-28", df$cleaned)
  cleaned<-ifelse(cleaned == "24/7/2020" , "2020-07-24", cleaned)
  cleaned<-ifelse(cleaned == "11/8/20" , "2020-08-11", cleaned)
  cleaned<-ifelse(cleaned == "15/7/2020" , "2020-07-15", cleaned)
  cleaned<-ifelse(cleaned == "20/7/2020" , "2020-07-20",cleaned)
  cleaned<-ifelse(cleaned == "25/7/2020" , "2020-07-25", cleaned)
  cleaned<-ifelse(cleaned == "28/7/2020" , "2020-07-28", cleaned)
  cleaned<-ifelse(cleaned == "30/7/2020" , "2020-07-30", cleaned) 
  cleaned<-ifelse(cleaned == "31/7/2020" , "2020-07-31", cleaned) 
  cleaned<-ifelse(cleaned == "30/5/20" , "2020-05-30",  cleaned)
  cleaned<-ifelse(cleaned =="13/6/20", "2020-06-13",cleaned)
  cleaned<-ifelse(cleaned == "18/6/20" , "2020-06-18", cleaned) 
  cleaned<-ifelse(cleaned == "19/6/20" , "2020-06-19",  cleaned)
  cleaned<-ifelse(cleaned == "23/7/2020" , "2020-07-23", cleaned)
  cleaned<-ifelse(cleaned == "24/7/2020" , "2020-07-25", cleaned)
  cleaned<-ifelse(cleaned == "11/8/2020" , "2020-08-11", cleaned)
  cleaned<-ifelse(cleaned < "2020-03-01" , NA, cleaned)
  
  return(as.Date(as.character(cleaned)))
  
}
columns_used = append(columns_used,"report_date")
# Date of notification (line), character, YYYY-MM-DD

clean_columns[['name_first']] = keep_empty
clean_columns[['name_last']] = keep_empty
# Names, keep empty

clean_columns[['patinfo_ageonset_years']] = function(df) {
  clean<- df[['patinfo_ageonset']]
  clean<- ifelse(clean == "uk" , NA, ifelse(clean == "A", NA, clean))
  return((as.numeric(as.character(clean))))  
  
}
columns_used = append(columns_used,"patinfo_ageonset")
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[['patinfo_ageonset_months']] = function(df) 
{ 
  
  df$patinfo_ageonsetunit<- ifelse(df[,'patinfo_ageonsetunit'] == "18 months", "8", df[,'patinfo_ageonsetunit'])
  df$patinfo_ageonsetunit<-ifelse(df[,'patinfo_ageonsetunit'] == "2months", "2",  df[,'patinfo_ageonsetunit'])
  df$patinfo_ageonsetunit<-ifelse(is.na(df[,'patinfo_ageonsetunit'])==T & as.numeric(as.character(df[,'patinfo_ageonset']))==0,0, as.numeric(df[,'patinfo_ageonsetunit']))  
}

columns_used = append(columns_used,"patinfo_ageonsetunit")
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric
# Ignore error for SaoTome, these come from S.I & S.I. (NA in Portuguese) entries, which are converted to NA as they should be

clean_columns[['patcourse_status']] =  function(df) 
{
  cleaned<- tolower(df$patcourse_status)
  cleaned<-ifelse(grepl("died|dead|deceased",cleaned), "dead",
                  ifelse(grepl("tran|not yet recovered|absconded|alive|isolation|active|released|discharged",cleaned), "alive",
                         ifelse(cleaned == "+", NA ,
                                ifelse(grepl("recovered|re covered|", cleaned), "recovered",cleaned))))
  toupper(cleaned)
}
columns_used = append(columns_used,"patcourse_status")
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[['patinfo_sex']] = function(df) {
  df$patinfo_sex<- toupper(df$patinfo_sex)
  df$patinfo_sex<- ifelse(df$patinfo_sex == "MALE" |df$patinfo_sex == "ME" , "M",
                          ifelse(df$patinfo_sex == "FEMALE" | df$patinfo_sex == "FEAM", "F", df$patinfo_sex))
}
columns_used = append(columns_used,"patinfo_sex")
# M/F, factor

clean_columns[['patinfo_resadmin1']] = function(df) 
{
  df$patinfo_resadmin1<-toupper(df$patinfo_resadmin1) 
  df$patinfo_resadmin1<- ifelse(df$patinfo_resadmin1 == "HOSPITAL FLATS", "MASHONALAND CENTRAL", df$patinfo_resadmin1)  # checked the resadim2
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "MASH CENTRAL" | df$patinfo_resadmin1 == "MASHCENTRAL" , "MASHONALAND CENTRAL",df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "MASH WEST" | df$patinfo_resadmin1 == "MASHONALAN WEST" , "MASHONALAND WEST",df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "MASH EAST", "MASHONALAND EAST",df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "MATABELALAND SOUTH" | df$patinfo_resadmin1 == "MAT SOUTH" | df$patinfo_resadmin1 == "MATABELELANDA SOUTH" | df$patinfo_resadmin1 == "MATEBELELAND SOUTH" , "MATABELELAND SOUTH", df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "MAT NORTH" | df$patinfo_resadmin1 == "MATABELELAND NORTH", "MATABELELAND NORTH", df$patinfo_resadmin1) 
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "MIDLANDA" | df$patinfo_resadmin1 == "MIDLANDS" | df$patinfo_resadmin1 == "GWERU", "MIDLANDS", df$patinfo_resadmin1) 
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "MANICALANDS" | df$patinfo_resadmin1 == "MANICALAND", "MANICALAND", df$patinfo_resadmin1)
  df$patinfo_resadmin1<- ifelse(df$patinfo_resadmin1 == "NOT  ALLOWED TO BE DISCLOSED" | df$patinfo_resadmin1 == "ZIMBABWE", NA,  df$patinfo_resadmin1)
  df$patinfo_resadmin1<- ifelse(grepl("BUL",df$patinfo_resadmin1), "BULAWAYO",  df$patinfo_resadmin1)
  df$patinfo_resadmin1<- ifelse(grepl("HARARE",df$patinfo_resadmin1), "HARARE",  df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(grepl("VIC FALLS",df$patinfo_resadmin1), "MATABELELAND SOUTH", df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(grepl("HWANGE|UMGUZA",df$patinfo_resadmin1), "MATABELELAND NORTH",df$patinfo_resadmin1) 
  df$patinfo_resadmin1<-ifelse(grepl("KADOMA",df$patinfo_resadmin1), "MASHONALAND WEST", df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(grepl("LUPANE",df$patinfo_resadmin1), "MATABELELAND NORTH", df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "ELIHLO 2  KH NYOSI NCUBE H/H POMENT NCUBE", "MATABELELAND NORTH", df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "ST LUKES NURSES HOME FATHERS", "MATABELELAND NORTH", df$patinfo_resadmin1)
  df$patinfo_resadmin1<-ifelse(df$patinfo_resadmin1 == "MALAWI", "IMPORTED", df$patinfo_resadmin1)
}

columns_used = append(columns_used,"patinfo_resadmin1")
# Patient residence (province), Standardize names to all uppercase, factor
# Province missing for many lines, fill in from districts

clean_columns[['patinfo_resadmin2']] = function(df) toupper(df[,'patinfo_resadmin2'])
columns_used = append(columns_used,"patinfo_resadmin2")
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[['patinfo_occus']] = function(df) ifelse(grepl("health|nurse|doctor|rng|midwife",tolower(df$patinfo_occus)), "Y",
                                                       ifelse(is.na(df$patinfo_occus), NA, "N"))
columns_used = append(columns_used,"patinfo_occus")
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[['patinfo_occus_specify']] = map_to('patinfo_occus')
columns_used = append(columns_used,"patinfo_occus")
# Patient occupation, character string (factor)

clean_columns[['expo_travel']] = function(df) {
  
  cleaned <- ifelse(tolower(df[['expo_travel']]) %in% c("oui","yes", "y", "south africa"),  "Y", df[['expo_travel']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n","none", "nil", "nop"),  "N", cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown","inconnu","n/a"),  NA, cleaned) # map to UNKNOWN
  
  return(cleaned)
  
}
columns_used = append(columns_used,"expo_travel")
# Patient history of travel?, Y/N, factor

clean_columns[['expo_travel_country']] = function(df) ifelse(tolower(df$expo_travel_country) == "botswana" | tolower(df$expo_travel_country) == "bots", "botswana",
                                                             ifelse(tolower(df$expo_travel_country) == "united states" | tolower(df$expo_travel_country) == "miami", "usa",
                                                                    ifelse(grepl("south africa|south  africa|rsa|south africe|sa", tolower(df$expo_travel_country)), "south africa",
                                                                           ifelse(grepl("mozambique", tolower(df$expo_travel_country)), "mozambique",
                                                                                  ifelse(grepl("uk|great britain|united kingdom", tolower(df$expo_travel_country)), "uk",
                                                                                         ifelse(tolower(df$expo_travel_country) == "n/a" | tolower(df$expo_travel_country) == "nil" | tolower(df$expo_travel_country) == "local" | tolower(df$expo_travel_country) == "no"| tolower(df$expo_travel_country) == "contact", NA, tolower(df$expo_travel_country)))))))
columns_used = append(columns_used,"expo_travel_country")
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[['expo_date_departure']] = function(df) {
  df$cleaned <- as.character(df[["expo_date_departure"]])
  df$cleaned<- ifelse(df$cleaned == "Mutare" | df$cleaned == "Zvishavane" |  toupper(df$cleaned) == "NO" |  toupper(df$cleaned) == "N/A" |  toupper(df$cleaned) == "NIL", NA, df$cleaned)
  #identify different types of dates
  numericdateconditionwin <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar(df$cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar(df$cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  df$cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  df$cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  df$cleaned[which(eightchardatecondition)] <- format(as.Date(df$cleaned[which(eightchardatecondition)], tryFormats = c("%d-%m-%y","%d/%m/%y","%Y-%m-%d")),'%Y-%m-%d')
  df$cleaned[which(tenchardatecondition)] <- format(as.Date(df$cleaned[which(tenchardatecondition)], tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d", "%d %m %Y")),'%Y-%m-%d')
  
  #df$cleaned<-  ifelse(tolower(df$cleaned) %in% c("nil","no","n","none", "n/a", "mutare"),  NA, df$cleaned) 
  df$cleaned<- as.character(df$cleaned)
  
  df$cleaned<-ifelse(df$cleaned == "2020-10-07" & df$patinfo_ID == "020107600012" , "2020-07-10",df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2019-12-20" & df$patinfo_ID == "08050Z00005" , NA, df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2018-03-12" & df$patinfo_ID == "08050Z00009", "2020-03-12", df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2019-09-22"& df$patinfo_ID == "08050Z00001", NA,df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2019-09-13"& df$patinfo_ID == "08050Z00004", NA,df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2019-08-01" & is.na(df$patinfo_ID) == T, "2020-08-01",  df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2019-01-11" & df$patinfo_ID == "08050Z00008", "2020-01-11", df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2019-05-28" & df$patinfo_ID == "08050Z00011", "2020-05-28", df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2021-02-13" & df$patinfo_ID == "070832019", "2020-02-13",df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2019-09-01" & df$patinfo_ID == "08050Z00001" , NA, df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2019-08-01" & is.na(df$patinfo_ID) == T , "2020-08-01", df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2019-09-13" & df$patinfo_ID == "08050Z00004" , NA, df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2019-12-20" & df$patinfo_ID == "08050Z00005" , NA, df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2019-01-11" & df$patinfo_ID == "08050Z00008" , NA, df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "2021-02-13" & df$patinfo_ID == "070832019" , "2020-02-13", df$cleaned)
  df$cleaned<-ifelse(df$cleaned == "24/7/2020"  , "2020-07-24", df$cleaned)
  
  return(as.character(df$cleaned)) } 

columns_used = append(columns_used,"expo_date_departure")
# Date departed from country visited / Date retuned from country visited, character, YYYY-MM-DD

clean_columns[['pat_symptomatic']] = function(df) {
  df$patsympt_cough<- ifelse(grepl("y|yes" , tolower(df$patsympt_cough)), "Y", df$patsympt_cough)
  df$patsympt_cough<- ifelse(is.na(df[,'patsympt_cough'])==T | tolower(df$patsympt_cough) =="n/a" , NA, "N")
  df$patsympt_fever<-  ifelse(grepl("y|yes" , tolower(df$patsympt_fever)), "Y",ifelse(is.na(df[,'patsympt_fever'])==T | tolower(df$patsympt_fever) =="n/a" , NA, "N"))
  df$patsympt_runnynose<- ifelse(grepl("y|yes" , tolower(df$patsympt_runnynose)), "Y",df$patsympt_runnynose)
  df$patsympt_runnynose<-ifelse(is.na(df[,'patsympt_runnynose'])==T | tolower(df$patsympt_runnynose) =="n/a" , NA, "N")
  df$patsympt_sorethroat<- ifelse(grepl("y|yes" , tolower(df$patsympt_sorethroat)), "Y",df$patsympt_sorethroat)
  df$patsympt_sorethroat<-ifelse(is.na(df[,'patsympt_sorethroat'])==T | tolower(df$patsympt_sorethroat) =="n/a" , NA, "N")
  df$patsympt_short<- ifelse(grepl("y|yes" , tolower(df$patsympt_short)), "Y",df$patsympt_short)
  df$patsympt_short<-ifelse(is.na(df[,'patsympt_short'])==T | tolower(df$patsympt_short) =="n/a" , NA, "N")
  
  df$patsympt_other<- ifelse(df$patsympt_other == "No" | df$patsympt_other == "None" | df$patsympt_other == "Pregnancy" | df$patsympt_other == "HIV", "N", 
                             ifelse(is.na(df[,'patsympt_other'])==T, NA, "Y"))
  
  ifelse(df$patsympt_fever=="Y" | df$patsympt_sorethroat=="Y"|df$patsympt_cough=="Y"|df$patsympt_runnynose=="Y"|df$patsympt_short=="Y","Y",
         ifelse(df$patsympt_fever=="N" & df$patsympt_sorethroat=="N" & df$patsympt_cough=="N" & df$patsympt_runnynose=="N" & df$patsympt_short=="N" ,"N",
                NA))
  
}
# columns_used = append(columns_used,"pat_symptomatic")
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable 'pat_symptomatic' otherwise, use boolean ifelse (any=1,Y) (all=0,N) (negative & missing data,NA)

# clean_columns[['pat_contact']] = map_to('pat_contact')
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[['patcourse_dateonset']] = function(df) { 
  df$cleaned <- as.character(df[["patcourse_dateonset"]])
  df$cleaned<- ifelse(tolower(df$cleaned) == "yes" | tolower(df$cleaned) == "no" | tolower(df$cleaned) == "n/a", NA, df$cleaned)
  #identify different types of dates
  numericdateconditionwin <- nchar( df$cleaned) == 5 & as.numeric( df$cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar( df$cleaned) == 5 & as.numeric( df$cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar( df$cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar( df$cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  df$cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric( df$cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  df$cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric( df$cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  df$cleaned[which(eightchardatecondition)] <- format(as.Date( df$cleaned[which(eightchardatecondition)], tryFormats = c("%d-%m-%y","%d/%m/%y","%Y-%m-%d")),'%Y-%m-%d')
  df$cleaned[which(tenchardatecondition)] <- format(as.Date( df$cleaned[which(tenchardatecondition)], tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d","%d/%m-%Y")),'%Y-%m-%d')
  df$cleaned<- as.character(df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID == "02040A00026", "2020-07-10",df$cleaned)
  df$cleaned<-ifelse(df$cleaned =="2020-12-07" & df$patinfo_ID == "30500012", "2020-07-12", df$cleaned)
  df$cleaned<-ifelse(df$cleaned =="22/7/2020" , "2020-07-22", df$cleaned)
  return(as.Date(as.character(df$cleaned)))
}
columns_used = append(columns_used,"patcourse_dateonset")
# Date of onset of symptoms,  character, YYYY-MM-DD
# NA if no symptoms
# check that this is not identical to other dates (eg, 'report_date')
# *** This is currently identical to report_date.

clean_columns[['expo_sourcecaseids']] = function(df) {
  
  df[,'expo_ID1']<- ifelse(toupper(df[,'expo_ID1']) == "NO" | toupper(df[,'expo_ID1']) == "N/A" | is.na(df[,'expo_ID1']) == T | df[,'expo_ID1'] == "Not confirmed in the province" | df[,'expo_ID1'] == "RETURNEES" | df[,'expo_ID1'] == "20/2020", NA, df[,'expo_ID1'])
  df[,'expo_ID2']<- ifelse(toupper(df[,'expo_ID2']) == "NO" | toupper(df[,'expo_ID2']) == "N/A" | is.na(df[,'expo_ID2']) == T | toupper(df[,'expo_ID2']) == "NIL" , NA, df[,'expo_ID2'])
  df[,'expo_ID3']<- ifelse(toupper(df[,'expo_ID3'])== "NO" | toupper(df[,'expo_ID3']) == "N/A" | is.na(df[,'expo_ID3'])  == T | toupper(df[,'expo_ID3']) == "NIL" | toupper(df[,'expo_ID3']) == "A" , NA, df[,'expo_ID3'])
  df[,'expo_ID4']<- ifelse(toupper(df[,'expo_ID4']) == "NO" | toupper(df[,'expo_ID4']) == "N/A" | is.na(df[,'expo_ID4']) == T | toupper(df[,'expo_ID4']) == "NIL" , NA, df[,'expo_ID4'])
  
  paste(df[,'expo_ID1'],df[,'expo_ID2'],df[,'expo_ID3'],df[,'expo_ID4'],sep=",")
  
  
}
columns_used = append(columns_used,"expo_ID1")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)
# Renamed from WHO template

#clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# columns_used = append(columns_used,"patcourse_severity")
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[['report_classif']] = function(df) ifelse(df[,'report_classif']=="Pending","RESULTS PENDING",
                                                        ifelse(df[,'report_classif']=="probable_known","PROBABLE",
                                                               ifelse(df[,'report_classif']=="suspected","SUSPECTED",
                                                                      ifelse(df[,'report_classif']=="no_case","NOT A CASE",
                                                                             ifelse(grepl( "confirmed" ,tolower(df[,'report_classif'])), "CONFIRMED", df[,'report_classif'] )))))
columns_used = append(columns_used,"report_classif")
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = function(df) {
  df$cleaned <- as.character(df[["patcourse_datedeath"]])
  # df$cleaned<- ifelse(tolower(df$cleaned) == "yes" | tolower(df$cleaned) == "no" | tolower(df$cleaned) == "n/a", NA, df$cleaned)
  #identify different types of dates
  numericdateconditionwin <- nchar( df$cleaned) == 5 & as.numeric( df$cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar( df$cleaned) == 5 & as.numeric( df$cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar( df$cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar( df$cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  df$cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric( df$cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  df$cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric( df$cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  df$cleaned[which(eightchardatecondition)] <- format(as.Date( df$cleaned[which(eightchardatecondition)], tryFormats = c("%d-%m-%y","%d/%m/%y","%Y-%m-%d")),'%Y-%m-%d')
  df$cleaned[which(tenchardatecondition)] <- format(as.Date( df$cleaned[which(tenchardatecondition)], tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d","%d/%m-%Y")),'%Y-%m-%d')
  df$cleaned<- as.character(df$cleaned)
  df$cleaned<-  ifelse(df$cleaned =="2020-08-28" & df$patinfo_ID ==  "106020012" ,"2020-07-28", df$cleaned)
  df$cleaned<-ifelse(df$cleaned =="2020-08-28" & df$patinfo_ID ==  "106020013","2020-07-28", df$cleaned) 
  df$cleaned<-ifelse(df$cleaned =="2020-08-28" & df$patinfo_ID ==  "106020014" ,"2020-07-28",df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-08-28" & df$patinfo_ID ==  "104010004" , "2020-07-28", df$cleaned)
  
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID ==  "4060A00007" , "2020-07-10", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID ==  "4060A00008" , "2020-07-10", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID ==  "4060A00009" , "2020-07-10", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID ==  "4060A00006" , "2020-07-10", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID ==  "4060A00005" , "2020-07-10", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID ==  "4060A00002" , "2020-07-10", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID ==  "4060A00003" , "2020-07-10", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID ==  "4060A00004" , "2020-07-10", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="2020-10-07" & df$patinfo_ID ==  "4060A00001" , "2020-07-10", df$cleaned)
  
  
  df$cleaned<- ifelse(df$cleaned =="14/6/2020"  , "2020-06-14", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="17/6/2020"  , "2020-06-17", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="25/7/2020"  , "2020-07-25", df$cleaned)
  df$cleaned<- ifelse(df$cleaned =="22/6/2020"  , "2020-06-22", df$cleaned)
  
  df$cleaned_status<- tolower(df$patcourse_status)
  df$cleaned_status<-ifelse(grepl("died|dead|deceased",df$cleaned_status), "dead",
                            ifelse(grepl("tran|not yet recovered|absconded|alive|isolation|active|released|discharged",df$cleaned_status), "alive",
                                   ifelse(df$cleaned_status == "+", NA ,
                                          ifelse(grepl("recovered|re covered|", df$cleaned_status), "recovered",df$cleaned_status))))
  df$cleaned_status<- toupper(df$cleaned_status)
  
  df$cleaned<- ifelse(df$cleaned_status != "DEAD", NA , df$cleaned)
  return(as.Date(as.character(df$cleaned)))
  
}

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

clean_columns[['patinfo_idadmin1']] =  map_to('patinfo_idadmin1')
columns_used = append(columns_used,"patinfo_idadmin1")
# Where the case was diagnosed, admin level 1 (Province), factor

clean_columns[['patinfo_idadmin2']] = map_to('patinfo_idadmin2')
columns_used = append(columns_used,"patinfo_idadmin2")
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = function(df) {
  cleaned <- ifelse(tolower(df[['report_pointofentry']]) %in% c("oui","yes", "y", "vic falls airport", "imported"),  "Y", df[['report_pointofentry']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n","none", "nil", "local case lafort farm", "no history of travel", "local case", "local case chitemere vil"),  "N", cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown","inconnu","ne sait pas", "n/a", "local", "mupfure quarrantine", "rukawo quarrantine",
                                            "quarrantane centre"),  NA, cleaned) # map to UNKNOWN
  return(cleaned)}
columns_used = append(columns_used,"report_pointofentry")
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = function(df) {
  
  cleaned <- as.character(df$report_pointofentry_date)
  cleaned<- ifelse(cleaned == "Border Jumper" | cleaned == "Bulawayo Polytechnic" | cleaned == "UCE Quarantine" | cleaned == "N/A" | cleaned == "No", NA, cleaned)
  #identify different types of dates
  numericdateconditionwin <- nchar(cleaned) == 5 & as.numeric(cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar(cleaned) == 5 & as.numeric(cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar(cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar(cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric(cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric(cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  cleaned[which(eightchardatecondition)] <- format(as.Date(cleaned[which(eightchardatecondition)], tryFormats = c("%d-%m-%y","%d/%m/%y","%Y-%m-%d")),'%Y-%m-%d')
  cleaned[which(tenchardatecondition)] <- format(as.Date(cleaned[which(tenchardatecondition)], tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d")),'%Y-%m-%d')
  cleaned2<-ifelse(cleaned == "29/0/2020", NA, cleaned)
  return(as.Date(as.character(cleaned2)))
  
}
columns_used = append(columns_used,"report_pointofentry_date")
# Date detected at point of entry, character, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = function(df) {
  
  df$cleaned <- as.character(df$consultation_dateHF)
  #identify different types of dates
  numericdateconditionwin <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar(df$cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar(df$cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  df$cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  df$cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  df$cleaned[which(eightchardatecondition)] <- format(as.Date(df$cleaned[which(eightchardatecondition)], tryFormats = c("%d-%m-%y","%d/%m/%y","%Y-%m-%d")),'%Y-%m-%d')
  df$cleaned[which(tenchardatecondition)] <- format(as.Date(df$cleaned[which(tenchardatecondition)], tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d")),'%Y-%m-%d')
  df$cleaned<- ifelse(tolower(df$cleaned) %in% c("non","no","n","n/a", "nil"),  NA, df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "30/5/2020" , "2020-05-30", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "29/7/2020" , "2020-07-29", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "25/7/2020" , "2020-07-25", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "23/7/2020" , "2020-07-23", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "19/6/2020" , "2020-06-19", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "15/6/2020" , "2020-06-15", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "2020-11-07" & df$patinfo_ID == "020107600012", "2020-07-11", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "17/7/2020" , "2020-07-17", as.character(df$cleaned))
  df$cleaned<-ifelse(df$cleaned == "22/5/2020" , "2020-05-22", as.character(df$cleaned))
  
  df$cleaned<-ifelse(df$cleaned == "2020-10-10" & df$patinfo_ID == "6010322", NA, df$cleaned)
  
  df$cleaned[1711:1723] <- "2020-07-11"  # IDs from 6050106 to 6050118
  
  return(as.Date(as.character(df$cleaned)))
}
columns_used = append(columns_used,"consultation_dateHF")
# Date of first consultation at this Health Facility, character, YYYY-MM-DD


clean_columns[['patcourse_admit']] = function(df) ifelse(tolower(df[,'patcourse_admit']) == "self isolated"  | tolower(df[,'patcourse_admit']) == "self isolation" | 
                                                           toupper(df[,'patcourse_admit']) == "NO" | toupper(df[,'patcourse_admit']) == "N" | df[,'patcourse_admit'] == 44021, "N",
                                                         ifelse(is.na(df[,'patcourse_admit']) == T, NA, "Y"))
columns_used = append(columns_used,"patcourse_admit")
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = function(df) {
  df$cleaned <- as.character(df$patcourse_presHCF)
  df$cleaned<- ifelse(df$cleaned == "N/A" | df$cleaned == "NO", NA, df$cleaned)
  #identify different types of dates
  numericdateconditionwin <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar(df$cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar(df$cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  df$cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  df$cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  df$cleaned[which(eightchardatecondition)] <- format(as.Date(as.character(df$cleaned) [which(eightchardatecondition)], tryFormats = c("%d-%m-%y","%d/%m/%y","%Y-%m-%d")),'%Y-%m-%d')
  df$cleaned[which(tenchardatecondition)] <- format(as.Date(as.character(df$cleaned) [which(tenchardatecondition)], tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d", "%y/%m-%d")),'%Y-%m-%d')
  df$cleaned<-as.character(df$cleaned)  
  df$cleaned<-ifelse(df$cleaned == "2020-10-06"  & df$patinfo_ID =="020707500004" , "2020-06-10", df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2020-10-06"  & df$patinfo_ID == "020707500005", "2020-06-10", df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2020-10-06"  & df$patinfo_ID == "020707500006", "2020-06-10", df$cleaned) 
  df$cleaned<-ifelse(df$cleaned == "2020-10-06"  & df$patinfo_ID == "020707500007", "2020-06-10", df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "2020-12-07" & df$patinfo_ID =="020107600012", "2020-07-12", df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "31/5/2020", "2020-05-31", df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "18/6/2020", "2020-06-18", df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "20/6/2020", "2020-06-20", df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "13/6/2020", "2020-06-13", df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "20/7/2020", "2020-07-20", df$cleaned)
  
  return(as.Date(as.character(df$cleaned)))
}
columns_used = append(columns_used,"patcourse_presHCF")
# For this episode, date first admitted to hospital, character, YYYY-MM-DD

clean_columns[['patcourse_comp']] = function(df) {
  cleaned <- ifelse(tolower(df[['patcourse_comp']]) %in% c("no","no signs", "none", "nil", "n", "n/a"),  NA, df[['patcourse_comp']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% c("y","yes", "asymptomatic", "symptomatic"),  NA, cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown","inconnu","ne sait pas", "n/a"),  NA, cleaned) # map to UNKNOWN
  return(cleaned)
}
columns_used = append(columns_used,"patcourse_comp")
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = function(df) {
  cleaned <- ifelse(tolower(df[['patsympt_fever']]) %in% c("oui","yes", "y"),  "Y", df[['patsympt_fever']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n","none", "nil"),  "N", cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown","inconnu","ne sait pas", "n/a"),  NA, cleaned) # map to UNKNOWN
  return(cleaned)
}
columns_used = append(columns_used,"patsympt_fever")
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = function(df) {
  cleaned <- ifelse(tolower(df[['patsympt_sorethroat']]) %in% c("oui","yes", "y"),  "Y", df[['patsympt_sorethroat']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n","none", "nil"),  "N", cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown","inconnu","ne sait pas", "n/a"),  NA, cleaned) # map to UNKNOWN
  cleaned <- ifelse(grepl("no", tolower(cleaned)), "N", cleaned)
  cleaned <- ifelse(grepl("n", tolower(cleaned)), "N", cleaned)
  return(cleaned)}
columns_used = append(columns_used,"patsympt_sorethroat")
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = function(df) {
  cleaned <- ifelse(tolower(df[['patsympt_cough']]) %in% c("oui","yes", "y"),  "Y", df[['patsympt_cough']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n","none", "nil"),  "N", cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown","inconnu","ne sait pas", "n/a"),  NA, cleaned) # map to UNKNOWN
  return(cleaned)}
columns_used = append(columns_used,"patsympt_cough")
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = yes_no_clean('patsympt_runnynose')
columns_used = append(columns_used,"patsympt_runnynose")
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = function(df) {
  cleaned <- ifelse(tolower(df[['patsympt_short']]) %in% c("oui","yes", "y"),  "Y", df[['patsympt_short']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n","none", "nil", "sneezing"),  "N", cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown","inconnu","ne sait pas", "n/a"),  NA, cleaned) # map to UNKNOWN
  return(cleaned)}
columns_used = append(columns_used,"patsympt_short")
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = function(df) ifelse(df$patsympt_other == "No" | df$patsympt_other == "None" | df$patsympt_other == "Pregnancy" | df$patsympt_other == "HIV", "N", 
                                                        ifelse(is.na(df[,'patsympt_other'])==T, NA, "Y"))
columns_used = append(columns_used,"patsympt_other")
# Other signs or symptoms, character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = function(df) {
  
  conditions<- c("epigastric pain & muscle","hiv","yes, headache","yes, mild tonsilitis", "yes, hiv","?? tb","malaise, loa","diabetes","ulcers" , "ageusia",
                 "headache","malaise, headache")
  cleaned <- ifelse(tolower(df[['Comcond_preexist1']]) %in% c("oui","yes", "y"),  "Y", df[['Comcond_preexist1']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% conditions,  "Y", cleaned)
  cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n", "m", "none", "nil", "nasal congestion" ,"diarrhoea, muscle aches"),  "N", cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown","inconnu","ne sait pas", "n/a"),  NA, cleaned) # map to UNKNOWN
  return(cleaned)}
columns_used = append(columns_used,"Comcond_preexist1")
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = function(df) ifelse(toupper(df$Comcond_preexist) == "NO" | toupper(df$Comcond_preexist) == "NONE" | is.na(df$Comcond_preexist) == T | 
                                                            toupper(df$Comcond_preexist) == "NIL" | toupper(df$Comcond_preexist) == "N/A"  | 
                                                            toupper(df$Comcond_preexist) == "N" | toupper(df$Comcond_preexist) == "UNKNOWN" , NA,
                                                          ifelse(grepl("diabetes|diabetis|diabetse|diabetic", tolower(df$Comcond_preexist)), "diabetes" ,
                                                                 ifelse(grepl("asth", tolower(df$Comcond_preexist)), "asthma" ,
                                                                        ifelse(grepl("cancer", tolower(df$Comcond_preexist)), "cancer" ,tolower(df$Comcond_preexist)))))
columns_used = append(columns_used,"Comcond_preexist")
# Patient's pre-existing conditions, character string (comma-separated list)

clean_columns[['expo_visit_healthcare']] = function(df) {
  cleaned <- ifelse(tolower(df[['expo_visit_healthcare']]) %in% c("yes", "y", "bindura hospital", "shashi hospital"),  "Y", df[['expo_visit_healthcare']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n","none", "nil"),  "N", cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown", "n/a", "freda rebecca", "43928", "44019"),  NA, cleaned) # map to UNKNOWN
  return(cleaned)}
columns_used = append(columns_used,"expo_visit_healthcare")
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = function(df) ifelse(grepl("N/A|Unknown|not known|UK|CHINEESE COMPANY HWANGE|Unkown|unknown|RRT member",df$expo_ari), NA,
                                                  ifelse(grepl("YES|Yes|yes",df$expo_ari), "Y",
                                                         ifelse(grepl("NO|No|no",df$expo_ari), "N",
                                                                ifelse(is.na(df$expo_ari) == T, NA, df$expo_ari))))
columns_used = append(columns_used,"expo_ari")
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = function(df) {
  cleaned <- ifelse(tolower(df[['expo_aricontsetting']]) %in% c("yes", "y"),  "Y", df[['expo_aricontsetting']]) # map variants of yes to Y
  cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n","none", "nil"),  "N", cleaned) # map variants of no to N
  cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown", "n/a", "freda rebecca", "43928", "44019"),  NA, cleaned) # map to UNKNOWN
  return(cleaned)}
columns_used = append(columns_used,"expo_aricontsetting")
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = map_to('expo_other')
columns_used = append(columns_used,"expo_other")
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = function(df) ifelse(grepl("n/a|not known|unkown|unknown",tolower(df$expo_contact_case)), NA,
                                                           ifelse(grepl("yes|confirmed case|contact|staff",tolower(df$expo_contact_case)), "Y",
                                                                  ifelse(grepl("no",tolower(df$expo_contact_case)), "N",
                                                                         ifelse(is.na(df$expo_contact_case) == T, NA, df$expo_contact_case))))
columns_used = append(columns_used,"expo_contact_case")
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')


clean_columns[['expo_ID1']] = function(df) ifelse(toupper(df[,'expo_ID1']) == "NO" | toupper(df[,'expo_ID1']) == "N/A" | is.na(df[,'expo_ID1']) == T | df[,'expo_ID1'] == "Not confirmed in the province" | df[,'expo_ID1'] == "RETURNEES" | df[,'expo_ID1'] == "20/2020", NA, df[,'expo_ID1'])
columns_used = append(columns_used,"expo_ID1")
# ID of confirmed or probable case 1, numeric

clean_columns[['expo_ID2']] = function(df) ifelse(toupper(df[,'expo_ID2']) == "NO" | toupper(df[,'expo_ID2']) == "N/A" | is.na(df[,'expo_ID2'])== T | toupper(df[,'expo_ID2']) == "NIL" , NA, df[,'expo_ID2'])
columns_used = append(columns_used,"expo_ID2")
# ID of confirmed or probable case 2, numeric

clean_columns[['expo_ID3']] = function(df) ifelse(toupper(df[,'expo_ID3'])== "NO" | toupper(df[,'expo_ID3']) == "N/A" | is.na(df[,'expo_ID3'])== T | toupper(df[,'expo_ID3']) == "NIL" | toupper(df[,'expo_ID3']) == "A"  , NA, df[,'expo_ID3'])
columns_used = append(columns_used,"expo_ID3")
# ID of confirmed or probable case 3, numeric

clean_columns[['expo_ID4']] = function(df) ifelse(toupper(df[,'expo_ID4']) == "NO" | toupper(df[,'expo_ID4']) == "N/A" | is.na(df[,'expo_ID4']) == T | toupper(df[,'expo_ID4']) == "NIL" , NA, df[,'expo_ID4'])
columns_used = append(columns_used,"expo_ID4")
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = function(df)  ifelse(tolower(df$expo_arisetting) %in% c("non","no","n","none", "nil"),  "N", df$expo_arisetting) 
columns_used = append(columns_used,"expo_arisetting")
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = yes_no_clean('Lab_coll')
columns_used = append(columns_used,"Lab_coll")
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = function(df)  ifelse(grepl("NASO|NASP",toupper(df[,'Lab_type'])), "NASOPHARYNGEAL SWAB",
                                                   ifelse(df$Lab_type == "PCR", NA, toupper(df[,'Lab_type'])))
columns_used = append(columns_used,"Lab_type")
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL/OTHER, factor

clean_columns[['Lab_datetaken']] = function(df) {
  df$cleaned <- as.character(df$Lab_datetaken)
  df$cleaned<- ifelse(df$cleaned == "7 AGUST 2020" , "2020-08-07", df$cleaned)
  #identify different types of dates
  numericdateconditionwin <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar(df$cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar(df$cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  df$cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  df$cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  df$cleaned[which(eightchardatecondition)] <- format(as.Date(as.character(df$cleaned) [which(eightchardatecondition)], tryFormats = c("%d-%m-%y","%d/%m/%y","%Y-%m-%d")),'%Y-%m-%d')
  df$cleaned[which(tenchardatecondition)] <- format(as.Date(as.character(df$cleaned) [which(tenchardatecondition)], tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d", "%y/%m-%d")),'%Y-%m-%d')
  df$cleaned<-as.character(df$cleaned)

  df$cleaned<- ifelse(df$cleaned == "2020-11-07" & df$patinfo_ID == "0202020A00011", "2020-07-11",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "2020-11-07" & df$patinfo_ID == "020107600012" ,"2020-07-11",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "2020-09-07" & df$patinfo_ID == "08052900004" ,"2020-07-09",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "2020-12-07" & df$patinfo_ID == "090A0E00319" ,"2020-07-12",  df$cleaned)

  df$cleaned<- ifelse(df$cleaned == "2020-08-28" & df$patinfo_ID == "105390055" ,"2020-07-12",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "2024-06-04" & df$patinfo_ID == "55" ,"2020-07-12",  df$cleaned)

  df$cleaned<- ifelse(df$cleaned == "2020-10-08" & df$patinfo_ID == "6050099" ,"2020-08-10",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "2020-10-08" & df$patinfo_ID == "6050100" ,"2020-08-10",  df$cleaned)

  df$cleaned<- ifelse(df$cleaned == "2020-10-10" & df$patinfo_ID == "6010323" ,NA,  df$cleaned)

  df$cleaned[1711:1723] <- "2020-07-11"  # IDs from 6050106 to 6050118

  df$cleaned<- ifelse(df$cleaned == "31/7/2020"  ,"2020-07-31",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "24/7/2020" ,"2020-07-24",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "15/7/2020" ,"2020-07-15",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "167/07/2020" ,"2020-07-16",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "1462"  ,NA,  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "30/7/2020" ,"2020-07-30",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "23/7/2020"  ,"2020-07-23",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "13/6/2020" ,"2020-06-13",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "31/5/2020" ,"2020-05-31",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "28/7/2020"  ,"2020-07-28",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "20/7/2020"  ,"2020-07-20",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "03/07/202" ,"2020-07-03",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "19/6/2020"  ,"2020-06-19",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "25/7/2020"  ,"2020-07-25",  df$cleaned)
  df$cleaned<- ifelse(df$cleaned < "2020-03-01"  ,NA,  df$cleaned)


  return(as.Date(as.character(df$cleaned)))
}

columns_used = append(columns_used,"Lab_datetaken")
# Date when COVID19 Lab sample was taken, character, YYYY-MM-DD

clean_columns[['Lab_performed']] = function(df) {
  
  cleaned<-toupper(df[,'Lab_performed'])
  ifelse(cleaned == "YES", NA, cleaned)
}
columns_used = append(columns_used,"Lab_performed")
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = function(df) { 
  
  cleaned<- toupper(df[,'Lab_result']) 
  ifelse(cleaned == "POS" |cleaned == "PCR POSITIVE" | cleaned == "POSTIVE" | cleaned == "PCR +VE", "POSITIVE", 
         ifelse(cleaned == "POS/NEGATIVE", "INCONCLUSIVE", cleaned))
  
}
columns_used = append(columns_used,"Lab_result")
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = function(df) {
  df$cleaned <- as.character(df$Lab_resdate)
  #identify different types of dates
  numericdateconditionwin <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar(df$cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar(df$cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  df$cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  df$cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  df$cleaned[which(eightchardatecondition)] <- format(as.Date(as.character(df$cleaned) [which(eightchardatecondition)], tryFormats = c("%d-%m-%y","%d/%m/%y","%Y-%m-%d")),'%Y-%m-%d')
  df$cleaned[which(tenchardatecondition)] <- format(as.Date(as.character(df$cleaned) [which(tenchardatecondition)], tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d", "%y/%m-%d")),'%Y-%m-%d')
  df$cleaned<-as.character(df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "2020-11-07" & df$patinfo_ID == "0202020A00011" , "2020-07-11",df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "2020-11-07" & df$patinfo_ID == "020107600012" , "2020-07-11",df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "2020-10-08" & df$patinfo_ID == "4052100005" , "2020-07-11",df$cleaned)

  df$cleaned<- ifelse(df$cleaned == "31/5/2020" , "2020-05-31",df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "25/7/2020" , "2020-07-25",df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "19/6/2020" , "2020-06-19",df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "18/6/2020" , "2020-06-18",df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "13/6/2020" , "2020-06-13",df$cleaned)

  df$cleaned[1711:1723] <- "2020-07-11"  # IDs from 6050106 to 6050118
  #return vector as date
  return(as.character(df$cleaned))

}
columns_used = append(columns_used,"Lab_resdate")
# Date when COVID19 Lab result was returned, character, YYYY-MM-DD

clean_columns[['Lab_other']] = function(df) { 
  clean<- tolower(df$Lab_other)
  clean<- ifelse(clean %in% c("n/a","nil", "no", "none", "n0", "44020", "44043"),  NA, clean)
  return(toupper(clean))
}
columns_used = append(columns_used,"Lab_other")
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_otherres']] = function(df) {
  clean<- tolower(df$Lab_otherres)
  clean<- ifelse(clean %in% c("n/a","nil", "no", "none"),  NA, clean)
  return(toupper(clean))
}
columns_used = append(columns_used,"Lab_otherres")
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = function(df) {
  df$cleaned <- as.character(df$patcourse_datedischarge)
  df$cleaned <- ifelse(df$cleaned == "N/A", NA, df$cleaned)
  #identify different types of dates
  numericdateconditionwin <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
  numericdateconditionmac <- nchar(df$cleaned) == 5 & as.numeric(df$cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
  eightchardatecondition <- nchar(df$cleaned) == 8 # eight characters,e.g.22/04/20
  tenchardatecondition <- nchar(df$cleaned) == 10 # ten characters,e.g.04/04/2020
  #replace each type of date with YYYY-MM-DD formats
  df$cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
  df$cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric(df$cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
  df$cleaned[which(eightchardatecondition)] <- format(as.Date(as.character(df$cleaned) [which(eightchardatecondition)], tryFormats = c("%d-%m-%y","%d/%m/%y","%Y-%m-%d")),'%Y-%m-%d')
  df$cleaned[which(tenchardatecondition)] <- format(as.Date(as.character(df$cleaned) [which(tenchardatecondition)], tryFormats = c("%d-%m-%Y","%d/%m/%Y","%Y-%m-%d", "%y/%m-%d")),'%Y-%m-%d')
  df$cleaned<-as.character(df$cleaned)

  df$cleaned<- ifelse(df$cleaned == "2020-10-07" & df$patinfo_ID == "4060A00002" , "2020-07-10",df$cleaned)

  df$cleaned<- ifelse(df$cleaned == "14/6/2020" , "2020-06-14",df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "29/6/2020" , "2020-06-29",df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "N/A" , NA ,df$cleaned)
  df$cleaned<- ifelse(df$cleaned == "30/6/2020" , "2020-06-30",df$cleaned)

}
columns_used = append(columns_used,"patcourse_datedischarge")
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
# colnames(raw)[!colnames(raw) %in% columns_used]
# BUT THIS HAS TO BE HARDCODED the unused columns!
# the following code puts this list in a format you can copy to hardcode above:
# cat(paste(paste0('"', colnames(raw)[!colnames(raw) %in% columns_used], '"'), collapse = ", "), "\n")
