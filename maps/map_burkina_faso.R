#############################################
#
#     Map & Transform Data
#     raw -> clean
#
#     Country:
#     burkina_faso
#
#############################################

# Install pkgs

# Load utility functions
source("~/data-cleaning/scripts/utils_csv.R")

# --------------------------------------
# initialize
clean_columns <- list()

# --------------------------------------
# map variables 

clean_columns[["patinfo_ID"]] <- map_to("Enrollment")
# Anonymized patient ID, character

clean_columns[["report_date"]] <- map_to("Enrollment date")
# Date of notification (line), date, YYYY-MM-DD

clean_columns[["patinfo_first_name"]] <- keep_empty
clean_columns[["patinfo_last_name"]] <- keep_empty
# Names, keep empty

clean_columns[["patinfo_ageonset_years"]] <- function(df) ifelse(as.numeric(df[, "Age en années"]) < 0 |
                                                                   as.numeric(df[, "Age en années"]) > 150,
                                                                 NA,
                                                                 df[, "Age en années"])
# Age at intake in years, 0 for infants < 12 months, numeric

clean_columns[["patinfo_ageonset_months"]] <- function(df) ifelse(is.na(df[, "Age en mois (si<12 mois)"]) == T & 
                                                                    as.numeric(df[, "Age en mois (si<12 mois)"]) == 0, 
                                                                  0, 
                                                                  ifelse(as.numeric(df[, "Age en mois (si<12 mois)"]) < 0 | 
                                                                           as.numeric(df[, "Age en mois (si<12 mois)"]) > 23,
                                                                         0,
                                                                         df[, "Age en mois (si<12 mois)"]))
# Age at intake in months for < 2 years old, 0 for newborns, NA > 23 months (2 years old), numeric

clean_columns[["patcourse_status"]] <- function(df) factor(ifelse(is.na(df[, "COVID Evolution"]) | 
                                                                     df[, "COVID Evolution"] == "Inconnu",
                                                                   NA,
                                                                   ifelse(df[, "COVID Evolution"] == "Decede",
                                                                          "DEAD",
                                                                          ifelse(df[, "COVID Evolution"] == "Guéri",
                                                                                 "RECOVERED",
                                                                                 "ALIVE"))))
# Patient's clinical outcome, ALIVE/DEAD/RECOVERED, factor
# Alive means an ongoing active case; This does not indicate confirmation of case, only patient outcome regardless of classification

clean_columns[["patinfo_sex"]] <- function(df) toupper(substr(df[, "Sexe du patient"],
                                                              1,
                                                              1))
# M/F, factor

clean_columns[["patinfo_resadmin1"]] <- function(df) toupper(df[, "Regions"])
# Patient residence (province), Standardize names to all uppercase, factor

clean_columns[["patinfo_resadmin2"]] <- function(df) toupper(df[, "Districts"])
# Patient residence (district), Standardize names to all uppercase, factor

clean_columns[["patinfo_occus"]] <- function(df) ifelse(df[, "COVID Professionnel de sante"] == "Oui" |
                                                          df[, "COVID Professionnel de laboratoire"] == "Oui",
                                                        "Y",
                                                        "N")
# Patient occupation is healthcare worker?, Y/N, factor

clean_columns[["patinfo_occus_specify"]] <- function(df) toupper(df[, "COVID Preciser Autre Profession"])
# Patient occupation, Standardize names to all uppercase, factor

clean_columns[["expo_travel"]] <- function(df) ifelse(!is.na(df[, "COVID Pays de voyage 1"]) |
                                                        !is.na(df[, "COVID Pays de voyage 2"]) |
                                                        !is.na(df[, "COVID Pays de voyage 3"]),
                                                      "Y",
                                                      "N")
# Patient history of travel?, Y/N, factor

clean_columns[["expo_travel_country"]]  <- function(df) paste(toupper(df[, "COVID Pays de voyage 1"]),
                                                            toupper(df[, "COVID Pays de voyage 2"]),
                                                            toupper(df[, "COVID Pays de voyage 3"]),
                                                            sep = "-")
# Country(ies) patient travelled to, character string (comma-separated list)

clean_columns[["expo_date_departure"]] <- keep_empty
# Date departed from country visited / Date returned from country visited, date, YYYY-MM-DD

clean_columns[["pat_symptomatic"]] <- function(df) ifelse(df[, "COVID Maux de gorge"] == "Oui" |
                                                            df[, "COVID Toux"] == "Oui" |
                                                            df[, "COVID Ecoulement nasal"] == "Oui" |
                                                            df[, "COVID Essoufflement, manque d’air"] == "Oui" |
                                                            df[, "COVID Fievre / Antecedent de fievre / frissons"] == "Oui" |
                                                            df[, "COVID Cephalees"] == "Oui" |
                                                            df[, "COVID Douleur Abdominale"] == "Oui" |
                                                            df[, "COVID Faiblesse generale"] == "Oui" |
                                                            df[, "COVID Diarrhee"] == "Oui" |
                                                            df[, "COVID Nausee/vomissement"] == "Oui" |
                                                            df[, "COVID Douleur Articulation"] == "Oui" |
                                                            df[, "COVID Douleur Musculaire"] == "Oui",
                                                          "Y",
                                                          ifelse(df[, "COVID Maux de gorge"] == "Non" |
                                                                   df[, "COVID Toux"]== "Non" |
                                                                   df[, "COVID Ecoulement nasal"] == "Non" |
                                                                   df[, "COVID Essoufflement, manque d’air"] == "Non" |
                                                                   df[, "COVID Fievre / Antecedent de fievre / frissons"] == "Non" |
                                                                   df[, "COVID Cephalees"] == "Non" |
                                                                   df[, "COVID Douleur Abdominale"] == "Non" |
                                                                   df[, "COVID Faiblesse generale"] == "Non" |
                                                                   df[, "COVID Diarrhee"] == "Non" |
                                                                   df[, "COVID Nausee/vomissement"] == "Non" |
                                                                   df[, "COVID Douleur Articulation"] == "Non" |
                                                                   df[, "COVID Douleur Musculaire"] == "Non",
                                                                 "N",
                                                                 NA))
# Does the patient present with current or past history of symptoms?, Y/N, factor
# should be variable "pat_symptomatic" otherwise, use boolean ifelse (any = 1, Y) (all = 0, N) (negative & missing data, NA)

clean_columns[["pat_contact"]] <- keep_empty
# Has the patient had close contact with a probable or confirmed case?, Y/N, factor
# This is/should be redundant with 'expo_contact_case'; But it is ambiguous, so prioritize 'expo_contact_case'

clean_columns[["patcourse_dateonset"]] <- map_to("COVID Date d’apparition des symptomes")
# Date of onset of symptoms,  date, YYYY-MM-DD
# NA if no symptoms

clean_columns[["expo_sourcecaseids"]] <- keep_empty
# clean_columns[['expo_sourcecaseids']] = function(df) paste(df[, 'expo_ID1'], df[, 'expo_ID2'], df[, 'expo_ID3'], df[, 'expo_ID4'], sep = "-")
# Confirmed case ID(s) to which patient was exposed, character string (comma-separated list)

clean_columns[["patcourse_severity"]] <- keep_empty
# clean_columns[['patcourse_severity']] = map_to('patcourse_severity')
# COVID19 case severity classification, MILD/MODERATE/SEVERE/CRITICAL, factor

clean_columns[["report_classif"]] <- function(df) ifelse(is.na(df[, "COVID Classification Finale"]), NA
                                                        , ifelse(df[, "COVID Classification Finale"] == "Cas_negatif"
                                                                 , "NOT A CASE"
                                                                 , ifelse(df[, "COVID Classification Finale"] == "Cas_confirme"
                                                                          , "CONFIRMED"
                                                                          , ifelse(df[, "COVID Classification Finale"] == "Cas_probable"
                                                                                   , "PROBABLE"
                                                                                   , "SUSPECTED"))))
# COVID19 case classification, SUSPECTED/PROBABLE/CONFIRMED/NOT A CASE/RESULTS PENDING, factor

clean_columns[['patcourse_datedeath']] = keep_empty
# Date of Death for decesased patients, date, YYYY-MM-DD, NA if Alive

clean_columns[['patinfo_resadmin3']] = map_to('Ville/Village')
# Place of residence admin level 3 (Health Zone/Town), factor

clean_columns[['patinfo_resadmin4']] = map_to('Quartier/Secteur')
# Place of residence admin level 4 (Village), factor

clean_columns[['report_orginst']] = map_to('Organisation unit name')
# Reporting health facility/institution, factor

clean_columns[['patinfo_idadmin1']] = keep_empty
# Where the case was diagnosed, admin level 1 (Province), factor

clean_columns[['patinfo_idadmin2']] = keep_empty
# Where the case was diagnosed, admin level 2 (District), factor

clean_columns[['report_pointofentry']] = keep_empty
# Detected at point of entry (eg, border crossing, airport)?, Y/N, factor 

clean_columns[['report_pointofentry_date']] = keep_empty
# Date detected at point of entry, date, YYYY-MM-DD

clean_columns[['consultation_dateHF']] = map_to('COVID Date de consultation')
# Date of first consultation at this Health Facility, date, YYYY-MM-DD

clean_columns[['patcourse_admit']] = function(df) ifelse(is.na(df[, "COVID Statut du patient"]),
                                                                 NA,
                                                                 ifelse(df[, "COVID Statut du patient"] == "hospitalise/Mise en observation",
                                                                        "Y",
                                                                        "N"))
# Admission to hospital?, Y/N, factor

clean_columns[['patcourse_presHCF']] = keep_empty
# For this episode, date first admitted to hospital, date, YYYY-MM-DD

clean_columns[['patcourse_comp']] = keep_empty
# Other clinical complications, character string (comma-separated list)

clean_columns[['patsympt_fever']] = ouinon2en('COVID Fievre / Antecedent de fievre / frissons')
# History of fever or chills?, Y/N, factor

clean_columns[['patsympt_sorethroat']] = ouinon2en('COVID Maux de gorge')
# History of sore throat?, Y/N, factor

clean_columns[['patsympt_cough']] = ouinon2en('COVID Toux')
# History of cough?, Y/N, factor

clean_columns[['patsympt_runnynose']] = ouinon2en('COVID Ecoulement nasal')
# History of runny nose?, Y/N, factor

clean_columns[['patsympt_short']] = ouinon2en('COVID Essoufflement, manque d’air')
# History of shortness of breath?, Y/N, factor

clean_columns[['patsympt_other']] = function(df) ifelse(df[, "COVID Cephalees"] == "Oui" |
                                                          df[, "COVID Douleur Abdominale"] == "Oui" |
                                                          df[, "COVID Faiblesse generale"] == "Oui" |
                                                          df[, "COVID Diarrhee"] == "Oui" |
                                                          df[, "COVID Nausee/vomissement"] == "Oui" |
                                                          df[, "COVID Douleur Articulation"] == "Oui" |
                                                          df[, "COVID Douleur Musculaire"] == "Oui",
                                                        "Y",
                                                        ifelse(df[, "COVID Cephalees"] == "Non" |
                                                                 df[, "COVID Douleur Abdominale"] == "Non" |
                                                                 df[, "COVID Faiblesse generale"] == "Non" |
                                                                 df[, "COVID Diarrhee"] == "Non" |
                                                                 df[, "COVID Nausee/vomissement"] == "Non" |
                                                                 df[, "COVID Douleur Articulation"] == "Non" |
                                                                 df[, "COVID Douleur Musculaire"] == "Non",
                                                               "N",
                                                               NA))
# Other signs or symptoms, for now Y/N but should be character string (comma-separated list)

clean_columns[['Comcond_preexist1']] = function(df) ifelse(df[, "COVID Maladie neurologique ou neuromusculaire chronique"] == "Oui" |
                                                             df[, "COVID Maladie cardiovasculaire incluant HTA"] == "Oui" |
                                                             df[, "COVID Maladie chronique des poumons"] == "Oui" |
                                                             df[, "COVID Cancer"] == "Oui" |
                                                             df[, "COVID Maladie renale"] == "Oui" |
                                                             df[, "COVID Diabete"] == "Oui",
                                                           "Y",
                                                           ifelse(df[, "COVID Maladie neurologique ou neuromusculaire chronique"] == "Non" |
                                                                    df[, "COVID Maladie cardiovasculaire incluant HTA"] == "Non" |
                                                                    df[, "COVID Maladie chronique des poumons"] == "Non" |
                                                                    df[, "COVID Cancer"] == "Non" |
                                                                    df[, "COVID Maladie renale"] == "Non" |
                                                                    df[, "COVID Diabete"] == "Non",
                                                                  "N",
                                                                  NA))
# Patient has pre-existing conditions?, Y/N, factor

clean_columns[['Comcond_preexist']] = keep_empty
# Patient's pre-existing conditions, character string (comma-separated list) / needs to be done

clean_columns[['expo_healthcare']] = keep_empty
# Has patient visited any healthcare facility in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_ari']] = keep_empty
# Has patient had close contact with a person with acute respiratory infection in the 14 days prior to symptom onset?, Y/N, factor

clean_columns[['expo_aricontsetting']] = keep_empty
# Setting where the patient had close contact with a person with acute respiratory infection, character string, factor

clean_columns[['expo_other']] = keep_empty
# Other exposures, character string (comma-separated list)

clean_columns[['expo_contact_case']] = keep_empty
# Has the patient had contact with a probable or confirmed case?, Y/N, factor
# KEEP THIS COLUMN (Redundant with 'pat_contact')

clean_columns[['expo_ID1']] = keep_empty
# ID of confirmed or probable case 1, numeric

clean_columns[['expo_ID2']] = keep_empty
# ID of confirmed or probable case 2, numeric

clean_columns[['expo_ID3']] = keep_empty
# ID of confirmed or probable case 3, numeric

clean_columns[['expo_ID4']] = keep_empty
# ID of confirmed or probable case 4, numeric

clean_columns[['expo_arisetting']] = keep_empty
# Setting where exposure to confirmed or probable case(s) occurred, character string (comma-separated list)

clean_columns[['Lab_coll']] = ouinon2en('COVID Echantillon Preleve')
# COVID19 lab sample collected?, Y/N, factor

clean_columns[['Lab_type']] = keep_empty
# COVID19 lab sample type, NASOPHARYNGEAL SWAB/SALIVA/BLOOD/STOOL, factor

clean_columns[['Lab_datetaken']] = map_to('COVID Date du prelevement')
# Date when COVID19 Lab sample was taken, date, YYYY-MM-DD

clean_columns[['Lab_performed']] = keep_empty
# What type of lab analysis was performed?, PCR/RDT/ANTIBODY SEROLOGY, factor

clean_columns[['Lab_result']] = keep_empty
# Result of lab analysis performed, POSITIVE/NEGATIVE/INCONCLUSIVE/AWAITING RESULTS, factor

clean_columns[['Lab_resdate']] = keep_empty
# Date when COVID19 Lab result was returned, date, YYYY-MM-DD

clean_columns[['Lab_other']] = keep_empty
# Other lab sample(s), character string (comma-separated list)

clean_columns[['lab_other_samples_result_list']] = keep_empty
# Other lab sample result(s), character string (comma-separated list)

clean_columns[['patcourse_datedischarge']] = keep_empty
# Date when patient was discharged (if alive and hospitalized), date, MM/DD/YYYY
# sanity check: be sure all patients with datedischarge are also alive and were hospitalized
