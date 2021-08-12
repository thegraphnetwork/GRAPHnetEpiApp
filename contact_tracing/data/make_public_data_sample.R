#' 
#' #' ---------------------------
#' #' Purpose of script:
#' #' Generates sample data for COG
#' #' (based on ODK export)
#' #' ---------------------------
#' 
#' library(charlatan)
#' library(randomNames)
#' library(tidyverse)
#' 
#' n_contacts <- 360  ## must be an even number
#' n_cases <- 40
#' outbreak_how_many_days_ago <- 30
#' 
#' 
#' half_contacts <- round(n_contacts/2)
#' 
#' # ~~~~ Accumulate functions ----
#' ## these are functions that depend on the previous element in the vector
#' 
#' follow_or_not <-
#'   function(previous, current) {
#'     if(!is.na(current)) return(current)  # for initialized values
#'     # otherwise,
#'     return(sample(c("vu ou contacte", "non vu ou contacte"),
#'                   prob = c(0.95, 0.05),
#'                   replace = T,
#'                   size = 1))
#'   }
#' ## once an individual is not seen once, they are not seen for good (for simplicity)
#' cascade_down_not_seen <-
#'   function(previous, current) {
#'     if(previous == "non vu ou contacte") return("non vu ou contacte")
#'     # otherwise,
#'     return(current)
#'   }
#' 
#' 
#' cascade_down_symptomatic <-
#'   function(previous, current) {
#'     if (previous == "devenu symptomatique et resultats tests attendus") return("devenu symptomatique et resultats tests attendus")
#'     # otherwise,
#'     return(current)
#'   }
#' 
#' test_result <-
#'   function(previous, current) {
#'     if (previous == "devenu symptomatique et resultats tests attendus") return(sample(c("devenu cas confirme", "sorti sain"),
#'                                                                                       prob = c(0.3, 0.7),
#'                                                                                       size = 1))
#'     # otherwise,
#'     return(current)
#'   }
#' 
#' 
#' cascade_down_test_result <-
#'   function(previous, current) {
#'     if(previous == "devenu cas confirme") return("devenu cas confirme")
#'     if(previous == "sorti sain") return("sorti sain")
#'     # otherwise,
#'     return(current)
#'   }
#' 
#' 
#' 
#' #### BEGIN HERE
#' 
#' regions_df <-
#'   tibble::tribble(
#'     ~region,                         ~district,
#'     "Big City",        "Gomade", 
#'     "Big City",        "Tomo", 
#'     "Big City",        "Newtown", 
#'     "Big City",        "Solaplace", 
#'     "Big City",        "Firefly", 
#'     "Big City",        "Gomo", 
#'     "Big City",        "Charewa", 
#'     "Big City",        "Tolo", 
#'     "Big City",        "GHU", 
#'     "Small City",      "Gimbe", 
#'     "Small City",      "Hio", 
#'     "Small City",      "Dom", 
#'     "Small City",      "Dom 2", 
#'     "Small City",      "Ghera", 
#'     "Small City",      "Feme", 
#'     "Small City",      "Tolotaiwo" 
#'   ) %>%
#'   mutate(location = paste(region, district, sep = "--"))
#' 
#' liens_contact_adultes <- c("parent proche", "ami", "collegue de travail", "co passager", "co patient", "patient", "autre")
#' liens_contact_enfants <- c("parent proche", "ami", "co passager", "co patient", "autre")
#' 
#' 
#' 
#' contacts_male <-
#'   tibble(quel_est_le_nom_du_contact = randomNames(n = half_contacts, which.names = "last", gender = 0),
#'          quel_est_le_prenom_du_contact = randomNames(n = half_contacts, which.names = "first", gender = 0),
#'          sexe = "homme"
#'   )
#' 
#' contacts_female <-
#'   tibble(quel_est_le_nom_du_contact = randomNames(n = half_contacts, which.names = "last", gender = 1),
#'          quel_est_le_prenom_du_contact = randomNames(n = half_contacts, which.names = "first", gender = 1),
#'          sexe = "femme")
#' 
#' liste_contacts_sample <-
#'   contacts_male %>%
#'   bind_rows(contacts_female) %>%
#'   slice_sample(n = nrow(.)) %>%
#'   mutate(code_unique_du_contact = paste0("Civ", 1:n_contacts)) %>%
#'   arrange(code_unique_du_contact) %>%
#'   mutate(quel_est_l_age_du_contact = sample( c(1:80, rep(NA, times = 5) ),  ## add NA's to make messy
#'                                              size = n(), replace = T)) %>%
#'   mutate(quelle_est_l_unite_de_l_age = "ans") %>%
#'   mutate(location = sample(regions_df$location, size = n(), replace = T)) %>%
#'   mutate(location = sample(c(location, rep(NA_character_, times = 10)),
#'                            size = n(),
#'                            replace = T)) %>%
#'   separate(location, into = c("region_de_residence", "district_de_residence"), sep = "--", remove = T) %>%
#'   mutate(profession = ch_job(n = n(), locale = "fr_FR")) %>%
#'   mutate(profession = sample(c(profession, rep(NA_character_, times = 50)),
#'                              size = n(),
#'                              replace = T)) %>%
#'   mutate(profession = if_else(!is.na(quel_est_l_age_du_contact) & quel_est_l_age_du_contact < 18,
#'                               "etudiant",
#'                               profession)) %>%
#'   mutate(code_du_cas_index =  paste0("CAS", sample(1:n_cases, size = n(), replace = T)) ) %>%
#'   mutate(code_du_cas_index = sample(c(code_du_cas_index, rep(NA_character_, times = 15)),
#'                                     size = n(),
#'                                     replace = T)) %>%
#'   mutate(quel_est_le_lien_du_contact_avec_le_cas = ifelse(quel_est_l_age_du_contact >= 18,
#'                                                           sample(liens_contact_adultes, size = n(), replace = T),
#'                                                           NA_character_)) %>%
#'   mutate(quel_est_le_lien_du_contact_avec_le_cas = ifelse(quel_est_l_age_du_contact < 18,
#'                                                           sample(liens_contact_enfants, size = n(), replace = T),
#'                                                           quel_est_le_lien_du_contact_avec_le_cas)) %>%
#'   mutate(quel_est_le_lien_du_contact_avec_le_cas = sample(c(quel_est_le_lien_du_contact_avec_le_cas, rep(NA_character_, times = 20)),
#'                                                           size = n(),
#'                                                           replace = T)) %>%
#'   mutate(quel_type_de_contact  = sample(c("haut risque", "risque eleve",
#'                                           "risque modere ou faible"), size = n(), replace = T )) %>%
#'   mutate(quel_type_de_contact = sample(c(quel_type_de_contact, rep(NA_character_, times = 20)),
#'                                        size = n(),
#'                                        replace = T)) %>%
#'   mutate(date_du_dernier_contact_avec_le_cas =  sample(seq.Date(Sys.Date()-outbreak_how_many_days_ago,
#'                                                                 Sys.Date(),
#'                                                                 by = "1 days"),
#'                                                        size = n(),
#'                                                        replace = T)) %>%
#'   ## arrange properly
#'   mutate(sort_number = as.numeric(str_remove_all(code_unique_du_contact, "Civ"))) %>%
#'   arrange(sort_number)
#' 
#' 
#' 
#' 
#' 
#' suivi_contacts_sample <-
#'   liste_contacts_sample %>%
#'   select(code_unique_du_contact, date_du_dernier_contact_avec_le_cas) %>%
#'   group_by(code_unique_du_contact) %>%
#'   summarise(date_du_suivi = seq.Date(date_du_dernier_contact_avec_le_cas + 1,
#'                                      date_du_dernier_contact_avec_le_cas + 10,
#'                                      by = "1 days"),
#'             date_du_dernier_contact_avec_le_cas = date_du_dernier_contact_avec_le_cas
#'   ) %>%
#'   filter(date_du_suivi <= Sys.Date()) %>%
#'   mutate(jour_du_suivi = as.numeric(date_du_suivi - date_du_dernier_contact_avec_le_cas )) %>%
#'   mutate(etat_du_suivi = if_else(jour_du_suivi == 1,
#'                                  true = sample(c("vu ou contacte", "non vu ou contacte"),
#'                                                prob = c(0.8, 0.2),
#'                                                size = n(),
#'                                                replace = T),
#'                                  false = NA_character_)) %>%
#'   ## if contact was followed on previous day, roll 90-10 dice on whether or not they will be followed on next day
#'   mutate(etat_du_suivi = accumulate(etat_du_suivi, follow_or_not)) %>%
#'   mutate(etat_du_suivi = accumulate(etat_du_suivi, cascade_down_not_seen)) %>%
#'   ## classify follow-up into regular follow-up or symptomatic, needing testing
#'   mutate(issue_du_suivi = "NA",
#'          issue_du_suivi = if_else(etat_du_suivi == "non vu ou contacte",
#'                                   "NA",
#'                                   # only for those who were followed-up
#'                                   sample(c("poursuite du suivi",
#'                                            "devenu symptomatique et resultats tests attendus"),
#'                                          prob = c(0.9, 0.1),
#'                                          replace = T,
#'                                          size  = n()))) %>%
#'   ## cascade down symptomatic state
#'   mutate(issue_du_suivi = if_else(issue_du_suivi == "NA",
#'                                   "NA",
#'                                   accumulate(issue_du_suivi, cascade_down_symptomatic))) %>%
#'   ## if symptomatic, create result
#'   mutate(issue_du_suivi = if_else(issue_du_suivi == "NA",
#'                                   "NA",
#'                                   accumulate(issue_du_suivi, test_result))) %>%
#'   ## cascade down test result
#'   mutate(issue_du_suivi = accumulate(issue_du_suivi, cascade_down_test_result)) %>%
#'   ## now that you're done, put NAs back
#'   mutate(issue_du_suivi = ifelse(issue_du_suivi == "NA", NA_character_, issue_du_suivi)) %>%
#'   select(-date_du_dernier_contact_avec_le_cas)
#' 
#' 
#' 
#' openxlsx::write.xlsx(liste_contacts_sample, here::here("data/liste_contacts_sample_SAMPLE.xlsx"))
#' openxlsx::write.xlsx(suivi_contacts_sample, here::here("data/suivi_contacts_sample_SAMPLE.xlsx"))
#' 
