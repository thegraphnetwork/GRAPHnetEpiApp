#############################################
# Data quality control
#############################################

function(clean) {
  
  # remove specific empty columns
  drops <- c("patinfo_first_name", "patinfo_last_name", "patinfo_resadmin3", "patinfo_resadmin4")
  clean <- as.data.frame(clean[, !(names(clean) %in% drops)])

  cat("\n-----------------------\nChecking Missing Values\n-----------------------\n")
  
  na_percentages <- round(colMeans(is.na(clean)) * 100, 2)
  cols_complete <- names(na_percentages[na_percentages == 0])
  cols_partial <- na_percentages[na_percentages > 0 & na_percentages < 100]
  cols_empty <- names(na_percentages[na_percentages == 100])
  cols_notempty <- names(na_percentages[na_percentages != 100])
  
  cat(paste0("\n", length(cols_complete), " complete columns:\n"))
  cat(paste(cols_complete, collapse = ", "))
  
  cat(paste0("\n\n", length(cols_partial), " partially filled columns (see percentage of NA below):\n"))
  print(datatable(as.data.frame(sort(cols_partial))))
  
  cat(paste0("\n", length(cols_empty), " empty columns:\n"))
  cat(paste(cols_empty, collapse = ", "))
  cat("\n")

  # List of columns with the corresponding datatypes
  check_datatypes <- list(c("patinfo_ID", "character"),
                          c("report_date", "Date"),
                          c("patinfo_ageonset_years", "numeric"),
                          c("patinfo_ageonset_months", "numeric"),
                          c("patcourse_status", "character"),
                          c("patinfo_sex", "character"),
                          c("patinfo_resadmin1", "character"),
                          c("patinfo_resadmin2", "character"),
                          c("patinfo_occus", "character"),
                          c("patinfo_occus_specify", "character"),
                          c("expo_travel", "character"),
                          c("expo_travel_country", "character"),
                          c("expo_date_departure", "Date"),
                          c("pat_symptomatic", "character"),
                          c("pat_contact", "character"),
                          c("patcourse_dateonset", "Date"),
                          c("expo_sourcecaseids", "character"),
                          c("patcourse_severity", "character"),
                          c("report_classif", "character"),
                          c("patcourse_datedeath", "Date"),
                          c("report_orginst", "character"),
                          c("patinfo_idadmin1", "character"),
                          c("patinfo_idadmin2", "character"),
                          c("report_pointofentry", "character"),
                          c("report_pointofentry_date", "Date"),
                          c("consultation_dateHF", "Date"),
                          c("patcourse_admit", "character"),
                          c("patcourse_presHCF", "Date"),
                          c("patcourse_comp", "character"),
                          c("patsympt_fever", "character"),
                          c("patsympt_sorethroat", "character"),
                          c("patsympt_cough", "character"),
                          c("patsympt_runnynose", "character"),
                          c("patsympt_short", "character"),
                          c("patsympt_other", "character"),
                          c("Comcond_preexist1", "character"),
                          c("Comcond_preexist", "character"),
                          c("expo_healthcare", "character"),
                          c("expo_ari", "character"),
                          c("expo_aricontsetting", "character"),
                          c("expo_other", "character"),
                          c("expo_contact_case", "character"),
                          c("expo_ID1", "character"),
                          c("expo_ID2", "character"),
                          c("expo_ID3", "character"),
                          c("expo_ID4", "character"),
                          c("expo_arisetting", "character"),
                          c("Lab_coll", "character"),
                          c("Lab_type", "character"),
                          c("Lab_datetaken", "Date"),
                          c("Lab_performed", "character"),
                          c("Lab_result", "character"),
                          c("Lab_resdate", "Date"),
                          c("Lab_other", "character"),
                          c("patcourse_datedischarge", "Date"))
  
  cat("\n-----------------------\nChecking Datatypes\n-----------------------\n")
  
  for(column in check_datatypes) {
    if(column[[1]] %in% cols_notempty) {
      if(class(clean[, column[1]]) != column[2]) {
        print(paste0(column[1], " not of class ", column[2], ", but of class ", class(clean[, column[1]])))
      }
    }
  }
  
  dates_columns <- list("report_date",
                        "expo_date_departure",
                        "patcourse_dateonset",
                        "patcourse_datedeath",
                        "report_pointofentry_date",
                        "consultation_dateHF",
                        "patcourse_presHCF",
                        "Lab_datetaken",
                        "Lab_resdate",
                        "patcourse_datedischarge")
  
  for(column in dates_columns) {
    clean[, column] <- as.Date(clean[, column])
  }
  
  get_invalid_dates <- function(df, date_col, d_min, d_max) {
    dates <- as.Date(df[!is.na(df[, date_col]), date_col])
    return(df[(dates < as.Date(d_min) | dates > as.Date(d_max)), c('patinfo_ID', date_col)])
  }
  
  cat("\n-----------------------\nChecking Dates\n-----------------------\n")
  
  for(column in dates_columns) {
    if(column %in% cols_notempty) {
      invalid_dates <- get_invalid_dates(clean, column, as.Date("2020-01-01"), Sys.Date())
      if(nrow(invalid_dates) > 0) {
        print(paste0(column, " has ", nrow(invalid_dates), " invalid dates:"))
        print(head(invalid_dates, 5))
      }
    }
  }
  
  # list of columns which should not have duplicates
  check_duplicates <- list("patinfo_ID")
  
  # check for duplicate values
  get_duplicates <- function(df, col) {
    return(df[duplicated(df[col]) | duplicated(df[col], fromLast = TRUE), c("patinfo_ID", col)])
  }
  
  cat("\n-----------------------\nChecking Duplicates\n-----------------------\n")
  
  for(column in check_duplicates) {
    if(column %in% cols_notempty) {
      duplicated_ids = get_duplicates(clean, column)
      if(nrow(duplicated_ids) > 0) {
        print(paste0(column, " has ", nrow(duplicated_ids), " duplicates:"))
        print(head(duplicated_ids, 5))
      }
    }
  }
  
  # ------------------------------------
  # Value checking
  
  # list of columns where only a fixed set of values is allowed
  check_values = list(
    list('patcourse_status',list("ALIVE","DEAD","RECOVERED")),
    list('patinfo_sex',c("M","F")),
    list('patinfo_occus',c("Y","N")),
    list('expo_travel',c("Y","N")),
    list('pat_symptomatic',c("Y","N")),
    list('pat_contact',c("Y","N")),
    list('patcourse_severity',c("MILD","MODERATE","SEVERE","CRITICAL")),
    list('report_classif',c("SUSPECTED","PROBABLE","CONFIRMED","NOT A CASE","RESULTS PENDING")),
    list('report_pointofentry',c("Y","N")),
    list('patcourse_admit',c("Y","N")),
    list('patsympt_fever',c("Y","N")),
    list('patsympt_sorethroat',c("Y","N")),
    list('patsympt_cough',c("Y","N")),
    list('patsympt_runnynose',c("Y","N")),
    list('patsympt_short',c("Y","N")),
    list('Comcond_preexist1',c("Y","N")),
    list('expo_healthcare',c("Y","N")),
    list('expo_ari',c("Y","N")),
    list('expo_contact_case',c("Y","N")),
    list('Lab_coll',c("Y","N")),
    list('Lab_type',c("NASOPHARYNGEAL SWAB","SALIVA","BLOOD","STOOL","OTHER")),
    list('Lab_performed',c("PCR","RDT","ANTIBODY SEROLOGY")),
    list('Lab_result',c("POSITIVE","NEGATIVE","INCONCLUSIVE","AWAITING RESULTS"))
  )
  
  cat("\n-----------------------\nChecking Categorical Values\n-----------------------\n")
  for(column in check_values) {
    if(column[[1]] %in% cols_notempty) {
      check_known <- levels(as.factor(as.character(clean[, column[[1]]]))) %in% column[[2]]
      number_unknown <- sum(!check_known)
      if(number_unknown > 0) {
        print(paste0(column[[1]], " has ", number_unknown, " unknown factor values:"))
        print(levels(as.factor(as.character(clean[, column[[1]]])))[!check_known])
      }
    }
  }
  
  cat("\n-----------------------\nSpecific Sanity Checks\n-----------------------\n")
  
  # Be sure all patients with datedischarge are also alive and were hospitalized
  if("patcourse_datedischarge" %in% cols_notempty) {
    not_dead = (clean[,'patcourse_status']!="DEAD" & is.na(clean[,'patcourse_datedeath']))
    not_dead_and_hosp = not_dead & (clean[,'patcourse_admit']=="Y")
    discharged_impossible = !is.na(clean[,"patcourse_datedischarge"]) & !ifelse(is.na(not_dead_and_hosp),F,not_dead_and_hosp)
    invalid_discharged = clean[discharged_impossible,c('patinfo_ID','patcourse_datedischarge','patcourse_status','patcourse_datedeath','patcourse_admit')]
    
    if(nrow(invalid_discharged)>0) {
      print(paste0("patcourse_datedischarge has ",nrow(invalid_discharged)," invalid entries:"))
      print(head(invalid_discharged,5))
      print("Note: patcourse_datedischarge is only valid if patient is alive and has been admitted to hospital.")
    }
  }
}

