#############################################
#
#   IGH WHO Africa COVID-19 Assistance Working Group
#
#   Data Cleaning (Pre-analysis) Script
#
#   Generic cleaning code
#
#############################################

function(country) {
  # This function runs all the necessary cleaning code for a given country (parameter)
  
    #############################################
    # Load Libraries ############################
    #############################################
    
    library(readr)
    library(DescTools)
    
    #############################################
    # Load Raw Data
    #############################################

    path_name <- paste0(country, "/") #paste0("~/data-cleaning/data/", country, "/")
    latest_report_date <- max(as.Date(gsub(".csv", "", gsub(paste0(country, "_"), "", list.files(path_name))), "%Y_%m_%d"), na.rm = T)
    (latest_report_date <- gsub("-", "_", as.character(latest_report_date)))
    file_name <- paste0(path_name, country, "_",
                        latest_report_date, ".csv")
    raw <- as.data.frame(read_csv(file_name))
   
    # clean up raw column names 
    colnames(raw) <- trimws(colnames(raw), whitespace = "[ \t\r\n]")
    colnames(raw) <- gsub("\u00A0", "", colnames(raw), fixed = TRUE)
    colnames(raw)
    
    
    #############################################
    # Clean Data ################################
    #############################################
    # Map raw data columns to clean data columns, format data
    # ---------------------------
    # Set-up the target dataframe
    
    # Define column names for the cleaned dataframe
    cols <- c('patinfo_ID',
              'report_date',
              'patinfo_first_name',
              'patinfo_last_name',
              'patinfo_ageonset_years',
              'patinfo_ageonset_months',
              'patcourse_status',
              'patinfo_sex',
              'patinfo_resadmin1',
              'patinfo_resadmin2',
              'patinfo_occus',
              'patinfo_occus_specify',
              'expo_travel',
              'expo_travel_country',
              'expo_date_departure',
              'pat_symptomatic',
              'pat_contact',
              'patcourse_dateonset',
              'expo_sourcecaseids',
              'patcourse_severity',
              'report_classif',
              'patcourse_datedeath',
              'patinfo_resadmin3',
              'patinfo_resadmin4',
              'report_orginst',
              'patinfo_idadmin1',
              'report_idadmin2',
              'report_pointofentry',
              'report_pointofentry_date',
              'consultation_dateHF',
              'patcourse_admit',
              'patcourse_presHCF',
              'patcourse_comp',
              'patsympt_fever',
              'patsympt_sorethroat',
              'patsympt_cough',
              'patsympt_runnynose',
              'patsympt_short',
              'patsympt_other',
              'Comcond_preexist1',
              'Comcond_preexist',
              'expo_healthcare',
              'expo_ari',
              'expo_aricontsetting',
              'expo_other',
              'expo_contact_case',
              'expo_ID1',
              'expo_ID2',
              'expo_ID3',
              'expo_ID4',
              'expo_arisetting',
              'Lab_coll',
              'Lab_type',
              'Lab_datetaken',
              'Lab_performed',
              'Lab_result',
              'Lab_resdate',
              'Lab_other',
              'patcourse_datedischarge')
    
    # Generate a dataframe with the necessary columns
    clean <- data.frame(matrix(ncol = length(cols), nrow = nrow(raw)), row.names = NULL, check.names = F)
    colnames(clean) <- cols
    
    # -----------------------------------------------------------------------------
    # Define the country-specific mappings and transformations
    # This is the only country-specific code and is loaded from a separate file
    
    source(paste0("~/data-cleaning/notebooks/maps/map_", country, ".R"))
    
    # --------------------------------------
    # Apply the mappings and transformations
    
    # Apply mappings and transformations
    for(column in names(clean_columns)) {
      print(paste("Filling column", column))
      clean[, column] <- clean_columns[[column]](raw)
    }
    
    #############################################
    # Save raw & clean data to new csv files    #
    #############################################
    
    # Save raw csv file for archiving
    (raw_csv_name <- paste0("~/data-cleaning/data/archive/", country, "_", latest_report_date, "_raw.csv"))
    write.csv(raw, raw_csv_name, row.names = F)
    
    # Save clean csv file for archiving
    (clean_csv_archive_name <- paste0("~/data-cleaning/data/archive/", country, "_", latest_report_date, "_clean.csv"))
    write.csv(clean, clean_csv_archive_name, row.names = F)
    
    # Save clean csv file for further processing
    (clean_csv_name <- paste0("~/data-cleaning/data/cleanCSV/", country, "_clean.csv"))
    write.csv(clean, clean_csv_name, row.names = F)
    
    #############################################
    
    # --------------------------------------
    # Report new columns
    
    # raw_filled <- raw[!sapply(raw, function(x) any(is.na(x)))] # only take the columns which are not completely null
    # new_columns <- colnames(raw_filled)[!((colnames(raw_filled) %in% columns_used) | (colnames(raw_filled) %in% columns_unused))]
    # 
    # if(length(new_columns) == 0) {
    #   new_columns <- "No new unused columns"
    #   }
    # 
    # return(new_columns)

}
