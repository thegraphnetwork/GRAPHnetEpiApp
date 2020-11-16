############################################
#
#   IGH WHO Africa COVID-19 Assistance Working Group
#
#   Data Cleaning (Pre-analysis) Script
#
#   Utility functions
#
#############################################


# Mapping helper functions (these are just shorthands for common transformations)

# Map to specific column from a dataframe
map_to = function(raw_column){
  return(function(df) df[,raw_column])
}

# Map to NA
keep_empty = function(df){return(NA)}

# Map Oui/Non to Y/N
ouinon2en = function(raw_column){
  return (function(df) toupper(ifelse(is.na(df[, raw_column]), NA
                                      , ifelse(tolower(df[, raw_column]) == "oui", "Y"
                                               , ifelse(tolower(df[, raw_column]) == "non", "N", df[, raw_column])))))
}


# Map variants of answers to Yes/No questions to Y/N/UNKNOWN
yes_no_clean <- function(raw_column) { 
  return(function(df) {
    cleaned <- ifelse(tolower(df[[raw_column]]) %in% c("oui","yes", "y"),  "Y", df[[raw_column]]) # map variants of yes to Y
    cleaned <- ifelse(tolower(cleaned) %in% c("non","no","n","none", "nil"),  "N", cleaned) # map variants of no to N
    cleaned <- ifelse(tolower(cleaned) %in% c("don't know","unknown","inconnu","ne sait pas"),  NA, cleaned) # map to UNKNOWN
    return(cleaned)
  })
}

# Clean dates. Modify this as needed for other date format.
clean_dates_1 <- function(raw_column) {  
  return(
    function(df) {
      date_only <- substr(df[[raw_column]], 1, 10) # remove 'UTC'
      parsed_date <- as.Date(date_only,"%Y-%m-%d")  # read in the date
      return(parsed_date) }
  ) 
}

# Convert five digit numeric dates to standard format. Some Mauritius dates are like this for some reason
clean_numeric_dates <- function(raw_column) {  
  return(
    function(df) {
      cleaned <- as.character(df[[raw_column]])
      # define tests for the different numeric date types
      numericdateconditionwin <- nchar(cleaned) == 5 & as.numeric(cleaned) > 43000  # excel for windows uses an earlier start date (Dec 30, 1899), therefore number is bigger
      numericdateconditionmac <- nchar(cleaned) == 5 & as.numeric(cleaned) < 43000  # excel for mac uses a later (Jan 1 1904) start date, therefore number is smaller
      # replace with proper date format
      cleaned[which(numericdateconditionwin)] <- format(as.Date(as.numeric(cleaned[which(numericdateconditionwin)]), origin = "1899-12-30"), '%Y-%m-%d')
      cleaned[which(numericdateconditionmac)] <- format(as.Date(as.numeric(cleaned[which(numericdateconditionmac)]), origin = "1904-01-01"), '%Y-%m-%d')
      return(cleaned) }
  ) 
}

# multiple date types function
# Some columns have a variety of date types. This function checks for numeric dates, dates with two digit years and dates with four digit years, 
# then modifies as needed. You can add tests for other date types if needed.
multiple_date_types <- function(raw_column) {
  return(function(df) {
    cleaned <- as.character(df[[raw_column]])
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
    #return vector as date
    return(as.Date(cleaned)) 
  })
}


# Upper case
uppercase <- function(raw_column) {
  return( function(df) toupper(df[[raw_column]])  ) 
}
