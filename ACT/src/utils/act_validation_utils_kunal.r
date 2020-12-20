####################################################################################################
# act_validation_utils_kunal.R
#
# This script contains functions used to validate the structure of ACT data.
#
# ------------------------------------------------------------------------------------------------
#
# Functions:
#  * ValidateSchema: Determines whether two provided schemas are identitical to one another
#  * ValidVals: Determines whether values in ACT pressures are in an acceptable range (-20 to 119)
#  * ValidTime: Determines whether the timestamps in the data are valid
#  * ValidFinites: Determines whether there are any infinite (+/-) in the data
#  * ValidPatientIds: Determines whether the patientId provided exists in the patient dataset
#
####################################################################################################

library(tidyverse)

validate_data_schema <- function(data, schema) {
  # Validate whether two provided schemas are identical to one another,
  #  once ordered alphabetically
  #
  # Args:
  #   data: data.frame of data to compare
  #   schema: data.frame representing the desired schema (basis for comparison)
  #
  # Returns:
  #   None

  data_cols <- sapply(data, class)
  schema_cols <- sapply(schema, class)

  data_cols <- data_cols[order(names(data_cols))]
  schema_cols <- schema_cols[order(names(schema_cols))]

  if (!identical(data_cols, schema_cols)) {

    stop("Data schema does not match expected schema")
  }
}

valid_raw_act_data <- function(pr, link_table){

    # Returns TRUE if all valid checks pass
    # date. FALSE, otherwise.
    #
    # Args:
    #   pr: Data.frame of pressure raw vals
    #   linkTable: linkMoresecure table
    #
    # Returns:
    #   TRUE or FALSE

    check1 <- valid_vals(pr)
    check2 <- valid_finites(pr)
    check3 <- valid_patient_ids(pr, link_table)
    return(check1 == TRUE & check2 == TRUE & check3 == TRUE)

}

valid_vals <- function(pr) {
  # Returns TRUE if values in range -20 to 119.
  # FALSE otherwise.
  #
  # Args:
  #   pr: Data.frame of pressure raw vals
  #
  # Returns:
  #   TRUE or FALSE

  mn <- min(pr$pressure_detrend) # min pressure value
  mx <- max(pr$pressure_detrend) # max pressure value
  return(mn >= -20 & mx <= 119)
}


valid_time <- function(pr, link_table) {
  # Returns TRUE if time data is within earliest data and current
  # date. FALSE, otherwise.
  #
  # Args:
  #   pr: Data.frame of pressure raw vals
  #   link_table: linkMoresecure table
  #
  # Returns:
  #   TRUE or FALSE

  mn <- as.Date(min(pr$time)) # min time
  mx <- as.Date(max(pr$time)) # max time

  # Formate dates in link_table
  link_table <- link_table %>%
    mutate(
      date_recruited = lubridate::ymd(date_recruited)
    )

  earliest_date <- min(link_table$date_recruited) # earliest date

  # Return true if timestamps between first day and current day
  return(mn >= earliest_date & mx <= Sys.Date())
}


valid_finites <- function(pr) {
  # Returns TRUE if all values are finite. FALSE, otherwise.
  #
  # Args:
  #   pr: Data.frame of pressure raw vals
  #
  # Returns:
  #   TRUE or FALSE

  not_finite <- sum(!is.finite(pr$pressure_detrend))
    # count all non finite values
  return(not_finite == 0)
}


valid_patient_ids <- function(pr, link_table) {
  # Returns TRUE if all patient IDs are valid. FALSE, otherwise.
  #
  # Args:
  #   pr: Data.frame of pressure raw
  #   linkTable: linkMoresecure table
  #
  # Returns:
  #   TRUE or FALSE

  pr_id <- unique(pr$patient_id) # all patient record ids
  all_idds <- unique(link_table$patient_record_id)
    # all ids of linkMoresecure data

  # if all data coming from listed participants return true
  return(all(pr_id %in% all_idds))
}
