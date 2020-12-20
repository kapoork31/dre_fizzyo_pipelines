source(file.path("~",
                "scripts",
                "fitbit_pipeline_cleaning_kunal",
                "src",
                "utils",
                "fitbit_schema_utils.R"
                )
            )
            
userid_column <- "fizzyo_hub_id"

validate_data_schema <- function(data, schema) {
  # Validate whether two provided schemas are identical to one another,
  # once ordered alphabetically

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



Validate_hr_data <- function(heart_data, link_table) {
  # Run validation checks on heart rate dataframe.
  #
  # Args:
  #   heart_data: HR data.frame
  #   link_table: linkMoreSecure table for clinical data
  #
  # returns:
  #  Errors if any of the validation checks fail

  check_nas(heart_data)
  check_col_names(heart_data)
  check_data_type(heart_data)
  check_col_number(heart_data)
  check_nfs(heart_data)
  check_hr_range(heart_data)
  check_dates(heart_data, link_table)
  check_clinical_id(heart_data, link_table)
}


validate_steps_data <- function(step_data, link_table) {
  # Run validation checks on steps dataframe.
  #
  # Args:
  #   stepData: steps data.frame
  #   clinicalData: clinical dataset
  #
  # returns:
  #  Errors if any of the validation checks fail

  check_nas(step_data)
  check_col_names(step_data)
  check_data_type(step_data)
  check_col_number(step_data)
  check_nfs(step_data)
  check_step_range(step_data)
  check_dates(step_data, link_table)
  check_clinical_id(step_data, link_table)
}


######################
#   NAs in Dataset   #
######################
check_nas <- function(df) {
  # Checks if there are any NAs in the dataset
  #
  # Args:
  #   df: any daily step or HR data.frame
  #
  # returns:
  #  TRUE if there are NAs, or else returns FALSE

  if (any(is.na(df))){

    stop("check unsuccessful - NAs found in dataset")
  } else {
    return("NA check was successful")
  }
}

######################
#    NF in Dataset   #
######################
check_nfs <- function(df) {
  # Checks if there are any non finite (NF) values in the dataset,
  #
  # Args:
  #   df: any daily step or HR data.frame
  #
  # returns:
  #  TRUE if there are NAs, or else returns FALSE

  not_finite <- sum(!is.finite(df$value))

  if (not_finite != 0){

    stop("check unsuccessful - Nonfinite values found in dataset")
  } else {
    return("NonFinite check was successful")
  }
}

######################################
#    Range of Steps/Day in Dataset   #
######################################
check_step_range <- function(df) {
  # Args:
  #   df: any daily step data.frame
  #
  # returns:
  #   Success message if daily steps are within desired range,
  #   or else message saying check is unsuccessful

  expected_min_steps <- 0
  expected_max_steps <- 50000
  min_step_check <- min(df$value, na.rm = TRUE) < expected_min_steps
  max_step_Check <- max(df$value, na.rm = TRUE) > expected_max_steps
  if (min_step_check == TRUE | max_step_Check == TRUE){

    stop("check unsuccessful - daily step range outside 0-50000")
  } else {
    return("step range check successful")
  }
}

check_hr_range <- function(df) {
  # Args:
  #   df: any HR data.frame
  #
  # Returns:
  #   Success message and range of HR values if within desired range,
  #   or else message saying check is unsuccessful
  expected_min_hr <- 30
  expected_max_hr <- 220
  min_hr_check <- min(df$value, na.rm = TRUE) < expected_min_hr
  max_hr_check <- max(df$value, na.rm = TRUE) > expected_max_hr
  if (min_hr_check == TRUE | max_hr_check == TRUE){

    stop("check unsuccessful - HR range outside 30-220")
  } else {
    return("HR range check successful")
  }
}

######################################
#        Variable Name Check         #
######################################
check_col_names <- function(df) {
  # Args:
  #   df: any daily step or HR data.frame
  #
  # returns:
  #   Message saying whether all three desired column names exist in df
  #   else message saying name check unsuccessful

  if ("userid" %in% colnames(df) & "time" %in% colnames(df)
    & "value" %in% colnames(df)){

    return("name check successful: userid, time & value exist")
  } else {
    stop("name check unsuccessful")
  }
}

######################################
#          DataType Check            #
######################################
check_data_type <- function(df) {
  # Args:
  #   df: any daily step or HR data.frame
  #
  # returns:
  #   success message if class of data is expected,
  #   or else an error message

  if (!is.integer(df$value) & !is.character(df$userid) & !is.Date(df$time)){

    stop("column of improper type exists in data")
  } else {
    return("data type check successful")
  }
}

######################################
#          Column Number             #
######################################
check_col_number <- function(df) {
  # Args:
  #   df: any daily step or HR data.frame
  #
  # returns:
  #   success message if column number = 3, or else error message

  expected_num_cols <- 3
  if (ncol(df) != expected_num_cols){

    stop("incorrect number of columns")
  } else {
    return("column number check successful")
  }
}

######################################
#               Date                 #
######################################
check_dates <- function(df, link_table) {
  # Args:
  #   df: any daily step or HR data.frame
  #   link_table: linkMoreSecure dataset with recruitment dates and patient IDs
  #
  # returns:
  #   Merges df with clinical recruit_dates and returns error message if
  #   stepdata date is > today
  #   or if stepdata date is < recruit time, else a success
  #   message

  link_table <- rename(link_table, userid = !! as.name(userid_column)) %>%
    group_by(userid) %>%
    mutate(entryid = 1:n()) %>%
    select("userid", "date_recruited", "entryid")

  df <- df %>%
    group_by(userid) %>%
    mutate(entryid = 1:n())

  df <- left_join(df, link_table, by = c("userid"))
  today <- Sys.Date()
  tz <- base::format(df$time[1], format = "%Z")

  if (tz == "BST"){

      df$time <- df$time + 3600
  }

  if (any(df$time > today)){

    stop("date check unsuccessful: date > today")
  }
  else if (any(df$time < df$date_recruited)){
    stop("date check unsuccessful: date < recruit date")
  } else {
    return("date check successful")
  }
}

###########################################
#  IDs Present in Clinical and ACT Data  #
###########################################
check_clinical_id <- function(df, link_table) {
  # Args:
  #   df: any daily step or HR data.frame
  #   link_table: linkMoreSecure dataset with patient IDs
  #
  # returns:
  #   error message if step or HR IDs are not present in clinical data,
  #   else a success message

  link_table <- rename(link_table, userid = !!userid_column)
  if (any(!df$userid %in% link_table$userid)){

    stop("ERROR: user ID not in clinical data")
  } else {
    return("Clinical ID check successful")
  }
}

check_act_id <- function(df, act_data) {
  # Args:
  #   df: any daily step or HR data.frame
  #   ACTdata: ACT dataset with patient IDs
  #
  # returns:
  #   error message if step or HR IDs are not present in ACT data,
  #   else a success message

  act_data <- rename(act_data, userid = !! as.name(userid_column))
  if (any(!df$userid %in% act_data$userid)){

    stop("ERROR: user ID not in ACT data")
  } else {
    return("ID check successful: IDs in ACT data")
  }
}
