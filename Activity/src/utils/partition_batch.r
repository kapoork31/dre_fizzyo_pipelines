################################################################################
## This file contains the following fitbit batching functions:
##
## clean_all: take in list of patients and run cleaning on them
## featurise_all: take in list of patients and run cleaning featurising on them
## clean_step: step to clean given person data between 2 dates
## featurise_step: step to featurise_step given person data between 2 dates
################################################################################

source(file.path("~",
                 "scripts",
                 "fitbit_pipeline_cleaning_kunal",
                 "src",
                 "utils",
                 "fitbit_cleaning_utils.R"
    )
)

source(file.path("~",
                 "scripts",
                 "fitbit_pipeline_cleaning_kunal",
                 "src",
                 "utils",
                 "fitbit_validation.R"
    )
)

source(file.path("~",
                 "scripts",
                 "fitbit_pipeline_cleaning_kunal",
                 "src",
                 "utils",
                 "fitbit_utils_5_fix_lint.R"
    )
)

clean_all <- function(conn, start_date, end_date,
                      input_table_name, output_table_name,
                      link, raw_hr_meta, clean_hr_meta,
                      id_list){

  # call clean step function on list of people betwene 2 dates
  #
  # Args:
  #   start_date: start_date of data to clean | date
  #   end_date: end of data to clean | date
  #   id_list: list of people to clean
  #   ... various other specific datatable names

  patient_ids <- id_list
  for (patient_id in patient_ids) {

    clean_step(conn, start_date, end_date, patient_id,
               input_table_name, output_table_name, raw_hr_meta, clean_hr_meta)

  }
}

featurise_all <- function(conn, start_date, end_date,
                         input_table_name_hr, input_table_name_fs,
                         hr_clean_meta, fs_meta, output_table_name,
                         link, output_table_name_meta, thresh_table,
                         id_list){

  # call featurise step function on list of people betwene 2 dates
  #
  # Args:
  #   start_date: start_date of data to featurise | date
  #   end_date: end of data to featurise | date
  #   id_list: list of people to featurise
  #   ... various other specific datatable names

  patient_ids <- id_list
  for (patient_id in patient_ids) {

    featurise_step(conn, start_date, end_date, patient_id,
                  input_table_name_hr, input_table_name_fs,
                  hr_clean_meta, fs_meta, output_table_name,
                  link, output_table_name_meta, thresh_table)

  }
}

clean_step <- function(conn, start_date, end_date, patient,
                       input_table_name, output_table_name,
                       raw_hr_meta, clean_hr_meta) {

  # clean data for a single person given start and end date
  #
  # Args:
  #   start_date: start_date of data to clean | date
  #   end_date: end of data to clean | date
  #   patient: id of person to clean
  #   ... various other specific datatable names

  writeLines(patient)
  hr_metadata <- tbl(conn, raw_hr_meta) %>%
    filter(userid == patient &
             date >= start_date &
             date < end_date) %>%
    select(date) %>%
    distinct() %>%
    collect()
  if (nrow(hr_metadata) == 0) {

    writeLines(sprintf("No data for patient %s exists", patient))
    return ()
  }
  dates_to_clean <- sort(hr_metadata$date)

  if (dbExistsTable(conn, clean_hr_meta)) {

    processed_days <- tbl(conn, clean_hr_meta) %>%
      filter(userid == patient &
               date >= start_date &
               date < end_date) %>%
      select(date) %>%
      distinct() %>%
      collect()

    processed_days <- sort(processed_days$date)
    remaining_days <- setdiff(dates_to_clean, processed_days)
  }else{
    remaining_days <- dates_to_clean
  }

  remaining_days <- as.Date(remaining_days, origin = "1970-01-01")
  remaining_days <- sort(remaining_days)
  if (length(remaining_days) == 0){

    writeLines(sprintf("cleaned all days for patient or no data for", patient))
    return ()
  }
  if (length(dates_to_clean) == 1 & length(remaining_days) == 1){
    writeLines(sprintf("only one day of data for %s,
                        need to have more as last day is not cleaned",
                       patient
                       )
              )
    return ()
  }

  if (length(dates_to_clean) > 1 & length(remaining_days) == 1){

    writeLines(sprintf("patient fully cleaned, last day remains", patient))
    return()
  }

  for (i in 1:(length(remaining_days) - 1)) {

    st_date <- remaining_days[i]
    e_data <- st_date + 1
    data <- tbl(conn, input_table_name) %>%
      filter(userid == patient &
               time >= st_date &
               time < e_data) %>%
      arrange(time) %>%
      collect()


    cleaned_data <- remove70_leniant_1(data)
    cleaned_data <- remove_rep_points_data(cleaned_data)
    cleaned_data <- remove_outlier_points(cleaned_data)
    cleaned_data <- remove_random_points(cleaned_data)
    cleaned_data <- remove_random_points(cleaned_data)

    df <- data.frame(remaining_days[i], patient)
    colnames(df) <- c("date", "userid")

    validate_data_schema(cleaned_data, fitbit_clean_hr_schema)
    dbWriteTable(conn, clean_hr_meta, df, append = TRUE, row.names = FALSE)
    dbWriteTable(conn, output_table_name,
                 cleaned_data, append = TRUE,
                 row.names = FALSE
                 )

  }

}


featurise_step <- function(conn, start_date, end_date, patient,
                            input_table_name_hr, input_table_name_fs,
                            hr_clean_meta, fs_meta, output_table_name,
                            link, output_table_name_meta, thresh_table) {

  # featurise data for a single person given start and end date
  #
  # Args:
  #   start_date: start_date of data to featurise | date
  #   end_date: end of data to featurise | date
  #   patient: id of person to featurise
  #   ... various other specific datatable names

  writeLines(patient)

  date_rec <- link[link$fizzyo_hub_id == patient, "date_recruited"]
  hr_metadata <- tbl(conn, hr_clean_meta) %>%
    filter(userid == patient &
             date >= start_date &
             date < end_date) %>%
    select(date) %>%
    distinct() %>%
    collect()

  if (nrow(hr_metadata) == 0) {
    writeLines(sprintf("No data for patient %s exists", patient))
    return ()
  }

  fs_metadata <- tbl(conn, fs_meta) %>%
    filter(userid == patient &
             date >= start_date &
             date < end_date) %>%
    select(date) %>%
    distinct() %>%
    collect()

  dates_to_clean <- unique(c(hr_metadata$date, fs_metadata$date))
  dates_to_clean <- dates_to_clean[dates_to_clean >= date_rec]

  if (dbExistsTable(conn, output_table_name_meta)) {

    processed_days <- tbl(conn, output_table_name_meta) %>%
      filter(userid == patient &
               date >= start_date &
               date < end_date) %>%
      select(date) %>%
      distinct() %>%
      collect()

    processed_days <- sort(processed_days$date)
    remaining_days <- setdiff(dates_to_clean, processed_days)
  }else{
    remaining_days <- dates_to_clean
  }

  remaining_days <- as.Date(remaining_days, origin = "1970-01-01")
  remaining_days <- sort(remaining_days)

  if (length(remaining_days) == 0){

    writeLines(sprintf("featurised all days for patient ", patient))
    return ()
  }
  for (d in as.list(remaining_days)){
    st <- d
    ed <- d + 1

    data_hr <- tbl(conn, input_table_name_hr) %>%
      filter(userid == patient &
               time >= st &
               time < ed) %>%
      arrange(time) %>%
      collect()

    data_fs <- tbl(conn, input_table_name_fs) %>%
      filter(userid == patient &
               time >= st &
               time < ed) %>%
      arrange(time) %>%
      collect()

    if (nrow(data_fs) > 1 & nrow(data_hr) > 1 & min(data_hr$value) > 0){
      # make sure data for both hr and fs exist

      validate_steps_data(data_fs, link)
      Validate_hr_data(data_hr, link)
      raw_fitbit_data <- combine_foot_steps_hr(data_fs, data_hr)

      featurised_fitbit_data <-
            featurise_fitbit_combined(
                raw_fitbit_data,
                conn,
                thresh_table,
                d,
                patient
            ) %>%
        mutate_if(is.integer, as.numeric)

      validate_data_schema(
        featurised_fitbit_data,
        features_2020_12_schema
      )

      dbWriteTable(conn, output_table_name, featurised_fitbit_data,
                   append = TRUE, row.names = FALSE
                  )

    # if hr or fs dont exist then add date and userid else na
    }
    df <- data.frame(d, patient)
    colnames(df) <- c("date", "userid")
    dbWriteTable(
        conn,
        output_table_name_meta,
        df,
        append = TRUE,
        row.names = FALSE
    )

  }
}
