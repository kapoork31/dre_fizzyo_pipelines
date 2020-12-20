
#######################################
# pipeline_utils.R
#
# This script contains utilites for the ACT pipeline.
#
#
# -------------------------------------
#
# Functions in this file:
#
# fetch_all_devices: Fetches and formats ACT device data.
# fetch_patient_ids_from_devices: Fetches all patient ids in trial.

# CleanStep: Perform the ACT cleaning step on raw data.
# LabelStep: Perform the ACT labelling step on cleaned data.
# FeaturiseStep: Perform the ACT featurisation step on labelled data.
#
########################################

source(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_schema_utils_kunal.R"
    )
)

source(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_cleaning_utils_kunal.R"
    )
)

source(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_labelling_utils_kunal.R"
    )
)

source(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_featurisation_utils_kunal.R"
    )
)

source(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_validation_utils_kunal.R"
    )
)

options(digits.secs = 3)

process_unprocessed_patients <- function(conn, input_table_name,
    output_table_name, start_date, end_date, time_col_name,
    devices_clean_table_name, link_more_secure_table_name,
    pipeline_step_fn, ...
    ) {

  # Performs an ACT pipeline step on all unprocessed patients for a given
  # time interval.
  #
  # Args:
  #   conn: <PostgreSQLConnection> PostgreSQL connection object.
  #   input_table_name: <chr> Name of the input table that an ACT pipeline
  #                    step would read from.
  #   output_table_name: <chr> Name of the output table that an ACT pipeline
  #                    step would write to.
  #   start_date: <Date> The ACT pipeline step will process patient data from
  #              startDate to endDate.
  #   end_date: <Date> The ACT pipeline step will process patient data from
  #            startDate to endDate.
  #   time_col_name: <chr> The name of the column to filter on for startDate and
  #                endDate. For clean and label the column is "time". For
  #                featurise the column is "act_date".
  #   devices_clean_table_name: <chr> Name of devices table. (Necessary to
  #                          obtain all patients in trial and for the ACT
  #                          featurisation step.)
  #   link_more_secure_table_name: <chr> Name of link table. (Necessary to
  #                            obtain all patients in trial.)
  #   pipeline_step_fn: <function> ACT pipeline step function. One of:
  #                         CleanStep, LabelStep, FeaturiseStep.
  #   ...: Other args to provide to the pipelineStepFunction.

  if (output_table_name == "pressure_raw_vals") {
    stop("Output table name should not be equal to the raw data table name.")
  }

  devices <- fetch_all_devices(conn,
    devices_clean_table_name,
    link_more_secure_table_name
  )
  patient_ids <- fetch_patient_ids_from_devices(devices)

  processing_start_time <- Sys.time()
  for (patient_id in patient_ids) {

    writeLines(sprintf("Processing patient id %s", patient_id))
    step_start_time <- Sys.time()
    pipeline_step_fn(
      conn,
      patient_id,
      input_table_name,
      output_table_name,
      start_date,
      end_date,
      devices,
      ...
    )
    step_end_time <- Sys.time()

    writeLines(sprintf("Step took (mins): %s",
        difftime(step_end_time, step_start_time, units = "mins")))

  }
  processing_end_time <- Sys.time()

  writeLines(sprintf("Total Execution Time (mins): %s",
                     difftime(processing_end_time, processing_start_time,
                              units = "mins")))
}


fetch_all_devices <- function(
    conn,
    devices_clean_table_name,
    link_more_secure_table_name
    ) {
  # Returns a formatted devices data.frame necessary for
  # fetch_patient_ids_from_devices and for the FeaturisationStep's
  # FeaturiseTreatmentAdherence function call.
  #
  # Args:
  #   conn: <PostgreSQLConnection> PostgreSQL connection object.
  #   devicesCleanTableName: <chr> Name of ACT devices table.
  #   linkMoreSecureTableName: <chr> Name of link table.
  #
  # Returns:
  #   devices_data: <data.frame> Formatted devices data composed of all columns
  #   in ActDevicesSchema and a patient_id <chr> column).

  devices_data <- dbGetQuery(conn,
    sprintf("SELECT * FROM \"%s\"", devices_clean_table_name))

  link_data <- dbGetQuery(conn,
    sprintf("SELECT * FROM \"%s\"", link_more_secure_table_name))

  devices_data <- inner_join(devices_data,
                            link_data[, c("patient_record_id", "study_email")],
                            by = "study_email")

  devices_data %>%
    rename("patient_id" = "patient_record_id") %>%
    transform(
      #if_other_please_state = as.character(if_other_please_state),
      #if_other_please_state__1 = as.character(if_other_please_state__1),
      #if_other_please_state__2 = as.character(if_other_please_state__2),
      patient_id = as.character(patient_id),
      study_email = as.character(study_email)
    )
}


fetch_patient_ids_from_devices <- function(devices_data) {
  # Returns a vector of patient ids from formatted devices data.
  #
  # Args:
  #   devices_data: data.frame with a patient_ids column.
  #
  # Returns:
  #   patient_ids: vector of <chr> with each <chr> being a patient id.

  devices_data %>% pull("patient_id") %>% unique()
}

clean_step <- function(conn, pid, input_table_name, output_table_name,
    start_date, end_date, devices_data, raw_meta_table_name,
    clean_meta_table_name) {

    writeLines(pid)
    un_processed_sessions <- tbl(conn, raw_meta_table_name) %>%
        filter(patient_id == pid &
            date >= start_date &
            date < end_date) %>%
        select(session_id) %>%
        distinct() %>%
        collect()

    if (nrow(un_processed_sessions) == 0) {

        writeLines(sprintf("No data for patient %s exists", pid))
        return ()
    }

    sessions_to_clean <- un_processed_sessions$session_id

    if (dbExistsTable(conn, output_table_name)) {
        processed_sessions <- tbl(conn, clean_meta_table_name) %>%
            filter(patient_id == pid &
                date >= start_date &
                date < end_date) %>%
            select(session_id) %>%
            distinct() %>%
            collect()

        processed_sessions <- processed_sessions$session_id
        remaining_sessions <- setdiff(sessions_to_clean, processed_sessions)
    }else{

            remaining_sessions <- sessions_to_clean
    }

    if (length(remaining_sessions) == 0){

        writeLines(sprintf("cleaned all days for patient ", pid))
        return ()
    }else{


        data <- tbl(conn, input_table_name) %>%
            filter(patient_record_id == pid &
            id %in% remaining_sessions) %>%
            collect()

        cleaned_data <- clean_raw_act_data(data)

        df <- data %>%
            select(session_id = id, patient_id = patient_record_id, time) %>%
            group_by(patient_id, session_id) %>%
            summarize(date = as.Date(min(time, na.rm = TRUE))) %>%
            collect()

        dbWriteTable(conn, output_table_name,
            cleaned_data, append = TRUE, row.names = FALSE)

        dbWriteTable(conn, clean_meta_table_name,
            df, append = TRUE, row.names = FALSE)

        }

}


label_step <- function(conn, pid, input_table_name, output_table_name,
    start_date, end_date, devices_data, raw_meta_table_name,
    meta_act_table_name, label_meta_table_name) {

    meta <- dbGetQuery(conn,
        sprintf("SELECT * FROM \"%s\"", meta_act_table_name))

    un_processed_sessions <- tbl(conn, meta_act_table_name) %>%
        filter(patient_id == pid &
            date >= start_date &
            date < end_date) %>%
        select(session_id) %>%
        distinct() %>%
        collect()

    if (nrow(un_processed_sessions) == 0) {
        writeLines(sprintf("No data for patient %s exists", pid))
        return ()
    }

    sessions_to_label <- sort(un_processed_sessions$session_id)

    if (dbExistsTable(conn, output_table_name)) {
        processed_sessions <- tbl(conn, label_meta_table_name) %>%
            filter(patient_id == pid &
                date >= start_date &
                date < end_date) %>%
            select(session_id) %>%
            distinct() %>%
            collect()

        processed_sessions <- sort(processed_sessions$session_id)
        remaining_sessions <- setdiff(sessions_to_label, processed_sessions)
    }else{

            remaining_sessions <- sessions_to_label
    }

    if (length(remaining_sessions) == 0){

        writeLines(sprintf("labelled all sessions for patient ", pid))
        return ()

    }else{

        data <- tbl(conn, input_table_name) %>%
            filter(patient_id == pid &
            time >= start_date & time < end_date &
            session_id %in% remaining_sessions) %>%
            collect()

        label_data <- label_act_data(data, meta) %>%
            mutate_if(is.integer, as.numeric)
        # when counting breaths order by session start not time
        #   if normal ordering by session start is same as ordering by time
        #   if date slip, then ordering by session
        #   start wont create intersection of data

        df <- label_data %>%
            group_by(patient_id, session_id) %>%
            summarize(date = as.Date(min(time, na.rm = TRUE))) %>%
            collect()

        dbWriteTable(conn, output_table_name,
            label_data, append = TRUE, row.names = FALSE)

        dbWriteTable(conn, label_meta_table_name,
            df, append = TRUE, row.names = FALSE)

    }

}


featurise_step <- function(conn, pid, input_table_name,
    output_table_name, start_date, end_date, devices_data,
    label_treatment_meta, output_meta) {
  #  Perform the ACT featurisation step on labelled data
  #  for a patient on a date range
  #
  # Args:
  #  conn <PostgreSQLConnection> : PostgreSQL connection object
  #  patient_id <Char> : Patient Id
  #  input_table_name <Char> : Name of the input table that an ACT pipeline
  #                          step would read from.
  #  output_table_name <Char> : Name of the output table that an ACT pipeline
  #                           step would write to.
  #  startDate <Date> : The ACT pipeline step will process patient data from
  #                     startDate to endDate.
  #  endDate <Date> : The ACT pipeline step will process patient data from
  #                   startDate to endDate.
  #  devices_data <data.frame> : Formatted devices data composed of all columns
  #                   in ActDevicesSchema and a patient_id <chr> column).
  # Returns:
  #   None
  writeLines(sprintf("Performing the featurise step on patient %s", pid))
  data <- NULL
  date_range <- as.numeric(
    as.Date(end_date) - as.Date(start_date), units = "days")

  if ( (date_range < 7) | (date_range %% 7 != 0)) {

     writeLines(
        sprintf("Featurisation must occur on at least a week of
                    data and must be a multiple of 7 for treatment
                    adherence features to be valid."
        )
    )

     return()
  }

  processed_ids <- c()

  # Get processed treatment ids
  if (dbExistsTable(conn, output_meta)) {

    processed_ids <- tbl(conn, output_meta) %>%
      filter(patient_id == pid &
             act_date >= start_date &
             act_date < end_date) %>%
      select(treatment_id) %>%
      collect() %>%
      unique()
    processed_ids <- processed_ids$treatment_id
  }
  # get treatments to process
  if (length(processed_ids) == 0) {

    to_processed_ids <- tbl(conn, label_treatment_meta) %>%
      filter(patient_id == pid &
             session_start >= start_date &
             session_start < end_date) %>%
      select(treatment_id) %>%
      collect() %>%
      unique()

  } else {

    to_processed_ids <- tbl(conn, label_treatment_meta) %>%
      filter(patient_id == pid &
             session_start >= start_date &
             session_start < end_date &
           !(treatment_id %in% processed_ids)) %>%
      select(treatment_id) %>%
      collect() %>%
      unique()

  }

  to_processed_ids <- to_processed_ids$treatment_id

  # Validate that patient has data for the specified date range
  if (length(to_processed_ids) == 0) {

    writeLines(sprintf("No data for patient %s exists or patient was already \
                       processed in date range %s - %s",
                       pid, start_date, as.Date(end_date) - lubridate::days(1)))
    return()
  }else{

      data <- tbl(conn, input_table_name) %>%
        filter(patient_id == pid &
               (treatment_id %in% to_processed_ids)) %>%
        collect()
   }

  meta_treatments <- tbl(conn, label_treatment_meta) %>%
    filter(patient_id == pid & treatment_id %in% to_processed_ids) %>%
    collect() %>%
    mutate(act_date = as.Date(session_start)) %>%
    select(patient_id, treatment_id, act_date)

  # Time featurisation
  start_featurising <- Sys.time()
  featurised_data <- featurise_act_data(data)
  featurised_data <- featurised_data %>%
    filter(breath_count > 0 & pressures_completeness_score == 1)

  end_featurising <- Sys.time()
  treatment_features <- featurised_data %>%
    select(
      patient_id,
      treatment_id,
      act_date,
      breath_count,
      pressures_completeness_score
    )
  # Time Featurise Treatment Adherence Score
  start_time <- Sys.time()
  treatment_adherence_features <- featurise_treatment_adherence(
    treatment_features,
    devices_data
  )
  end_time <- Sys.time()
  writeLines(sprintf("Featurising took (mins): %s",
    difftime(end_featurising, start_featurising, units = "mins")))

  writeLines(sprintf("Processing took (mins): %s",
    difftime(end_time, start_time, units = "mins")))

  joined_act_features <-
    left_join(treatment_adherence_features, featurised_data) %>%
    mutate_if(is.integer, as.numeric)

  writeLines(sprintf("Writing %s rows to %s",
    nrow(joined_act_features), output_table_name))

  dbWriteTable(conn, output_table_name,
    joined_act_features, append = TRUE, row.names = FALSE)

  dbWriteTable(conn, output_meta,
    meta_treatments, append = TRUE, row.names = FALSE)

}
