################################################################################
# treatment_grouping.R
#
# This script contains a function for writing session metadata to a table.
#
# Available functions:
#   WriteSessionMetaData: Group sessions that are part of a treatment and
#                         writes session metadata and treatment ids to a
#                         metadata table.
#
################################################################################

library(DBI)
library(dplyr)
library(dbplyr)
library(zoo)

write_session_meta_data <- function(conn,
                                        pressure_table_name,
                                        meta_data_table_name
                                    ) {
  # Group sessions that are part of a treatment,
  # i.e. sessions that are within 1 hour of each other.
  # Builds the metadata table with format:
  #
  # patient_id <chr> | session_id <chr> | treatment_id <chr> |
  # session_start <datetime> | session_end <datetime>
  #
  # Processes the entire set of historical data.

  session_meta_data <- tbl(conn, pressure_table_name) %>%
    group_by(patient_id, session_id) %>%
    summarize(session_start = min(time, na.rm = TRUE),
              session_end = max(time, na.rm = TRUE)) %>%
    collect()

  session_meta_data_with_trs <- session_meta_data %>%
    mutate_at(c("session_start", "session_end"),
        ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%OS")) %>%
    arrange(session_start) %>%
    mutate(
      delta = difftime(session_start, lag(session_end, 1), units = "hours"),
      treatment_start = ifelse(is.na(delta) | (delta >= 1), session_id, NA),
      # for sessions with NA treatment ids, assign them a treatment id equal to
      # the most recent non-NA treatment id prior to it as they are part of that
      # treatment
      treatment_id = zoo::na.locf(treatment_start),
      date = as.Date(session_start)
    ) %>%
    select(patient_id, session_id,
        treatment_id, session_start, session_end, date) %>%
    ungroup()

  tryCatch({
    if (meta_data_table_name != "pressure_raw_vals") {
      DBI::dbWriteTable(conn, meta_data_table_name, session_meta_data_with_trs,
                        overwrite = TRUE, row.names = FALSE)
    }
  },
    error = function(e) {
        writeLines(paste("Unable to write to metadata table. ", e))
    }
  )

}


write_session_meta_data_lbld <- function(conn,
                                            pressure_table_name,
                                            meta_data_table_name
                                            ) {

    session_meta_data <- tbl(conn, pressure_table_name) %>%
    group_by(patient_id, treatment_id) %>%
    summarize(session_start = min(time, na.rm = TRUE),
              session_end = max(time, na.rm = TRUE)) %>%
    collect()

    DBI::dbWriteTable(conn, meta_data_table_name, session_meta_data,
                        overwrite = TRUE, row.names = FALSE)
}


write_session_meta_data_raw <- function(conn,
                                        pressure_table_name,
                                        meta_data_table_name
                                        ) {

    session_meta_data <- tbl(conn, pressure_table_name) %>%
    select("patient_id" = patient_record_id, "session_id" = id, time) %>%
    group_by(patient_id, session_id) %>%
    summarize(session_start = min(time, na.rm = TRUE),
              session_end = max(time, na.rm = TRUE)) %>%
    collect()

    session_meta_data$date <- as.Date(session_meta_data$session_start)

    DBI::dbWriteTable(conn, meta_data_table_name, session_meta_data,
                        overwrite = TRUE, row.names = FALSE)
}
