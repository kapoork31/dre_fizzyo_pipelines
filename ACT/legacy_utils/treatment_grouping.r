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

WriteSessionMetaData <- function(conn, pressureTableName, metaDataTableName) {
  # Group sessions that are part of a treatment,
  # i.e. sessions that are within 1 hour of each other.
  # Builds the metadata table with format:
  #
  # patientId <chr> | sessionId <chr> | treatmentId <chr> |
  # sessionStart <datetime> | sessionEnd <datetime>
  #
  # Processes the entire set of historical data.

  sessionMetaData <- tbl(conn, pressureTableName) %>%
    select("patientId" = patient_record_id, "sessionId" = id, time) %>%
    group_by(patientId, sessionId) %>%
    summarize(sessionStart = min(time, na.rm = TRUE),
              sessionEnd = max(time, na.rm = TRUE)) %>%
    collect()

  sessionMetaDataWithTreatments <- sessionMetaData %>%
    mutate_at(c("sessionStart", "sessionEnd"), ~ as.POSIXct(., format = "%Y-%m-%d %H:%M:%OS")) %>%
    arrange(sessionStart) %>%
    mutate(
      delta = difftime(sessionStart, lag(sessionEnd, 1), units = "hours"),
      treatmentStart = ifelse(is.na(delta) | (delta >= 1), sessionId, NA),
      # for sessions with NA treatment ids, assign them a treatment id equal to
      # the most recent non-NA treatment id prior to it as they are part of that
      # treatment
      treatmentId = zoo::na.locf(treatmentStart)
    ) %>%
    select(patientId, sessionId, treatmentId, sessionStart, sessionEnd) %>%
    ungroup()

  tryCatch({
    if (metaDataTableName != "pressure_raw_vals") {
      DBI::dbWriteTable(conn, metaDataTableName, sessionMetaDataWithTreatments,
                        overwrite = TRUE, row.names = FALSE)
    }
  }, error = function(e) {
    writeLines(paste("Unable to write to metadata table. ", e))
  })
}
