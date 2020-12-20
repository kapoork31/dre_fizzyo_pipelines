################################################################################
# act_pipeline_utils.R
#
# This script contains utilites for the ACT pipeline.
#
# Entrypoint Function: ProcessPatients()
#
# ------------------------------------------------------------------------------
#
# Functions in this file:
#
# ProcessPatients: Processes patients for a step in the ACT pipeline.
# CreateDateSeq: Creates a date sequence for the ProcessPatients function.
# FetchAllPatientIds: Fetches all unique patient ids from input table.
# FetchDevices: Fetches and formats ACT device data.
# CleanStep: Performs cleaning on a set of input data.
# LabelStep: Performs labeling on a set of (cleaned) input data.
# FeaturiseStep: Perform the ACT featurisation step on labelled data.
#
################################################################################

library(DBI)
library(dplyr)
library(dbplyr)
library(lubridate)


source("~/scripts/ACT_v3/1_featurise/utils/act_schema_utils.R")
source("~/scripts/ACT_v3/1_featurise/utils/act_cleaning_utils.R")
source("~/scripts/ACT_v3/1_featurise/utils/act_labelling_utils.R")
source("~/scripts/ACT_v3/1_featurise/utils/act_featurisation_utils.R")
source("~/scripts/ACT_v3/1_featurise/utils/treatment_grouping.R")


ProcessPatients <- function(conn, patientIds, inputTableName,
outputTableName, dateSeq, pipelineStepFn, ...) {
  # Performs an ACT pipeline step on all unprocessed patients for a given
  # time interval.
  #
  # Args:
  #   conn: <PostgreSQLConnection> PostgreSQL connection object.
  #   patientIds: <chr> Vector of all patient ids to process.
  #   inputTableName: <chr> Name of the input table that an ACT pipeline
  #                    step would read from.
  #   outputTableName: <chr> Name of the output table that an ACT pipeline
  #                    step would write to.
  #   dateSeq: <seq> Sequence of dates to do processing on. For e.g.
  #            c(seq(startDate, endDate, by = "week"), endDate) where this
  #            could output c("2019-04-01", "2019-04-08", "2019-04-13"). The
  #            date ranges processed is inclusive for the first date and exclusive
  #            for the last date (e.g. for the above example, 2019-04-13 will
  #            not be processed)
  #   pipelineStepFunction: <function> ACT pipeline step function. One of:
  #                         CleanStep, LabelStep, FeaturiseStep.
  #   ...: Other args to provide to the pipelineStepFunction.
  #
  # Returns:
  #   None

  if (outputTableName == "pressure_raw_vals") {
    stop("Output table name should not be equal to pressure_raw_vals.")
  }

  if(!dbExistsTable(conn, inputTableName)) {
    stop(sprintf("Input table %s does not exist.", inputTableName))
  }

  if(length(patientIds) == 0) {
    stop("No patient ids were provided to perform processing.")
  }

  patientCount <- 1
  processingStartTime <- Sys.time()
  for (patientId in patientIds) {
    writeLines(sprintf("Processing patient id %s", patientId))
    stepStartTime <- Sys.time()
    for (i in 1:(length(dateSeq) - 1)) {
      startDate <- dateSeq[i]
      endDate <- dateSeq[i + 1]
      
      # Validate date range
      if (startDate > endDate) {
        stop("Error: The startDate must be a date before the endDate.")
      }

      writeLines(sprintf("  Start date: %s", dateSeq[i]))
      writeLines(sprintf("  End date: %s", dateSeq[i + 1] - lubridate::days(1)))

      pipelineStepFn(
        conn,
        patientId,
        inputTableName,
        outputTableName,
        startDate,
        endDate,
        ...
      )
    }
    stepEndTime <- Sys.time()
    writeLines(sprintf("Step took (mins): %s",
                        difftime(stepEndTime, stepStartTime, units = "mins")))
    writeLines(sprintf("%s patients left to process", length(patientIds) - patientCount))
    patientCount <- patientCount + 1
  }
  processingEndTime <- Sys.time()
  writeLines(sprintf("Total Execution Time (mins): %s",
                      difftime(processingEndTime, processingStartTime,
                                units = "mins")))
}


CreateDateSeq <- function(startDate, endDate, by = "week") {
  # Creates a date sequence.
  #
  # Args:
  #   startDate: <chr> "YYYY-MM-DD"
  #   endDate: <chr> "YYYY-MM-DD"
  #   by: <chr> "day", "week", "month", "quarter" or "year"
  #
  # Returns:
  #   dateSeq: a sequence of dates

  c(seq(startDate, endDate, by = by), endDate)
}


FetchAllPatientIds <- function(conn, inputTableName, patientIdColName) {
  # Returns a vector of patient ids.
  #
  # Args:
  #   inputTableName: <chr> Name of input table.
  #
  # Returns:
  #   patientIds: vector of <chr> with each <chr> being a patient id.

  if(!dbExistsTable(conn, inputTableName)) {
    stop(sprintf("Input table %s does not exist.", inputTableName))
  }
  
  patientIdCol <- sym(patientIdColName)

  patientIds <- tbl(conn, inputTableName) %>%
    select(patientIdColName) %>%
    collect() %>%
    unique()
    
  patientIds[[patientIdColName]]
}


FetchDevices <- function(conn, devicesCleanTableName, linkMoreSecureTableName) {
  # Returns a formatted devices data.frame necessary for
  # FetchAllPatientIdsFromDevices and for the FeaturisationStep's
  # FeaturiseTreatmentAdherence function call.
  #
  # Args:
  #   conn: <PostgreSQLConnection> PostgreSQL connection object.
  #   devicesCleanTableName: <chr> Name of ACT devices table.
  #   linkMoreSecureTableName: <chr> Name of link table.
  #
  # Returns:
  #   devicesData: <data.frame> Formatted devices data composed of all columns
  #   in ActDevicesSchema and a patientId <chr> column).

  devicesData <- DBI::dbReadTable(conn, devicesCleanTableName)
  linkData <- DBI::dbReadTable(conn, linkMoreSecureTableName)

  devicesData <- inner_join(devicesData,
                            linkData[ , c("patient_record_id", "study_email")],
                            by = "study_email")

  devicesData %>%
    rename("patientId" = "patient_record_id") %>%
    transform(
      if_other_please_state = as.character(if_other_please_state),
      if_other_please_state__1 = as.character(if_other_please_state__1),
      if_other_please_state__2 = as.character(if_other_please_state__2),
      patientId = as.character(patientId)
    )
}


CleanStep <- function(conn, patientId, inputTableName, outputTableName,
startDate, endDate) {
  # Performs cleaning on a set of input data and then saves the cleaned ACT
  # data to the output table.
  #
  # Args:
  #   conn: <PostgreSQLConnection> PostgreSQL connection object.
  #   patientId: <chr> Patient id.
  #   inputTableName: <chr> Name of the input table.
  #   outputTableName: <chr> Name of the output table.
  #   startDate: <Date> The ACT pipeline step will process patient data from
  #              startDate to endDate.
  #   endDate: <Date> The ACT pipeline step will process patient data from
  #            startDate to endDate.

  processedIds <- c()
  data <- NULL

  # Get processed session ids
  if(dbExistsTable(conn, outputTableName)) {
    processedIds <- tbl(conn, outputTableName) %>%
      filter(patientId == patientId &
             time >= startDate &
             time <= endDate) %>%
      select(sessionId) %>%
      collect() %>%
      unique()

    processedIds <- processedIds$sessionId
  }

  # Get input data.frame with rows not containing processed session ids
  if(length(processedIds) != 0) {
    data <- tbl(conn, inputTableName) %>%
      filter(patient_record_id == patientId &
             time >= startDate &
             time <= endDate &
             !(id %in% processedIds)) %>%
      collect()
  } else {
    data <- tbl(conn, inputTableName) %>%
      filter(patient_record_id == patientId &
             time >= startDate &
             time <= endDate) %>%
      collect()
  }

  if (nrow(data) == 0) {
    writeLines(sprintf("No data for patient %s exists or patient was already processed in date range %s - %s",
                       patientId, startDate, endDate - lubridate::days(1)))
    return()
  }

  cleanedData <- CleanRawACTData(data)

  writeLines(sprintf("Writing %s rows to %s", nrow(cleanedData), outputTableName))
  dbWriteTable(conn, outputTableName, cleanedData, append = TRUE, row.names = FALSE)
}



			
			
LabelStep <- function(conn, patientId, inputTableName, outputTableName,
                      startDate, endDate, rawACTTableName, metaACTTableName) {
  # Performs labeling on a set of input data and then saves the labeled ACT
  # data to the output table. Labeling will identify breaths, breaks, sets, and
  # treatments.
  #
  # Args:
  #   conn: <PostgreSQLConnection> PostgreSQL connection object.
  #   patientId: <chr> Patient id.
  #   inputTableName: <chr> Name of the input table (usually cleaned ACT data).
  #   outputTableName: <chr> Name of the output table.
  #   startDate: <Date> The ACT pipeline step will process patient data from
  #              startDate to endDate.
  #   endDate: <Date> The ACT pipeline step will process patient data from
  #            startDate to endDate.
  #   rawACTTableName: <chr> Name of raw ACT table used to generate treatment labels.
  #            Should use pressure_raw_vals so you don't miss sessions.
  #   metaACTTableName: <chr> Name of meta data table. This table will be created
  #           to map session to treatment ids.
  #
  # Returns:
  #   None
  writeLines(sprintf("Starting LabelStep on %s", inputTableName))
  processedIds <- c()
  data <- NULL
  if (dbExistsTable(conn, outputTableName)) {
    processedIds <- tbl(conn, outputTableName) %>%
      filter(patientId == patientId &
              time >= startDate & time <= endDate) %>%
      select(sessionId) %>%
      collect() %>%
      unique()
    processedIds <- processedIds$sessionId
  }
  
  # Get input data.frame with rows not containing processed session ids
  if (length(processedIds) != 0) {
    data <- tbl(conn, inputTableName) %>%
      filter(patientId == patientId &
              time >= startDate & time <= endDate & !(sessionId %in% processedIds)) %>%
      collect()
  } else {
    data <- tbl(conn, inputTableName) %>%
      filter(patientId == patientId &
             time >= startDate & time <= endDate) %>%
      collect()
  }
  
  if (nrow(data) == 0) {
    writeLines(sprintf("No data for patient %s exists in date range %s - %s",
                       patientId, startDate, endDate - lubridate::days(1)))
    return()
  }
  
  if (!dbExistsTable(conn, metaACTTableName)) {
    writeLines("Generating session to treatment mapping table...")
    WriteSessionMetaData(conn, rawACTTableName, metaACTTableName)
  }
  
  meta <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", metaACTTableName))

  labelData <- LabelACTData(data, meta)
 
  writeLines(sprintf("Writing %s rows to %s", nrow(labelData), outputTableName))
  DBI::dbWriteTable(conn, outputTableName, labelData, append = TRUE, row.names = FALSE)
}



FeaturiseStep <- function(conn, patientId, inputTableName, outputTableName,
                          startDate, endDate, devicesData) {
  #  Perform the ACT featurisation step on labelled data
  #  for a patient on a date range
  #
  # Args:
  #  conn <PostgreSQLConnection> : PostgreSQL connection object
  #  patientId <Char> : Patient Id
  #  inputTableName <Char> : Name of the input table that an ACT pipeline
  #                          step would read from.
  #  outputTableName <Char> : Name of the output table that an ACT pipeline
  #                           step would write to.
  #  startDate <Date> : The ACT pipeline step will process patient data from
  #                     startDate to endDate.
  #  endDate <Date> : The ACT pipeline step will process patient data from
  #                   startDate to endDate.
  #  devicesData <data.frame> : Formatted devices data composed of all columns
  #                   in ActDevicesSchema and a patientId <chr> column).
  # Returns:
  #   None
  writeLines(sprintf("Performing the featurise step on patient %s", patientId))
  processedIds <- c()
  data <- NULL
  dateRange <- as.numeric(endDate - startDate, units = "days")
  if ((dateRange < 7) | (dateRange %% 7 != 0)) {
     writeLines(sprintf("Featurisation must occur on at least a week of data and must
                       be a multiple of 7 for treatment adherence features to be valid."))
     return()
  }
  # Get processed session ids
  if (dbExistsTable(conn, outputTableName)) {
    processedIds <- tbl(conn, outputTableName) %>%
      filter(patientId == patientId &
             actDate >= startDate &
             actDate <= endDate) %>%
      select(treatmentId) %>%
      collect() %>%
      unique()
    processedIds <- processedIds$treatmentId
  }
  # Get input data.frame with rows not containing processed session ids
  if (length(processedIds) == 0) {
    data <- tbl(conn, inputTableName) %>%
      filter(patientId == patientId &
             time >= startDate &
             time <= endDate) %>%
      collect()
  } else {
  data <- tbl(conn, inputTableName) %>%
    filter(patientId == patientId &
           time >= startDate &
           time <= endDate &
           !(treatmentId %in% processedIds)) %>%
    collect()
  }
  # Validate that patient has data for the specified date range
  if (nrow(data) == 0) {
    writeLines(sprintf("No data for patient %s exists or patient was already \
                       processed in date range %s - %s",
                       patientId, startDate, endDate - lubridate::days(1)))
    return()
  }
  # Time featurisation
  startFeaturising <- Sys.time()
  featurisedData <- FeaturiseACTData(data)
  endFeaturising <- Sys.time()
  treatmentFeatures <- featurisedData %>%
    select(
      patientId,
      treatmentId,
      actDate,
      breathCount,
      pressuresDayCompletenessScore
    )
  # Time Featurise Treatment Adherence Score
  startTime <- Sys.time()
  treatmentAdherenceFeatures <- FeaturiseTreatmentAdherence(
    treatmentFeatures,
    devicesData
  )
  endTime <- Sys.time()
  writeLines(sprintf("Featurising took (mins): %s", difftime(endFeaturising, startFeaturising,
                                                             units = "mins")))
  writeLines(sprintf("Processing took (mins): %s", difftime(endTime, startTime,
                                                            units = "mins")))
  joinedActFeatures <- left_join(treatmentAdherenceFeatures, featurisedData)
  writeLines(sprintf("Writing %s rows to %s", nrow(joinedActFeatures), outputTableName))
  dbWriteTable(conn, outputTableName, joinedActFeatures, append = TRUE, row.names = FALSE)
}
