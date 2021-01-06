################################################################################
# pipeline_utils.R
#
# This script contains utilites for the ACT pipeline.
#
# Entrypoint Function: ProcessUnprocessedPatients()
#
# ------------------------------------------------------------------------------
#
# Functions in this file:
#
# FetchAllDevices: Fetches and formats ACT device data.
# FetchAllPatientIdsFromDevices: Fetches all patient ids in trial.
# FetchUnprocessedPatients: Fetches all patient ids that weren't processed
#                           (i.e. patient ids that do not exist in the
#                           specified output table associated with an ACT
#                           pipeline step (clean, label, featurise)).
# ProcessUnprocessedPatients: Processes unprocessed patients for a step in the
#                             pipeline.
# CleanStep: Perform the ACT cleaning step on raw data.
# LabelStep: Perform the ACT labelling step on cleaned data.
# FeaturiseStep: Perform the ACT featurisation step on labelled data.
#
################################################################################

library(DBI)
library(dplyr)
library(dbplyr)
library(lubridate)

source("~/scripts/ACT_v3/act_pipeline/src/utils/act_schema_utils_kunal.R")
source("~/scripts/ACT_v3/act_pipeline/src/utils/act_cleaning_utils_kunal.R")
source("~/scripts/ACT_v3/act_pipeline/src/utils/act_labelling_utils_kunal.R")
source("~/scripts/ACT_v3/act_pipeline/src/utils/act_featurisation_utils_kunal_test_new_emma_features_21_10_2020.R")
source("~/scripts/ACT_v3/act_pipeline/src/utils/act_validation_utils.R")
options(digits.secs = 3)


ProcessUnprocessedPatients <- function(conn, inputTableName, outputTableName,
startDate, endDate, timeColName, devicesCleanTableName, linkMoreSecureTableName,
pipelineStepFn, ...) {
  # Performs an ACT pipeline step on all unprocessed patients for a given
  # time interval.
  #
  # Args:
  #   conn: <PostgreSQLConnection> PostgreSQL connection object.
  #   inputTableName: <chr> Name of the input table that an ACT pipeline
  #                    step would read from.
  #   outputTableName: <chr> Name of the output table that an ACT pipeline
  #                    step would write to.
  #   startDate: <Date> The ACT pipeline step will process patient data from
  #              startDate to endDate.
  #   endDate: <Date> The ACT pipeline step will process patient data from
  #            startDate to endDate.
  #   timeColName: <chr> The name of the column to filter on for startDate and
  #                endDate. For clean and label the column is "time". For
  #                featurise the column is "actDate".
  #   devicesCleanTableName: <chr> Name of devices table. (Necessary to
  #                          obtain all patients in trial and for the ACT
  #                          featurisation step.)
  #   linkMoreSecureTableName: <chr> Name of link table. (Necessary to
  #                            obtain all patients in trial.)
  #   pipelineStepFunction: <function> ACT pipeline step function. One of:
  #                         CleanStep, LabelStep, FeaturiseStep.
  #   ...: Other args to provide to the pipelineStepFunction.

  if (outputTableName == "pressure_raw_vals") {
    stop("Output table name should not be equal to the raw data table name.")
  }

  
  devices <- FetchAllDevices(conn, devicesCleanTableName, linkMoreSecureTableName)
  patientIds <- FetchAllPatientIdsFromDevices(devices)
  #processedPatients <- FetchProcessedPatients(conn, outputTableName, startDate, endDate, timeColName)
  #unprocessedPatientIds <- FetchUnprocessedPatients(conn, patientIds, processedPatients, outputTableName)

  patientCount <- 1
  processingStartTime <- Sys.time()
  for (patientId in patientIds) {
  
    writeLines(sprintf("Processing patient id %s", patientId))
    stepStartTime <- Sys.time()
    pipelineStepFn(
      conn,
      patientId,
      inputTableName,
      outputTableName,
      startDate,
      endDate,
      devices,
      ...
    )
    stepEndTime <- Sys.time()
    
    writeLines(sprintf("Step took (mins): %s",
                       difftime(stepEndTime, stepStartTime, units = "mins")))
    #writeLines(sprintf("%s patients left to process", length(unprocessedPatientIds) - patientCount))

    patientCount <- patientCount + 1
  }
  processingEndTime <- Sys.time()

  writeLines(sprintf("Total Execution Time (mins): %s",
                     difftime(processingEndTime, processingStartTime,
                              units = "mins")))
}


FetchAllDevices <- function(conn, devicesCleanTableName, linkMoreSecureTableName) {
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
  
  devicesData <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", devicesCleanTableName))
  linkData <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", linkMoreSecureTableName))
  
  devicesData <- inner_join(devicesData,
                            linkData[ , c("patient_record_id", "study_email")],
                            by = "study_email")
  
  devicesData %>%
    rename("patientId" = "patient_record_id") %>%
    transform(
      #if_other_please_state = as.character(if_other_please_state),
      #if_other_please_state__1 = as.character(if_other_please_state__1),
      #if_other_please_state__2 = as.character(if_other_please_state__2),
      patientId = as.character(patientId),
      study_email = as.character(study_email)
    )
}


FetchAllPatientIdsFromDevices <- function(devicesData) {
  # Returns a vector of patient ids from formatted devices data.
  #
  # Args:
  #   devicesData: data.frame with a patientIds column.
  #
  # Returns:
  #   patientIds: vector of <chr> with each <chr> being a patient id.

  devicesData %>% pull("patientId") %>% unique()
}


FetchProcessedPatients <- function(conn, outputTableName, startDate, endDate, timeColName) {
  if(!dbExistsTable(conn, outputTableName)) {
    return(c())
  }

  timeCol <- sym(timeColName)

  processedPatients <- tbl(conn, outputTableName) %>%
    filter(timeCol >= startDate & timeCol <= endDate) %>%
    select(patientId) %>%
    collect() %>%
    unique()
    
  if (nrow(processedPatients) == 0) {
    return(c())
  } else {
    return(processedPatients$patientId)
  }
}


FetchUnprocessedPatients <- function(conn, allPatientIds, processedPatients, outputTableName) {
  # Returns a vector of unprocessed patient ids for a given step in the
  # ACT pipeline.
  #
  # Args:
  #   conn: <PostgreSQLConnection> PostgreSQL connection object.
  #   allPatientIds: vector of <chr> Vector of all patient ids in the trial.
  #   outputTableName: <chr> Name of the output table that an ACT pipeline
  #                    step would write to.
  #
  # Returns:
  #   unprocessedPatientIds: vector of <chr> with each <chr> being a patient id.

  if (is.null(allPatientIds) | length(allPatientIds) == 0) {
    return(c())
  }

  if (!dbExistsTable(conn, outputTableName)) {
    writeLines("Output table for step does not exist, returning all patient ids...")
    return(allPatientIds)
  }

  unprocessedPatientIds <- setdiff(allPatientIds, processedPatients)

  if (length(unprocessedPatientIds) == 0) {
    return(c())
  }

  unprocessedPatientIds
}

interesectTimes <- function(meta,metaActTableName){
    cross = FALSE

    unProcessedSessions <- tbl(conn, metaActTableName) %>%
        filter(patientId == pid &
            date >= startDate &
            date < endDate) %>%
        distinct() %>%
        collect()%>%
        as.data.frame()
    
    for(i in 1:nrow(unProcessedSessions)){
        temp = unProcessedSessions[i,]
        st = temp$sessionstart
        ed = temp$sessionEnd
        for(j in i + 1:nrow(unProcessedSessions)){
            
        }
    }
    
}


CleanStep <- function(conn, pid, inputTableName, outputTableName,
startDate, endDate,devicesData,rawMetaTableName,cleanMetaTableName) {

  #  Perform the ACT clean step on raw data
  #  for a patient on a date range
  #
  # Args:
  #  conn <PostgreSQLConnection> : PostgreSQL connection object
  #  pid <Char> : Patient Id
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
    
    writeLines(pid)
    unProcessedSessions <- tbl(conn, rawMetaTableName) %>%
        filter(patientId == pid &
            date >= startDate &
            date < endDate) %>%
        select(sessionId) %>%
        distinct() %>%
        collect()
        
    if (nrow(unProcessedSessions) == 0) {
        writeLines(sprintf("No data for patient %s exists", pid))
        return ()
    }
    
    sessionsToClean = unProcessedSessions$sessionId
    
    if (dbExistsTable(conn, outputTableName)) {
        processedSessions <- tbl(conn, cleanMetaTableName) %>%
            filter(patientId == pid &
                date >= startDate &
                date < endDate) %>%
            select(sessionId) %>%
            distinct() %>%
            collect()
    
        processedSessions <- processedSessions$sessionId
        remainingSessions = setdiff(sessionsToClean,processedSessions)
    }else{
            remainingSessions = sessionsToClean
    }
    
    #remainingDays = as.Date(remainingDays, origin = "1970-01-01")
    #remainingDays = sort(remainingDays)
    
    if(length(remainingSessions) == 0){
        writeLines(sprintf("cleaned all days for patient ", pid))
        return ()       
    }else{
        
        data <- tbl(conn, inputTableName) %>%
            filter(patient_record_id == pid & 
            id %in% remainingSessions) %>%
            collect()
        
        cleanedData <- CleanRawACTData(data)
        
        df <- data %>%
            select(sessionId = id, patientId = patient_record_id,time)%>%
            group_by(patientId,sessionId)%>%
            summarize(date = as.Date(min(time,na.rm = TRUE)))%>%
            collect()
            
        dbWriteTable(conn, outputTableName, cleanedData, append = TRUE, row.names = FALSE)
        dbWriteTable(conn, cleanMetaTableName, df, append = TRUE, row.names = FALSE)
        
        }
    
}
    

LabelStep <- function(conn, pid, inputTableName, outputTableName,
startDate, endDate,devicesData, rawActTableName, metaActTableName,labelMetaTableName) {

  #  Perform the ACT label step on cleaned data
  #  for a patient on a date range
  #
  # Args:
  #  conn <PostgreSQLConnection> : PostgreSQL connection object
  #  pid <Char> : Patient Id
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

    meta <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", metaActTableName))
    
    unProcessedSessions <- tbl(conn, metaActTableName) %>%
        filter(patientId == pid &
            date >= startDate &
            date < endDate) %>%
        select(sessionId) %>%
        distinct() %>%
        collect()
        
    if (nrow(unProcessedSessions) == 0) {
        writeLines(sprintf("No data for patient %s exists", pid))
        return ()
    }
    
    sessionsToLabel = sort(unProcessedSessions$sessionId)
    
    if (dbExistsTable(conn, outputTableName)) {
        processedSessions <- tbl(conn, labelMetaTableName) %>%
            filter(patientId == pid &
                date >= startDate &
                date < endDate) %>%
            select(sessionId) %>%
            distinct() %>%
            collect()
    
        processedSessions <- sort(processedSessions$sessionId)
        remainingSessions = setdiff(sessionsToLabel,processedSessions)
    }else{
            remainingSessions = sessionsToLabel
    }
    
    if(length(remainingSessions) == 0){
        writeLines(sprintf("labelled all sessions for patient ", pid))
        return ()
        
    }else{
    
        data <- tbl(conn, inputTableName) %>%
            filter(patientId == pid & 
            time >= startDate & time < endDate & 
            sessionId %in% remainingSessions) %>%
            collect()
        
        labelData <- LabelACTData(data, meta)
        # when counting breaths order by session start not time
        # if normal ordering by session start is same as ordering by time
        # if date slip, then ordering by session start wont create intersection of data
        
        df <- labelData %>%
            group_by(patientId,sessionId) %>%
            summarize(date = as.Date(min(time,na.rm = TRUE)))%>%
            collect()

        dbWriteTable(conn, outputTableName, labelData, append = TRUE, row.names = FALSE)
        dbWriteTable(conn, labelMetaTableName, df, append = TRUE, row.names = FALSE)
        
        
    }
    
}


FeaturiseStep <- function(conn, patient_Id, inputTableName, outputTableName,startDate, endDate,devicesData,label_treatment_meta) {
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
  #print(label_treatment_meta)
  
  writeLines(sprintf("Performing the featurise step on patient %s", patient_Id))
  processedIds <- c()
  data <- NULL
  dateRange <- as.numeric(as.Date(endDate) - as.Date(startDate), units = "days")
  if ((dateRange < 7) | (dateRange %% 7 != 0)) {
     writeLines(sprintf("Featurisation must occur on at least a week of data and must
                       be a multiple of 7 for treatment adherence features to be valid."))
     return()
  }
  # Get processed treatment ids
  if (dbExistsTable(conn, outputTableName)) {
    processedIds <- tbl(conn, outputTableName) %>%
      filter(patientId == patient_Id &
             actDate >= startDate &
             actDate < endDate) %>%
      select(treatmentId) %>%
      collect() %>%
      unique()
    processedIds <- processedIds$treatmentId
  }
  # get treatments to process
  if (length(processedIds) == 0) {
    ToProcessedIds <- tbl(conn, label_treatment_meta) %>%
      filter(patientId == patient_Id &
             sessionStart >= startDate &
             sessionStart < endDate) %>%
      select(treatmentId) %>%
      collect() %>%
      unique()
    
    } else {
    ToProcessedIds <- tbl(conn, label_treatment_meta) %>%
      filter(patientId == patient_Id &
             sessionStart >= startDate &
             sessionStart < endDate &
           !(treatmentId %in% processedIds)) %>%
      select(treatmentId) %>%
      collect() %>%
      unique()

  }
  
  ToProcessedIds <- ToProcessedIds$treatmentId
  
  # Validate that patient has data for the specified date range
  if (length(ToProcessedIds) == 0) {
    writeLines(sprintf("No data for patient %s exists or patient was already \
                       processed in date range %s - %s",
                       patient_Id, startDate, as.Date(endDate) - lubridate::days(1)))
    return()
  }else{
      data <- tbl(conn, inputTableName) %>%
        filter(patientId == patient_Id &
               time >= startDate &
               time < endDate &
               (treatmentId %in% ToProcessedIds)) %>%
        collect()
   }
  
  # Time featurisation
  startFeaturising <- Sys.time()
  featurisedData <- FeaturiseACTData(data)
  featurisedData = featurisedData %>%
    filter(breathCount > 0 & pressuresDayCompletenessScore == 1)
  
  endFeaturising <- Sys.time()
  treatmentFeatures <- featurisedData %>%
    select(
      patientId,
      treatmentId,
      actDate,
      breathCount,
      setCount,
      pressuresDayCompletenessScore,
      PeakPressureTreatmentAdherenceScore,
      PressureTreatmentAdherenceScore,
      PressureDuration_TreatmentAdherenceScore,
      
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
