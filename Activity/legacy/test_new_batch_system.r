################################################################################
# batching_utils.R
#
#
# Available functions:
#
################################################################################

source("~/scripts/fitbit_pipeline_cleaning_kunal/src/utils/fitbit_cleaning_utils.R")
source("~/scripts/fitbit_pipeline_cleaning_kunal/src/utils/fitbit_validation.R")
source("~/scripts/fitbit_pipeline_cleaning_kunal/src/utils/fitbit_utils.R")

library(DBI)

cleanAll <- function(conn, inputTableName ,outputTableName,link,raw_hr_meta,clean_hr_meta){
    
    patientIds <- FetchAllPatientIdsFromLink(link)
    #processedPatients <- FetchProcessedPatients(conn, outputTableName)
    #unprocessedPatientIds <- FetchUnprocessedPatients(conn, patientIds, processedPatients, outputTableName)
    for (patientId in patientIds) {
    
        CleanStep(conn,patientId, inputTableName,outputTableName,raw_hr_meta,clean_hr_meta)
 
    }
}

featuriseAll <- function(conn, inputTableNameHR,inputTableNameFS,hr_clean_meta,fs_meta,outputTableName,link){
    patientIds <- FetchAllPatientIdsFromLink(link)
    #processedPatients <- FetchProcessedPatients(conn, outputTableName)
    #unprocessedPatientIds <- FetchUnprocessedPatients(conn, patientIds, processedPatients, outputTableName)
    for (patientId in patientIds) {
    
        featuriseStep(conn,patientId, inputTableNameHR, inputTableNameFS,hr_clean_meta,fs_meta,outputTableName,link)
 
    }
}


FetchAllPatientIdsFromLink <- function(link) {
  # Returns a vector of patient ids from formatted devices data.
  #
  # Args:
  #   devicesData: data.frame with a patientIds column.
  #
  # Returns:
  #   patientIds: vector of <chr> with each <chr> being a patient id.

  link %>% pull("fizzyo_hub_id") %>% unique()
}


FetchProcessedPatients <- function(conn, outputTableName) {
  if(!dbExistsTable(conn, outputTableName)) {
    return(c())
  }

  processedPatients <- tbl(conn, outputTableName) %>%
    select(userid) %>%
    collect() %>%
    unique()
    
  if (nrow(processedPatients) == 0) {
    return(c())
  } else {
    return(processedPatients$userid)
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

CleanStep <- function(conn, patient, inputTableName, outputTableName,raw_hr_meta,clean_hr_meta) {
    writeLines(patient)
    hrMetadata <- tbl(conn, raw_hr_meta) %>%
        filter(userid == patient) %>%
        select(date) %>%
        distinct() %>%
        collect()
    if (nrow(hrMetadata) == 0) {
        writeLines(sprintf("No data for patient %s exists", patient))
        return ()
    }
    datesToClean = sort(hrMetadata$date)
    
    if (dbExistsTable(conn, clean_hr_meta)) {
        processedDays <- tbl(conn, clean_hr_meta) %>%
            filter(userid == patient) %>%
            select(date) %>%
            distinct() %>%
            collect()
    
        processedDays <- sort(processedDays$date)
        remainingDays = setdiff(datesToClean,processedDays)
    }else{
            remainingDays = datesToClean
    }
    
    remainingDays = as.Date(remainingDays, origin = "1970-01-01")
    remainingDays = sort(remainingDays)
    if(length(remainingDays) == 0){
        writeLines(sprintf("cleaned all days for patient ", patient))
        return ()       
    }
    if(length(datesToClean) == 1 & length(remainingDays) == 1 ){
        writeLines(sprintf("only one day of data for %s, need to have more as last day is not cleaned", patient))
        return ()
    }
    if(length(datesToClean) > 1 & length(remainingDays) == 1 ){
        writeLines(sprintf("patient fully cleaned, last day remains", patient))
        return()
        
    }else{
        for (i in 1:(length(remainingDays) - 1)) {
            startDate <- remainingDays[i]
            endDate <- startDate + 1
            data <- tbl(conn, inputTableName) %>%
                filter(userid == patient &
                time >= startDate &
                time <= endDate)%>%
                arrange(time)%>%
                collect()
                
            #writeLines(sprintf("  Start date: %s", startDate))
            #writeLines(sprintf("  End date: %s", endDate))
            
            cleanedData = removeRandomPoints(data)
            cleanedData = removeOutlierPoints(cleanedData)
            cleanedData = remove70(cleanedData)
            cleanedData = removeRepPointsdata(cleanedData)
            
            #cleaned(data)
            if(nrow(cleanedData) == 0){
                cleanedData = data[1,]
                cleanedData$value = 0
            }
            
            dbWriteTable(conn, outputTableName, cleanedData, append = TRUE, row.names = FALSE)
        }
    }

}

featuriseStep <- function(conn, patient, inputTableNameHR,inputTableNameFS,hr_clean_meta,fs_meta,outputTableName,link) {
    writeLines(patient)
    dateRec = link[link$fizzyo_hub_id == patient,'date_recruited']
    hrMetadata <- tbl(conn, hr_clean_meta) %>%
        filter(userid == patient) %>%
        select(date) %>%
        distinct() %>%
        collect()
    
    if (nrow(hrMetadata) == 0) {
        writeLines(sprintf("No data for patient %s exists", patient))
        return ()
    }
    
    fsMetadata <- tbl(conn, fs_meta) %>%
        filter(userid == patient) %>%
        select(date) %>%
        distinct() %>%
        collect()
        
    datesToClean = unique(c(hrMetadata$date,fsMetadata$date))
    datesToClean = datesToClean[datesToClean >= dateRec]
    
    if (dbExistsTable(conn, outputTableName)) {
        processedDays <- tbl(conn, outputTableName) %>%
            filter(userid == patient) %>%
            select(date) %>%
            distinct() %>%
            collect()
    
        processedDays <- sort(processedDays$date)
        remainingDays = setdiff(datesToClean,processedDays)
    }else{
            remainingDays = datesToClean
    }
    
    remainingDays = as.Date(remainingDays, origin = "1970-01-01")
    remainingDays = sort(remainingDays)
    
    if(length(remainingDays) == 0){
        writeLines(sprintf("featurised all days for patient ", patient))
        return ()       
    }
    
    
    for(d in as.list(remainingDays)){
        st = d
        ed = d + 1
        dataHR <- tbl(conn, inputTableNameHR) %>%
            filter(userid == patient &
            time >= st &
            time <= ed) %>%
            arrange(time)%>%
            collect()
            
        dataFS <- tbl(conn, inputTableNameFS) %>%
            filter(userid == patient &
            time >= st &
            time < ed)%>%
            arrange(time)%>%
            collect()
        
        if(nrow(dataFS)>1 & nrow(dataHR)>1 & min(dataHR$value)>0){
            ValidateStepsData(dataFS,link)
            ValidateHRData(dataHR,link)
            rawFitbitData <- CombineFootstepsHR(dataFS, dataHR)  
            featurisedFitbitData <- FeaturiseFitbitCombined(rawFitbitData)
            dbWriteTable(conn, outputTableName, featurisedFitbitData, append = TRUE, row.names = FALSE)
        }
    }
}
