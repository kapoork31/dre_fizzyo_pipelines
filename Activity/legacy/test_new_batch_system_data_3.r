################################################################################
# batching_utils.R
#
#
# Available functions:
#
################################################################################

source("~/scripts/fitbit_pipeline_cleaning_kunal/utils/fitbit_cleaning_utils.R")
source("~/scripts/fitbit_pipeline_cleaning_kunal/utils/fitbit_validation.R")
source("~/scripts/fitbit_pipeline_cleaning_kunal/utils/fitbit_utils_4.R")
source("~/scripts/fitbit_pipeline_cleaning_kunal/utils/pre_featurise_clean.R")

cleanAll <- function(conn,startDate,endDate,inputTableName ,outputTableName,link,raw_hr_meta,clean_hr_meta){
    
    patientIds <- FetchAllPatientIdsFromLink(link)
    #processedPatients <- FetchProcessedPatients(conn, outputTableName)
    #unprocessedPatientIds <- FetchUnprocessedPatients(conn, patientIds, processedPatients, outputTableName)
    for (patientId in patientIds) {
    
        CleanStep(conn,startDate,endDate,patientId, inputTableName,outputTableName,raw_hr_meta,clean_hr_meta)
 
    }
}

featuriseAll <- function(conn,startDate,endDate,inputTableNameHR,inputTableNameFS,hr_clean_meta,fs_meta,outputTableName,link,outputTableNameMeta){
    patientIds <- FetchAllPatientIdsFromLink(link)
    #processedPatients <- FetchProcessedPatients(conn, outputTableName)
    #unprocessedPatientIds <- FetchUnprocessedPatients(conn, patientIds, processedPatients, outputTableName)
    for (patientId in patientIds) {
    
        featuriseStep(conn,startDate,endDate,patientId, inputTableNameHR, inputTableNameFS,hr_clean_meta,fs_meta,outputTableName,link,outputTableNameMeta,threshTable)
 
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

CleanStep <- function(conn,startDate,endDate,patient, inputTableName, outputTableName,raw_hr_meta,clean_hr_meta) {
    writeLines(patient)
    #print(startDate)
    hrMetadata <- tbl(conn, raw_hr_meta) %>%
        filter(userid == patient &
            date >= startDate &
            date < endDate)%>%
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
            filter(userid == patient &
                date >= startDate &
                date < endDate) %>%
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
        writeLines(sprintf("cleaned all days for patient or no data for", patient))
        return ()       
    }
    if(length(datesToClean) == 1 & length(remainingDays) == 1){
        writeLines(sprintf("only one day of data for %s, need to have more as last day is not cleaned", patient))
        return ()
    }
    if(length(datesToClean) > 1 & length(remainingDays) == 1){
        writeLines(sprintf("patient fully cleaned, last day remains", patient))
        return()
    }

    for (i in 1:(length(remainingDays) - 1)) {
        stDate <- remainingDays[i]
        eDate <- stDate + 1
        data <- tbl(conn, inputTableName) %>%
            filter(userid == patient &
            time >= stDate &
            time < eDate)%>%
            arrange(time)%>%
            collect()
                
        #writeLines(sprintf("  Start date: %s", startDate))
        #writeLines(sprintf("  End date: %s", endDate))
            
        cleanedData = remove70Leniant1(data)
        cleanedData = removeRepPointsdata(cleanedData)
        cleanedData = removeOutlierPoints(cleanedData)
        cleanedData = removeRandomPoints(cleanedData)
        cleanedData = removeRandomPoints(cleanedData)

        df = data.frame(remainingDays[i],patient)
        colnames(df) = c('date','userid')
        #cleaned(data)
        #if(nrow(cleanedData) == 0){
        #    cleanedData = data[1,]
        #    cleanedData$value = 0
        #}
        ValidateDataSchema(cleanedData,FitbitCleanHrSchema)
        dbWriteTable(conn,clean_hr_meta,df,append = TRUE, row.names = FALSE)
        dbWriteTable(conn, outputTableName, cleanedData, append = TRUE, row.names = FALSE)
            
    }

}

featuriseStep <- function(conn,startDate,endDate,patient, inputTableNameHR,inputTableNameFS,hr_clean_meta,fs_meta,outputTableName,link,outputTableNameMeta,threshTable) {
    patient = 'cf518ed4-ed66-449d-a8ca-14cee26e80a9'
    writeLines(patient)
    dateRec = link[link$fizzyo_hub_id == patient,'date_recruited']
    hrMetadata <- tbl(conn, hr_clean_meta) %>%
        filter(userid == patient &
            date >= startDate &
            date < endDate) %>%
        select(date) %>%
        distinct() %>%
        collect()
    
    if (nrow(hrMetadata) == 0) {
        writeLines(sprintf("No data for patient %s exists", patient))
        return ()
    }
    
    fsMetadata <- tbl(conn, fs_meta) %>%
        filter(userid == patient &
            date >= startDate &
            date < endDate) %>%
        select(date) %>%
        distinct() %>%
        collect()
        
    datesToClean = unique(c(hrMetadata$date,fsMetadata$date))
    datesToClean = datesToClean[datesToClean >= dateRec]
    
    if (dbExistsTable(conn, outputTableNameMeta)) {
        processedDays <- tbl(conn, outputTableNameMeta) %>%
            filter(userid == patient &
            date >= startDate &
            date < endDate) %>%
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
    
    #res = data.frame()
    #res2 = data.frame()

    for(d in as.list(remainingDays)){
        st = d
        ed = d + 1
        dataHR <- tbl(conn, inputTableNameHR) %>%
            filter(userid == patient &
            time >= st &
            time < ed) %>%
            arrange(time)%>%
            collect()
            
        dataFS <- tbl(conn, inputTableNameFS) %>%
            filter(userid == patient &
            time >= st &
            time < ed)%>%
            arrange(time)%>%
            collect()
        start_time <- Sys.time()


        if(nrow(dataFS)>1 & nrow(dataHR)>1 & min(dataHR$value)>0){ # make sure data for both hr and fs exist
            
            ValidateStepsData(dataFS,link)
            ValidateHRData(dataHR,link)
            rawFitbitData <- CombineFootstepsHR(dataFS, dataHR)  
            featurisedFitbitData <- FeaturiseFitbitCombined(rawFitbitData,conn)
            
            #xap.db.writeframe(featurisedFitbitData,'test_ff')
            
            #ValidateDataSchema(featurisedFitbitData,FitbitFeaturesSchema_2020_12)
            #dbWriteTable(conn, outputTableName, featurisedFitbitData, append = TRUE, row.names = FALSE)
            
             # if hr or fs dont exist then add date and userid but everything else will be na
        }  
        df = data.frame(d,patient)
        colnames(df) = c('date','userid')
        #res2 = rbind(res2,df)
        #dbWriteTable(conn, outputTableNameMeta, df, append = TRUE, row.names = FALSE)
        end_time <- Sys.time()
        print(end_time - start_time)
    }
}


