################################################################################
# batching_utils.R
#
#
# Available functions:
#
################################################################################

source("~/scripts/fitbit_pipeline_cleaning_kunal/src/utils/fitbit_cleaning_utils.R")
source("~/scripts/fitbit_pipeline_cleaning_kunal/src/utils/fitbit_validation.R")
source("~/scripts/fitbit_pipeline_cleaning_kunal/src/utils/fitbit_utils_3.R")
source("~/scripts/fitbit_pipeline_cleaning_kunal/src/srcutils/pre_featurise_clean.R")

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
                time < endDate)%>%
                arrange(time)%>%
                collect()
                
            #writeLines(sprintf("  Start date: %s", startDate))
            #writeLines(sprintf("  End date: %s", endDate))
            
            cleanedData = remove70(data)
            cleanedData = removeRepPointsdata(cleanedData)
            cleanedData = removeOutlierPoints(cleanedData)
            cleanedData = removeRandomPoints(cleanedData)
            cleanedData = removeRandomPoints(cleanedData)

            df = data.frame(startDate,patient)
            colnames(df) = c('date','userid')
            #cleaned(data)
            #if(nrow(cleanedData) == 0){
            #    cleanedData = data[1,]
            #    cleanedData$value = 0
            #}
            dbWriteTable(conn,clean_hr_meta,df,append = TRUE, row.names = FALSE)
            dbWriteTable(conn, outputTableName, cleanedData, append = TRUE, row.names = FALSE)
            
        }
    }

}

activeMinutesRegThresh <- function(patientId,date,wt,rhr){
    
    patientData <- tbl(conn, 'individual_mvpa_thresh') %>%
        filter(userid == patientId) %>%
        #select(date_recruited,age_recruited) %>%
        collect()
        
    
    
    #rd = patientData$date_recruited
    #ad = patientData$age_recruited

    #df = as.integer(date - rd)/365.25
    #age = ad + df
    
    df <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", 'age_mvpa_thresh'))
    index = which.min(abs(df$age_recruited - age))
    thresh = df[index,'daily_mvpa_thresh']
    return(thresh)
    
}


mvpaCountNeighbourTimesPrev <- function(activeHr,fs,windowSize){
    
    #activeHr = df[df$value >= thresh,]
    #matchActiveHRSteps = intersect(activeHr$time,fs$time)
    #matchActiveHRSteps = as_datetime(matchActiveHRSteps)
    #matchSteps = fs[fs$time %in% matchActiveHRSteps,]
    #print(nrow(matchSteps))
    noDupFs = fs[!duplicated(fs[,1]),]
    #matchActiveHRSteps = intersect(activeHr$time,noDupFs$time)
    
    timesIn = c()

    count = 0 
    for (t in unique(activeHr$time)){
        timeVal = as_datetime(t)
        #prvMinute = timeVal - 60

        #print(timeVal)
        prv = timeVal - (60*windowSize)
        #nx = timeVal + (60*windowSize)
        fsT = noDupFs[(noDupFs$time >= prv) & (noDupFs$time <= timeVal),]
        maxS = max(fsT$value, na.rm = TRUE)
        if(maxS > 0){
            count = count + 1
            timesIn = c(timesIn,timeVal)
        }
        
    }
    
    #timesIn = as_datetime(timesIn)
    #notIn = setdiff(unique(activeHr$time),timesIn)
    timesIn <- if (length(timesIn)>0) length(as_datetime(timesIn, tz = 'UTC')) else 0

    #timesIn = as_datetime(timesIn, tz = 'UTC')
    #print(nrow(matchSteps1))

    return(timesIn)
}


featuriseStep <- function(conn, patient, inputTableNameHR,inputTableNameFS,hr_clean_meta,fs_meta,outputTableName,link) {
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
    
    if (dbExistsTable(conn, outputTableName)) {
        processedDays <- tbl(conn, outputTableName) %>%
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
            
        #thresh = activeMinutesRegThresh(patient,d)
       
        if(nrow(dataFS)>1 & nrow(dataHR)>1 & min(dataHR$value)>0){ # make sure data for both hr and fs exist
        
            #tempHR = dataHR
            #tempHR$min = format(tempHR$time, format="%Y-%m-%d %H:%M") # create a min column
            #agg = aggregate(tempHR[c('value')], list(tempHR$min), mean) # aggregate by minute
            #colnames(agg) = c('time','value')	# change agg column names
            #agg$time = as.POSIXct(agg$time, format = "%Y-%m-%d %H:%M") # convert time column to time variable
            #activeHr = agg[agg$value > thresh,]
            #neighbour15Prev <- if (nrow(activeHr)>0 & nrow(dataFS) > 0) mvpaCountNeighbourTimesPrev(activeHr,dataFS,15) else 0
        
            ValidateStepsData(dataFS,link)
            ValidateHRData(dataHR,link)
            rawFitbitData <- CombineFootstepsHR(dataFS, dataHR)  
            #featurisedFitbitData <- FeaturiseFitbitCombined(rawFitbitData,thresh,neighbour15Prev)
            featurisedFitbitData <- FeaturiseFitbitCombined(rawFitbitData)
            thresh = activeMinutesRegThresh(patient,d,featurisedFitbitData$minsWear722,featurisedFitbitData$restingHr722)
            #if(featurisedFitbitData$activeMinsHrRegressionThresh >= 300){ # if min 300 active mins, check to see if day is valid
                
            #    validHighActivity = checkValid1(d,agg,dataFS)
                
            #}else{
            #    validHighActivity = 299
            #}
            
            #featurisedFitbitData$validHighActivity = validHighActivity
            
            dbWriteTable(conn, outputTableName, featurisedFitbitData, append = TRUE, row.names = FALSE)
            
        }else{ # if hr or fs dont exist then add date and userid but everything else will be na
            df = data.frame(d,patient)
            colnames(df) = c('date','userid')
            dbWriteTable(conn, outputTableName, df, append = TRUE, row.names = FALSE)

        }

    }
}


