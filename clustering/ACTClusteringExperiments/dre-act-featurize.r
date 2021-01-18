# Exclude Start

################################################################################
# dre-act-featurize.R
#
# This script  is meant to be run inside DRE. It will process 1 month 
# (or one week) for all patients  data:
# - detrend the data
# - extract breath features
# - extract adherence features 
# The results will be saved to a table.
# Script supports "restart after the time out" : it will check what 
# patients IDs are  present in the output table and will NOT process them again
# (focusing on patients that were not processed).
################################################################################


#source("~/scripts/1_featurise/utils/breath_featurise_utils.R")
#source("~/scripts/1_featurise/utils/breath_preprocessing_utils.R")
install.packages("RcppRoll")
install.packages("functional")

source("~/scripts/ACT_v2/utils/act_cleaning_utils.R")
source("~/scripts/ACT_v2/utils/act_labelling_utils_kunal.R")
source("~/scripts/ACT_v2/utils/act_featurisation_utils_olga.R")
source("~/scripts/ACT_v2/utils/batching_utils.R")
source("~/scripts/ACT_V2/utils/treatment_grouping.R")
source("~/scripts/ACT_V2/utils/core_utils.R")
source("~/scripts/Nicole Experiments/nicole_breath_secondary_utils.R")


library(dplyr)
library(lubridate)

ProcessOnePatient <- function(cleanACTData, devicesCleanData, sessionMetadata, outputTable, startDate, endDate) {
   #cleanACTData <- onePatientData
   #devicesCleanData <- devices
   #sessionMetadata <- sessions
   patientId <- cleanACTData$patientId[1]
   startTime <- Sys.time()
   print(unique(lubridate::as_date(cleanACTData$time)))
   cat("Labelling clean data \n")
   labelledACTData <- LabelACTData(cleanACTData, sessionMetadata)
   endTime <- Sys.time()
   cat(sprintf("Done labelling. Nrows = %s. \n",nrow(labelledACTData)))
   cat(sprintf("Labelling took (mins): %s \n.", difftime(endTime, startTime, units = "mins")))
   
   cat("Featurizing data \n")
   startTime <- Sys.time()
   featurisedACTData <- FeaturiseACTData(labelledACTData)
   endTime <- Sys.time()
   print(str(featurisedACTData))
   cat(sprintf("Featurizing took (mins): %s . Nrow=%s \n", 
              difftime(endTime, startTime, units = "mins"), nrow(featurisedACTData)))
   
   dataForTreatmentFeatures  <- featurisedACTData %>%select(
                                      patientId, treatmentId, actDate,
                                      breathCount, pressuresDayCompletenessScore) #arrange(actDate)

   startTime <- Sys.time()
   featuresAggTreatment <- FeaturiseTreatmentAdherence(dataForTreatmentFeatures, devicesCleanData, startDate, endDate)
   #pressureData, devicesData, startDate, endDate
   #pressureData <- dataForTreatmentFeatures 
   
   endTime <- Sys.time()
   cat(sprintf("FeaturiseTreatmentAdherence took (mins): %s. Nrow=%s \n", 
               difftime(endTime, startTime, units = "mins"), nrow(featuresAggTreatment)))
   cat("FeaturiseTreatmentAdherence results: \n")
   print(str(featuresAggTreatment))
   
   if (nrow(featuresAggTreatment) >  1) {
      print("Merge breath and treatment adherence features \n")
      features_all <- left_join(featuresAggTreatment, featurisedACTData)
                     #by.x = c("patientId","treatmentId","actDate"),
                      #    by.y = c("patientId","treatmentId","actDate")) 
      cat("Adding Secondary features \n")
      cat("Adding CumulativeAdherenceScore \n")
      act_features <- CumulativeAdherenceScore(features_all)
      cat("Adding SkippedTreatments \n")                  
      act_features <- SkippedTreatments(act_features)
   
      cat("Merge of all features \n")  
      act_features <- act_features %>%
                mutate(treatmentDuration = TreatmentDuration(pressuresDailyCount),
                       treatmentDuration = ifelse(is.na(treatmentDuration), 0, treatmentDuration))
                      
      print(str(act_features))  
      
      if (!CheckIfPatientDataExists(outputTable, patientId)) {
        dbWriteTable(xap.conn, outputTable, act_features,
                     row.names = FALSE, append = TRUE)
      }
      else {
         message <- sprintf("Wow, Patient %s has been processed already? Skippp writing to the table", patientId)
         LogMessage(message, printToScreen = TRUE, note = patientId, logTable = "olga_logTable")  
      }
    }
    else {
      message <- sprintf("Patient %s does not have any presription at all? Skipping further featurization", patientId)
      LogMessage(message, printToScreen = TRUE, level = 1, note = patientId, logTable = "olga_logTable")
    }
     
}

GetTimeSliceData <- function(startDate, endDate, table = "pressure_raw_vals ") {
  query <- sprintf("SELECT * from %s WHERE time >= '%s' AND time <= '%s' ORDER BY time" ,
                   table, startDate, endDate)
  df <- dbGetQuery(xap.conn, query)
  df
}

GetOnePatientData <- function(inputTable, patientColName ='patient_record_id', p){
  cat(sprintf("Load ACT data from: %s for patient %s \n", inputTable, p))
  query <- sprintf("SELECT * FROM %s WHERE \"%s\" = '%s'", inputTable, patientColName, p)
  df <- dbGetQuery(xap.conn, query)
  df
}

GetListOfUniquePatients <- function(tName, colName) {
   result <- character()
   conn <- xap.conn
   if (dbExistsTable(conn, tName)) {
      cat(sprintf("Reading list of patients output from: %s \n", tName))
      query <-sprintf("SELECT DISTINCT \"%s\" FROM %s", colName, tName)
      df <- dbGetQuery(conn, query)
      cat(sprintf("Total N of patients in %s is: %s \n", tName, nrow(df)))
      result <- df$patientId
   }
   
   result
}

GetPatientsPartition <- function(pPartition) {
   cat(sprintf("Partition: %s \n", pPartition))
   patientsPartition <- xap.read_table("patients_partition") 
   if (!is.null(pPartition)) {
       patients <- patientsPartition[patientsPartition$partition == pPartition, ]$patient_record_id
       cat(sprintf("Working on partition %s, got %s patients in the list \n", pPartition, length(patients)))
   }
   else
     patients <- patientsPartition$patient_record_id
   
   patients
}

CheckIfPatientDataExists <- function(tName, patientId) {
     conn <- xap.conn
     if (dbExistsTable(conn, tName)) {
       query <- sprintf("SELECT COUNT(*) FROM %s WHERE \"patientId\" = '%s'", tName, patientId) 
       df <- dbGetQuery(conn, query)
        df$count > 0
     }
     else
       FALSE
}


ProcessACTData <- function(inputTables, outputTable, sessionTable, devicesCleanTable, linkMoresecureTable, 
                           startDate, endDate, pPartition = NULL) {
# The function that loops though all the patients
# and calls processing.

   cat(sprintf("Load Session Metadata  from %s table\n",sessionTable))
   sessions <- xap.read_table(sessionTable)

   cat("Loading Prescription \n")
   devices<- xap.read_table(devicesCleanTable)
   linkMoresecData <- xap.read_table(linkMoresecureTable)
   devices <- merge(devices,
                          linkMoresecData[, c("patient_record_id", "study_email", "date_recruited")],
                          by.x = "study_email",
                          by.y = "study_email")
   colnames(devices)[colnames(devices) == "patient_record_id"] <- "patientId"
   #all_patients <- unique(devices$patientId)

   all_patients <- GetPatientsPartition(pPartition)

   outputPatientIds <- GetListOfUniquePatients(outputTable, "patientId")
  
   allPatientsStartTime <- Sys.time()
   i <- 0
   for(patientId in all_patients){
      onePatientData <- NULL
      for (inputTable in inputTables)
      {
         onePatientData_i <- GetOnePatientData(inputTable, "patientId", patientId) 
         onePatientData <- rbind(onePatientData, onePatientData_i)
      }

      if (nrow(onePatientData) == 0) {
          cat(sprintf("Data for patient %s  was not found \n", patientId))
          next
      }
       
      if (patientId %in% outputPatientIds) {
           message <- sprintf("Patient %s has been processed already", patientId)
           LogMessage(message, printToScreen = TRUE, note = sprintf("Partition: %s", toString(pPartition)), logTable = "olga_logTable")
           next
      }
      
      currEndTime <- Sys.time()
      cat(sprintf("In overall have been processing so far for: %s (since %s) \n", currEndTime - allPatientsStartTime, allPatientsStartTime))
      i <- i + 1
      
      message <- sprintf("Processing patient %s...", patientId)
      LogMessage(message, printToScreen = TRUE, level = 2, note = patientId, logTable = "olga_logTable")
      
      patientStartTime <- Sys.time()
      ProcessOnePatient(onePatientData, devices, sessions, outputTable, startDate, endDate)
      patientEndTime <- Sys.time()
      
      message <- sprintf("# %s Processing  for %s took (mins): %s", i, patientId, difftime(patientEndTime, patientStartTime, units = "mins"))
      LogMessage(message, printToScreen = TRUE, note = patientId, logTable = "olga_logTable")

   }

   endAll <- Sys.time()
   message <- sprintf("All done processing %s in: %s  (since %s). Results saved to %s", inputTables,
              difftime(endAll, allPatientsStartTime, units = "mins"), allPatientsStartTime, outputTable)
   LogMessage(message, printToScreen = TRUE, note = sprintf("Partition: %s", pPartition), logTable = "olga_logTable")
}

CleanACTData <- function(inputTable, outputTable, devicesCleanTable, linkMoresecureTable, pPartition) {
# The function that loops though all the patients
# and calls processing.
   conn <- xap.conn
   all_patients <- GetPatientsPartition(pPartition)

   outputPatientIds <- GetListOfUniquePatients(outputTable, "patientId")
   print(outputPatientIds)
   
   allPatientsStartTime <- Sys.time()
   i <- 0
   for(patientId in all_patients){
      onePatientData <- GetOnePatientData(inputTable, patientId)
      if (nrow(onePatientData) == 0) {
          cat(sprintf("Data for patient %s  was not found \n", patientId))
          next
      }
       
    if (patientId %in% outputPatientIds) {
           cat(sprintf("Patient %s has been processed already \n", patientId))
           next
      }
      
      currEndTime <- Sys.time()
      cat(sprintf("In overall have been cleaning data so far for: %s (since %s) \n", currEndTime - allPatientsStartTime, allPatientsStartTime))
      i <- i + 1
      cat(sprintf("Cleaning data for patient %s... \n", patientId))
      patientStartTime <- Sys.time()
      cleanACTData <- CleanRawACTData(onePatientData)
      writeLines("Writing cleaned data to table")
      dbWriteTable(conn, outputTable, cleanACTData, append = TRUE, row.names = FALSE)
      
      patientEndTime <- Sys.time()
      cat(sprintf("# %s Cleaning took  for %s took (mins): %s \n", i, patientId, difftime(patientEndTime, patientStartTime, units = "mins")))
   }

   endAll <- Sys.time()
   cat(sprintf("All done cleaning data %s in: %s  (since %s) \n. Results saved to %s \n",
                 inputTable, endAll - allPatientsStartTime, allPatientsStartTime, outputTable))
}

ProcessWeeklyFunc <- function() {
   inputTable <- "breath_test_data_month"
   outputTable <- 'breath_test_data_month_feat' 
   sessionTable <- "session_metadata1"
   
   devicesCleanTable <- "devices_clean"
   linkMoresecureTable <- "linkMoresecure"
   
   partition <- 1
   startDate <- "2019-04-01"
   endDate <- "2019-04-08"
   ProcessACTData(inputTable, outputTable, sessionTable, devicesCleanTable, linkMoresecureTable, startDate, endDate, partition)
}

ProcessWeeklyALL <- function() {
   t1 <- "breath_test_all_cleaned_week"
   cleanedACTtable <- c(t1)
       
   outputTable <- 'breath_test_all_data_week_feat_v3' 
   sessionTable <- "breath_test_all_meta_week"
   
   devicesCleanTable <- "devices_clean"
   linkMoresecureTable <- "linkMoresecure"
  
   startDate <- "2019-04-01"
   endDate <- "2019-04-08"
   ProcessACTData(cleanedACTtable, outputTable, sessionTable, devicesCleanTable, linkMoresecureTable, startDate, endDate)
}


ProcessMonthlyALL <- function() {
   #cleanedACTtable <- "breath_test_all_cleaned_month"
   #outputTable <- 'breath_test_all_data_month_feat_v2' 
   t1 <- "breath_test_all_cleaned_month"
   
   cleanedACTtable <- c(t1)
   outputTable <- 'breath_test_all_data_04_feat_v2'
   #sessionTable <- "breath_test_all_meta_month"
   sessionTable <- "session_metadata_jul"
   
   devicesCleanTable <- "devices_clean"
   linkMoresecureTable <- "linkMoresecure"
  
   startDate <- "2019-04-01"
   endDate <- "2019-04-30"
   partition <- 1
   ProcessACTData(cleanedACTtable, outputTable, sessionTable, devicesCleanTable, linkMoresecureTable, startDate, endDate, partition)
}

Process2Months <- function() {
   #cleanedACTtable <- "breath_test_all_cleaned_month"
   #outputTable <- 'breath_test_all_data_month_feat_v2' 
   #t1 <- "breath_test_all_data_02_cleaned"
   #t2 <- "breath_test_all_data_03_cleaned"
   
   t1 <- "breath_test_all_cleaned_dec_jan"
   cleanedACTtable <- c(t1)
   #outputTable <- 'breath_test_all_data_0203_feat_v2'
   outputTable <- 'breath_test_all_data_1201_feat_v2'
   #sessionTable <- "breath_test_all_meta_month"
   sessionTable <- "session_metadata_jul"
   
   devicesCleanTable <- "devices_clean"
   linkMoresecureTable <- "linkMoresecure"
  
   startDate <- "2018-12-01"
   endDate <- "2019-01-31"
   partition <- 2
   ProcessACTData(cleanedACTtable, outputTable, sessionTable, devicesCleanTable, linkMoresecureTable, startDate, endDate, partition)
}

ProcessSeveralMonths <- function() {
   t1 <- "breath_test_all_cleaned_sept_nov"
   t2 <- "breath_test_all_cleaned_dec_jan"
   t3 <- "breath_test_all_cleaned_feb_mar"
   t4 <- "breath_test_all_cleaned_apr_may"
   cleanedACTtable <- c(t1, t2, t3, t4)
   outputTable <- 'breath_all_data_feat_sept_may'
   sessionTable <- "session_metadata_jul"
   
   devicesCleanTable <- "devices_clean"
   linkMoresecureTable <- "linkMoresecure"
  
   startDate <- "2018-09-01"
   endDate <- "2019-05-31"
   partition <- 3
   ProcessACTData(cleanedACTtable, outputTable, sessionTable, devicesCleanTable, linkMoresecureTable, startDate, endDate, partition)
}

WriteAllMeta <- function (){
  rawACTtable <- "pressure_raw_vals"
  metaACTtable <- "breath_all_meta"
    # === Generating treatment grouping table ===
  writeLines("Generating metadata")
  WriteSessionMetaData(xap.conn, rawACTtable, metaACTtable)
  writeLines("Done with metadata")
}
CleanMonthData <- function() {
   #inputTable <- "breath_test_all_data_03"
   #outputTable <- "breath_test_all_data_03_cleaned"
   inputTable <- "breath_test_all_data_02"
   outputTable <- "breath_test_all_data_02_cleaned"
   
   
   devicesCleanTable <- "devices_clean"
   linkMoresecureTable <- "linkMoresecure"
   
   partition <- 1    
   CleanACTData(inputTable, outputTable, devicesCleanTable, linkMoresecureTable, partition)
}


CreateInputData <- function() {

  outTable <- "breath_test_all_data_sept_nov"

  sDate <- "2018-09-01"
  eDate <- "2018-11-30"

  inputTable <- "pressure_raw_vals"

  st <- Sys.time()
  dataSlice  <- GetTimeSliceData(sDate, eDate)
  en <- Sys.time()
  cat(sprintf("Filter in (mins): %s \n", difftime(en, st, units = "mins")))

  st <- Sys.time()
  dbWriteTable(xap.conn, outTable, dataSlice,
                     row.names = FALSE, append = TRUE)
  en <- Sys.time()
  cat(sprintf(" %s dbWriteTable in: %s \n", outTable,  difftime(en, st, units = "mins")))
}

MergeCleaned <- function(t1, t2) {
  #cat(sprintf("Get data from t2 = {%s} for dates [%s, %s]  and insert", t2, t2sDate, t2eDate))  
  cat(sprintf("Select data from %s and insert into %s", t2, t1))
  q <- sprintf("INSERT INTO %s SELECT * from %s", t1, t2)
  dbExecute(xap.conn, q)
  cat(sprintf("t1 %s now has data also for t2 %s", t1, t2))    
}

Merge0203 <- function() {
   t1 <- "breath_test_all_data_02_cleaned"
   t2 <- "breath_test_all_data_03_cleaned"
   #t2sDate <- "2019-03-01"
   #t2eDate <- "2019-03-31"
   MergeCleaned (t1, t2)
}


# Process  one month of data for ALL patients.
#ProcessMonthlyALL()

CreateZeros <- function () {
   featuresTable <- 'breath_test_all_data_month_feat' 
   devicesCleanTable <- "devices_clean"
   linkMoresecureTable <- "linkMoresecure"
  
   outputTable <- 'breath_test_all_zeros_month_feat'
  
   cat("Loading Prescription \n")
   devices<- xap.read_table(devicesCleanTable)
   linkMoresecData <- xap.read_table(linkMoresecureTable)
   devices <- merge(devices,
                          linkMoresecData[, c("patient_record_id", "study_email")],
                          by.x = "study_email",
                          by.y = "study_email")
   colnames(devices)[colnames(devices) == "patient_record_id"] <- "patientId"
   all_patients <- unique(devices$patientId)
   # TODO: expected ACT dates

   cat(sprintf("Load ACT featurized data from: %s \n", featuresTable))
   featuresData <- xap.read_table(featuresTable)
   
   all_dates <- sort(unique(featuresData$actDate))
   
   # Prevent the loss of class info in upcoming for-loop: https://www.r-bloggers.com/for-loops-in-r-can-lose-class-information/
   all_dates <- as.list(all_dates)
   processedPatientIds <- unique(featuresData$patientId)
   
   # Get sample schema
   dfRes <- head(featuresData, 0)
   
   # loop though all patients
   for(patientId in all_patients){
      cat(sprintf("Processing patient %s... \n", patientId))
      for (theDate in all_dates) {
          cat(sprintf("Processing date:  %s... \n", theDate))
          processedDateRow <- featuresData[(featuresData$patientId == patientId) & (featuresData$actDate == theDate),]
          if (nrow(processedDateRow) == 0) {
              cat(sprintf("Inserted NA features for date:  %s... \n", theDate))
              dfRes <- dfRes %>%
                       add_row (., actDate = theDate, patientId = patientId, treatmentId = "zeroTreatment" )
          }
      }
   }

   dfRes <- dfRes %>% replace(., is.na(.), 0)
   
   dbWriteTable(xap.conn, outputTable, dfRes,
                     row.names = FALSE, append = TRUE)
   cat(sprintf("Results saved to %s \n", outputTable))
}

FixWeeklyFeatures <- function() {
   featuresTable <- 'breath_test_all_data_month_feat' 
   featuresZeroTable <- 'breath_test_all_zeros_month_feat'
   outputTable <- 'breath_test_all_data_with_zeros_month_feat'
  
   cat(sprintf("Load ACT featurized data from: %s \n", featuresTable))
   featuresData <- xap.read_table(featuresTable)
   nrow(featuresData)
   
   cat(sprintf("Load zero features for no-act days from: %s \n", featuresZeroTable))
   featuresZeroData <- xap.read_table(featuresZeroTable)
   nrow(featuresZeroData)
   
   dfRes <- rbind(featuresData, featuresZeroData)
   nrow(dfRes)
   
   dfResFix <- dfRes %>%
    group_by(patientId, actWeek = week(actDate)) %>%
    mutate(
      treatmentWeekAdherenceScoreFix = max(treatmentWeekAdherenceScore),
      breathsWeekAdherenceScoreFix = max(breathsWeekAdherenceScore)
    ) %>%  ungroup()
    
    str(dfResFix)
    
    # Test
    p <- all_patients[1] 
    t <- dfResFix[(dfResFix$patientId == p) ,] 
    t <- t[order(t$actDate),] 
    tsmall <- t[c("actDate","treatmentId", "treatmentDayAdherenceScore", "treatmentWeekAdherenceScoreFix", "treatmentWeekAdherenceScore", "breathsDayAdherenceScore", "breathsWeekAdherenceScoreFix", "breathsWeekAdherenceScore")] 
    tsmall <- t %>% 
              select(actDate, treatmentId, treatmentDayAdherenceScore, treatmentWeekAdherenceScoreFix, treatmentWeekAdherenceScore, breathsDayAdherenceScore, breathsWeekAdherenceScoreFix, breathsWeekAdherenceScore) %>%
              rename(tD = treatmentDayAdherenceScore,
                     tWFix = treatmentWeekAdherenceScoreFix,
                     tW = treatmentWeekAdherenceScore, 
                     bD = breathsDayAdherenceScore, 
                     bWDFix = breathsWeekAdherenceScoreFix, 
                     bW = breathsWeekAdherenceScore)
                     
    tfeatures <- featuresData[(featuresData$patientId == p) ,] 
    tfeatures <- tfeatures[order(tfeatures$actDate),] 
    
    # Use *FIX* columns as the final ones
    dfResFix <- dfResFix %>%
                select(-treatmentWeekAdherenceScore,  -breathsWeekAdherenceScore, -actWeek) %>%
                rename(treatmentWeekAdherenceScore = treatmentWeekAdherenceScoreFix,
                       breathsWeekAdherenceScore = breathsWeekAdherenceScoreFix )
    str(dfResFix) 
    
    dbWriteTable(xap.conn, outputTable, dfResFix,
                     row.names = FALSE, append = TRUE)
    cat(sprintf("Results saved to %s \n", outputTable))
}

CheckAdherence <- function() {
   devicesCleanTable <- "devices_clean"
   linkMoresecureTable <- "linkMoresecure"
   
   featuresTable <- 'breath_test_all_data_month_feat' 
   featuresZeroTable <- 'breath_test_all_zeros_month_feat'
   featuresWithZeroTable <- 'breath_test_all_data_with_zeros_month_feat'
   
   devices<- xap.read_table(devicesCleanTable)
   linkMoresecData <- xap.read_table(linkMoresecureTable)
   devices <- merge(devices,
                          linkMoresecData[, c("patient_record_id", "study_email")],
                          by.x = "study_email",
                          by.y = "study_email")
   colnames(devices)[colnames(devices) == "patient_record_id"] <- "patientId"
   all_patients <- unique(devices$patientId)
   p <- all_patients[1] 
   
   features <- xap.read_table(featuresTable)
   t <- features[(features$patientId == p) ,]
   t <- t[order(t$actDate),]
   d <- t$actDate[1]
   td <- t[(t$actDate == d),]
   devices[(devices$patientId == p), ]
   
   source("~/scripts/ACTClusteringExperiments/breath_featurise_utils_debug.R")
   inputTable <- "breath_test_all_data_month"
   testData <- xap.read_table(inputTable)
   
   onePatientData <- testData[testData$patientId == p, ]
   onePatientData <- onePatientData[onePatientData[order(onePatientData$actDate),]]
   
   sessTable <- "session_metadata"
   sessionMetadata <- xap.read_table(sessTable)
   
   dfData <- merge(onePatientData,
                  sessionMetadata[, c("treatmentId", "sessionId")],
                  by.x = "id",
                  by.y = "sessionId")
   colnames(dfData)[colnames(dfData) == "id"] <- "sessionId"
   cat(sprintf("# of treatments for this patient: %s \n", length(unique(dfData$treatmentId))))
    
   pressureData <- dfData
   preProData <- PreprocessPressureData(pressureData,  groupCol = "treatmentId")
   
   features <- FeaturiseBreaths(preProData, groupCol = "treatmentId")
   
   dataForTreatmentFeatures  <- preProData %>% select(-data) %>% arrange(actDate)
   featuresAggTreatment <- FeaturiseTreatmentAdherence(dataForTreatmentFeatures, devices)
   dbWriteTable(xap.conn, "olga_FeaturiseTreatmentAdherence_debug", featuresAggTreatment,
                     row.names = FALSE, append = TRUE)
}
# Exclude End