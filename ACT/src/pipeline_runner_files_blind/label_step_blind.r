source("~/scripts/ACT_v3/act_pipeline/src/utils/pipeline_utils_kunal_3.R")
source("~/scripts/ACT_v3/act_pipeline/src/utils/treatment_grouping_utils_kunal.R")
#source("~/scripts/ACT_v3/act_pipeline/tests/labelling_tests.R")

options(digits.secs = 3)
library(DBI)
library(dplyr)
library(dbplyr)
library(lubridate)

conn <- xap.conn
linkMoreSecureTableName <- "linkMoresecure"
devicesCleanTableName <- "devices_clean_test"
link = xap.read_table('patient_key_id_list')

inputTableName <- "act_clean_test_blind"
outputTableName <- "act_labeled_new_blind_br_thresh_8"
timeColName <- "time"
pipelineStepFn <- LabelStep
rawTableName <- "pressure_raw_sept_oct"
metaTableName <- "clean_meta_sessions_blind"
labelMetaTableName = 'label_meta_sessions_new_blind_br_thresh_8'
devices <- FetchAllDevices(conn, devicesCleanTableName, linkMoreSecureTableName)


for (i in unique(link$patient_record_id)){
    
    pdata = link[link$patient_record_id == i,]
    startDate = pdata$date_recruited
    endDate = pdata$date_feedback_start
    if(is.na(endDate)){
        endDate = startDate + 60
    }
    #print(startDate)
    #print(endDate)
    print
    
    LabelStep(conn, i, inputTableName, outputTableName,startDate, endDate,devices,rawTableName,metaTableName,labelMetaTableName)
}