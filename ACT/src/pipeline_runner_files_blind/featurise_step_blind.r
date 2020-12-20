source("~/scripts/ACT_v3/act_pipeline/src/utils/pipeline_utils_kunal_2_blind_emma_thesis.R")
source("~/scripts/ACT_v3/act_pipeline/src/utils/treatment_grouping_utils_kunal.R")
#source("~/scripts/ACT_v3/act_pipeline/tests/featurisation_tests.R")


options(digits.secs = 3)

library(DBI)

conn <- xap.conn
linkMoreSecureTableName <- "linkMoresecure"
devicesCleanTableName <- "devices_clean_test"
devices <- FetchAllDevices(conn, devicesCleanTableName, linkMoreSecureTableName)

link = xap.read_table('patient_key_id_list')

inputTableName <- "act_labeled_new_blind_br_thresh_8"
outputTableName <- "act_featurised_blind_2020_12_v3"
timeColName <- "actDate"
pipelineStepFn <- FeaturiseStep
rawTableName <- "pressure_raw_sept_oct"
label_treatment_meta <- "labeled_treatment_meta_blind_br_thresh_8"
#WriteSessionMetaDataLabeled(conn,inputTableName,label_treatment_meta)

#nrow(temp[(temp$amplitude > 10 & temp$amplitude <20) |(temp$meanAmplitude > 10 & temp$meanAmplitude <20),])
for (i in unique(link$patient_record_id)){
    
    pdata = link[link$patient_record_id == i,]
    patient_Id = i
    startDate = pdata$date_recruited + 1
    endDate = pdata$date_feedback_start
    if(is.na(endDate)){
        endDate = startDate + 60
    }
    dif = as.integer(endDate - startDate)
    modSeven = 7 - (dif %% 7)
    endDate = endDate + modSeven
    #t = paste(as.integer(endDate - startDate),paste(startDate,endDate,sep = '_'),sep ='_')
    #print(t)
    FeaturiseStep(conn, i, inputTableName, outputTableName,startDate, endDate,devices,label_treatment_meta)
}


