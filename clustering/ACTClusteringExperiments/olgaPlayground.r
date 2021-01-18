
install.packages("RcppRoll")
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(DBI)
library(tidyr)


kActFeatures <- "breath_test_all_data_with_zeros_month_feat" #"breath_test_all_data_month_feat"
actDB <- xap.conn %>% tbl(kActFeatures)
act <- actDB %>% collect()
patientsId <- unique(act$patient_record_id)

idx <-1 
p <- patientsId[idx]
onePatient <- act[act$patient_record_id == p, ]
onePatient[order(onePatient$actDate), ]


rawACTtable <- "breath_test_all_data_month"
rawactDB <- xap.conn %>% tbl(rawACTtable)
rawact <- rawactDB %>% collect()
cleanedACTtable <- "breath_test_all_cleaned_month"
clactDB <- xap.conn %>% tbl(cleanedACTtable)
clact <- clactDB %>% collect()
nrow(rawact)
nrow(clact)
print("hi!")
# > nrow(rawact)
#  28050845
# nrow(clact)
# 26089466

tname <- "breath_test_all_data_04_feat_v2" #30135282  #"breath_test_all_data_week_feat_v2"
tDB <- xap.conn %>% tbl(tname)
t <- tDB %>% collect()
nrow(t)
patientIds <- unique(t$patientId)

rawACTtable2 <- "breath_test_all_data_with_zeros_month_feat" #"breath_test_all_data_month_feat_v2" -- 5496
rawactDB2 <- xap.conn %>% tbl(rawACTtable2)
rawact2 <- rawactDB2 %>% collect()
nrow(rawact2)


  sDate <- "2019-03-01"
  eDate <- "2019-03-31"

  inputTable <- "pressure_raw_vals"

  st <- Sys.time()
  actDB  <- xap.conn %>% tbl(inputTable)
  en <- Sys.time()
  cat(sprintf("xap.conn in: %s \n", en - st))

  st <- Sys.time()
  dataSlice <- actDB %>%
                filter(between(time, sDate, eDate)) %>%
                collect()
  nrow(dataSlice)                
  
  
  
getPressureDf <- function(conn, sessionIds) {
 query <- "SELECT * from pressure_raw_vals WHERE time >= '2018-09-01' AND time <= '2018-11-30' ORDER BY time"
  df <- dbGetQuery(xap.conn, query)
  nrow(df) # 1] 30135282
}


countPatientsRows <- function(table) {
  p <- "f39d1001-c666-4f91-9e1c-2ac066906a35"
 query <- sprintf("SELECT * FROM breath_test_all_data_03 WHERE patient_record_id = '%s'", p)
  df <- dbGetQuery(xap.conn, query)
  nrow(df) 
}

getUniquePatientsDf <- function(patientCol, table) {
  query <- sprintf("SELECT DISTINCT \"%s\" FROM \"%s\" ", patientCol, table)
  #query <- "SELECT DISTINCT \"patient_record_id\" FROM breath_test_all_data_sept_nov"
  df <- dbGetQuery(xap.conn, query)
  nrow(df) # 
}

getDitinctACTDatesPatientsDf <- function(patientCol, table, optionalCondition = NULL) {
  query <- sprintf("SELECT DISTINCT \"%s\", \"actDate\", \"treatmentId\" FROM \"%s\"
                    WHERE \"treatmentId\" <>'zeroT'
                    GROUP BY \"patientId\", \"actDate\", \"treatmentId\"
                    ", patientCol, table)
  #query <- "SELECT DISTINCT \"patient_record_id\" FROM breath_test_all_data_sept_nov"
  df <- dbGetQuery(xap.conn, query)
  df 
}

getRowCount <- function (table, optionalCondition = NULL) {
    query <- sprintf("SELECT COUNT(*) FROM %s", table)
    if (!is.null(optionalCondition)) 
       query <- paste(query, optionalCondition)
    
    df <- dbGetQuery(xap.conn, query)
    df
}

getRowCount("breath_test_all_data_02_cleaned")
getRowCount("breath_test_all_data_03_cleaned")
getRowCount("breath_test_all_data_02_cleaned", "WHERE time < '2019-03-01'")
getRowCount("breath_test_all_data_0203_feat_v2") # 
getRowCount("breath_test_all_data_1201_feat_v2") # 6051
getRowCount("pressure_raw_vals", "WHERE id = 'f1aa9c61-7951-49e4-9b98-64e41658e001'") # 28080
getRowCount("pressure_raw_vals", "WHERE id = '045e3d65-ab4a-4ccf-bec4-7402196a376f'") # 28080
getRowCount("pressure_raw_vals", "WHERE id = 'bef0ce40-99ae-4952-a356-a927b0a5426c'") # 28080
getRowCount("breath_test_all_data_sept_nov")
getRowCount("breath_test_all_cleaned_sept_nov")


deleteRows <- function(table, optionalCondition = NULL) {
   cat(sprintf("Deleting from %s, condition: %s", table, optionalCondition))
   query <- sprintf("DELETE FROM %s ", table)
   if (!is.null(optionalCondition)) 
       query <- paste(query, optionalCondition)
    
   dbExecute(xap.conn, query)
}

deleteRows("breath_test_all_data_02_cleaned", "WHERE time >= '2019-03-01'")



checkDupSessions <- function() {
     t <- "breath_test_all_data_03_cleaned"
    query <- sprintf("SELECT \"patientId\", \"sessionId\", time, COUNT(*) as count
              FROM %s
              GROUP BY \"patientId\", \"sessionId\", time
              HAVING COUNT(*) > 1", t)
    df <- dbGetQuery(xap.conn, query)
    head(df)

}

checkDupTreatments <- function() {
     t <- "breath_test_all_data_04_feat_v2"
    query <- sprintf("SELECT \"patientId\", \"treatmentId\", \"actDate\", COUNT(*) as count
              FROM %s
              GROUP BY \"patientId\", \"treatmentId\", \"actDate\"
              HAVING COUNT(*) > 1", t)
    df <- dbGetQuery(xap.conn, query)
    head(df)
    unique(df$patientId)

}
checkDupTreatments<- function() {
     t <- "breath_test_all_data_0203_feat_v2"
    query <- sprintf("SELECT \"patientId\", \"treatmentId\", \"actDate\", COUNT(*) as count
              FROM %s
              GROUP BY \"patientId\", \"treatmentId\", \"actDate\"
              HAVING COUNT(*) > 1", t)
    df <- dbGetQuery(xap.conn, query)
    head(df)

}

p <- "05a3a91e-8f08-406f-83fd-6a31af2e3b83"
#tr <- "b91ed670-61e8-4587-9b5b-7ee3571334c3"
#d <- "2019-02-13"
kActFeatures <-  "breath_test_all_data_0203_feat_v2"
query <- sprintf("SELECT * from %s
             WHERE \"patientId\"= '%s'",kActFeatures, p) 
featuresDf <- dbGetQuery(xap.conn, query)

pData <- act %>%  filter(patientId == p) %>%  filter(treatmentId == tr)
str(pData)
 dbWriteTable(xap.conn, "olga_feat_debug", pData,
                     row.names = FALSE, append = TRUE)  
 
p <- "05a3a91e-8f08-406f-83fd-6a31af2e3b83"
#tr <- "b91ed670-61e8-4587-9b5b-7ee3571334c3"
#d <- "2019-02-13"                    
tCleaned <-  "breath_test_all_data_02_cleaned"
query <- sprintf("SELECT * from breath_test_all_data_02_cleaned 
             WHERE \"patientId\"= '%s'", p) 
pDataCl <- dbGetQuery(xap.conn, query)

sessions <- xap.read_table("breath_all_meta")


source("~/scripts/ACT_v2/utils/act_cleaning_utils.R")
#source("~/scripts/ACT_v2/utils/act_labeling_utils_olga.R")
source("~/scripts/ACT_v2/utils/act_labelling_utils.R")
source("~/scripts/ACT_v2/utils/act_featurisation_utils_olga.R")
#source("~/scripts/ACT_v2/utils/act_featurisation_utils.R")
labelledACTData <- LabelACTData(pDataCl, sessions)
#tr <- "cea33673-d59c-4f9f-840b-557d539d0b3a"

#trdata <- labelledACTData %>% filter(treatmentId==tr)
#length(unique(trdata$sessionId))

#dlabelledACTData  <- labelledACTData #%>% filter(lubridate::date(time) == d)
#dtr <-dlabelledACTData %>% filter(treatmentId == tr)
#dtr <-dlabelledACTData #%>% filter(actDate >= "2019-02-06") %>%  filter(actDate <= d)
featurisedACTData <- FeaturiseACTData(labelledACTData)
nrow(featurisedACTData) 
dupsTr <- duplicated(featurisedACTData$treatmentId)
featurisedACTData$treatmentId[dupsTr]
trDups <- "b91ed670-61e8-4587-9b5b-7ee3571334c3"
#trData <- pDataCl %>% filter(treatmentId==tr)
trLabelledACTData <- labelledACTData %>% filter(treatmentId==trDups)
nrow(trLabelledACTData)
unique(trLabelledACTData$sessionId)


# dbWriteTable(xap.conn, "olga_feat_debug_05a3a91e", featurisedACTData,
#                     row.names = FALSE, append = TRUE)  
#unique(featurisedACTData$treatmentId)
trFeaturisedACTData <- FeaturiseACTData(trLabelledACTData)
colselect<- c("actDate", "percentTimeBreathing", "breathCount")
trFeaturisedACTDataSel <- trFeaturisedACTData %>% select(colselect)

cat("done feat")
dfeaturisedACTData <- featurisedACTData  %>% filter(actDate == d)# returns 6, should be 3
dataForTreatmentFeatures  <- featurisedACTData %>%select(
                                      patientId, treatmentId, actDate,
                                      breathCount, pressuresDayCompletenessScore) #arrange(actDate)

devicesCleanTable <- "devices_clean"
linkMoresecureTable <- "linkMoresecure"
devices<- xap.read_table(devicesCleanTable)
linkMoresecData <- xap.read_table(linkMoresecureTable)
devices <- merge(devices,
                          linkMoresecData[, c("patient_record_id", "study_email")],
                          by.x = "study_email",
                          by.y = "study_email")
colnames(devices)[colnames(devices) == "patient_record_id"] <- "patientId"
   
      
featuresAggTreatment <- FeaturiseTreatmentAdherence(dataForTreatmentFeatures, devices, "2019-02-6", d)
features_all <- left_join(featuresAggTreatment, featurisedACTData)
                     #by.x = c("patientId","treatmentId","actDate"),
                      #    by.y = c("patientId","treatmentId","actDate"))               
print(str(features_all))
dfeatures_all <- features_all %>% filter(actDate == d)


kActFeatures <-  "breath_test_all_data_0203_feat_v2"
query <- sprintf("SELECT * from %s",kActFeatures) 
featuresDf <- dbGetQuery(xap.conn, query)
length(featuresDf[featuresDf$treatmentId == "zeroT", ])
length(unique(featuresDf$patientId))

kActFeatures <- "ACTcleanPressuresTable_April"
p <- "d36508d1-fdc2-450e-bb65-0ebd7ac0565f"
query <- sprintf("SELECT * from \"%s\"
             WHERE \"patientId\"= '%s'",kActFeatures, p) 
featuresDf <- dbGetQuery(xap.conn, query)
featuresDf <- featuresDf %>% 
          mutate(actDate = lubridate::date(time))
d <- "2019-04-08"
dfeaturisedACTData <- featuresDf  %>% filter(actDate == d)# returns 6, should be 3


t <- "breath_test_all_cleaned_sept_nov"
getRowCount(t)
getUniquePatientsDf("patientId", t)

t  <- "breath_test_all_cleaned_dec_jan"
getRowCount(t)
getUniquePatientsDf("patientId", t)

t <- "breath_test_all_cleaned_feb_mar"
getRowCount(t)
getUniquePatientsDf("patientId", t)

t <- "breath_test_all_cleaned_apr_may"
getRowCount(t)
getUniquePatientsDf("patientId", t)

t <- 'breath_all_data_feat_sept_may_debug'
getRowCount(t)
getUniquePatientsDf("patientId", t)

t <- "breath_all_data_feat_sept_may"
getRowCount(t)
getUniquePatientsDf("patientId", t)
getDitinctACTDatesPatientsDf("patientId", t)
