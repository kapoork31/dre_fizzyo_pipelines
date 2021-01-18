source("~/scripts/ACT_v2/utils/act_cleaning_utils.R")
source("~/scripts/ACT_v2/utils/act_labelling_utils.R")
#source("~/scripts/ACT_v2/utils/act_featurisation_utils.R")
#source("~/scripts/ACT_v2/utils/act_featurisation_utils_steph.R")
source("~/scripts/ACT_v2/utils/act_featurisation_utils_olga.R")

library(DBI)

kConn <- xap.conn
p <- "05a3a91e-8f08-406f-83fd-6a31af2e3b83"
#tr <- "b91ed670-61e8-4587-9b5b-7ee3571334c3"
#d <- "2019-02-13"                    
tCleaned <-  "breath_test_all_data_02_cleaned"
query <- sprintf("SELECT * from breath_test_all_data_02_cleaned 
             WHERE \"patientId\"= '%s'", p) 
pDataCl <- dbGetQuery(xap.conn, query)

sessions <- xap.read_table("breath_all_meta")


labelledACTData <- LabelACTData(pDataCl, sessions)

session1 <- labelledACTData %>% filter(sessionId == 'cea33673-d59c-4f9f-840b-557d539d0b3a')
session2 <- labelledACTData %>% filter(sessionId == 'c9295e47-a40d-476f-9b3c-4db335070f3e')

plotBreaths <- function(data) {
  ggplot(data) +
    geom_line(aes(x = seq(1, nrow(data)), y = pressureDetrend, color = factor(breathId)))
}


# Check number of breaths in each session and see that it matches the number of breaths in treatment
kCleanTableName <- "breath_test_all_data_02_cleaned"
kSessionMetaTableName <- "session_metadata_jul"
sessionMetaDf <- dbGetQuery(xap.conn, sprintf("SELECT * FROM %s", kSessionMetaTableName))
treatmentId <- "b91ed670-61e8-4587-9b5b-7ee3571334c3"
sessionsInTreatment <- dbGetQuery(xap.conn, sprintf("SELECT \"sessionId\" FROM %s WHERE \"treatmentId\" = '%s'", sessionMetaDf, treatmentId))

# SESSION 1
kSessionId <- "cea33673-d59c-4f9f-840b-557d539d0b3a"
cleanedDf <- dbGetQuery(kConn, sprintf("SELECT * FROM %s WHERE \"sessionId\" = '%s'", kCleanTableName, kSessionId))
labelledDf <- LabelACTData(cleanedDf, sessionMetaDf)
featurisedDf <- FeaturiseACTData(labelledDf)
plotBreaths(labelledDf) # Looks like 38 breaths, but it's lower bc the breath ids skip sometimes when we have multiple peaks
# breathCount in featurisedDf = 31
print(sum(unique(labelledDf$breathId) != 0, na.rm = TRUE))

# SESSION 2
kSessionId <- "c9295e47-a40d-476f-9b3c-4db335070f3e"
cleanedDf <- dbGetQuery(kConn, sprintf("SELECT * FROM %s WHERE \"sessionId\" = '%s'", kCleanTableName, kSessionId))
labelledDf <- LabelACTData(cleanedDf, sessionMetaDf)
featurisedDf <- FeaturiseACTData(labelledDf)
plotBreaths(labelledDf)
# breathCount in featurisedDf = 4
print(sum(unique(labelledDf$breathId) != 0, na.rm = TRUE))

# TREATMENT
kTreatmentId <- "b91ed670-61e8-4587-9b5b-7ee3571334c3"
kSessionMetaTableName <- "breath_all_meta"
sessionsInTreatment <- dbGetQuery(kConn, sprintf("SELECT * FROM %s WHERE \"treatmentId\" = '%s'", kSessionMetaTableName, kTreatmentId))$sessionId
sessionsInTreatment

##############################################################################################
# Validate data for a specific patient:
source("~/scripts/ACT_v2/utils/act_cleaning_utils.R")
source("~/scripts/ACT_v2/utils/act_labelling_utils.R")
source("~/scripts/ACT_v2/utils/act_featurisation_utils_olga.R")

plotBreaths <- function(data) {
  ggplot(data) +
    geom_line(aes(x = seq(1, nrow(data)), y = pressureDetrend, color = factor(breathId)))
}

kConn <- xap.conn
patientId <- "f39d1001-c666-4f91-9e1c-2ac066906a35" # 05a3a91e-8f08-406f-83fd-6a31af2e3b83

kTreatmentId <- "b022bbe6-55ea-4af8-8a65-abd354fb11ff"
kSessionMetaTableName <- "session_metadata_jul"
sessionMetaDf <- dbGetQuery(kConn, sprintf("SELECT * FROM %s", kSessionMetaTableName))
cleanedDf <- dbGetQuery(kConn, sprintf("SELECT * from breath_test_all_data_02_cleaned WHERE \"patientId\"= '%s'", patientId))
labelledDf <- LabelACTData(cleanedDf, sessionMetaDf)
treatment <- labelledDf %>% filter(treatmentId == kTreatmentId)

# Check that # breaths in treatment = sum of breaths of all sessions in the treatment:
print(sprintf("%s breaths in treatment", sum(unique(treatment$breathId) != 0, na.rm = TRUE))) # treatment has 178 breaths
plotBreaths(treatment) # plot of a treatment for this patient
sessionsInTreatment <- dbGetQuery(kConn, sprintf("SELECT * FROM %s WHERE \"treatmentId\" = '%s'", kSessionMetaTableName, kTreatmentId))$sessionId
totalBreaths <- 0
for (sessId in sessionsInTreatment) {
  session <- labelledDf %>% filter(sessionId == sessId)
  print(plotBreaths(session))
  numBreathsInSession <- sum(unique(session$breathId) != 0, na.rm = TRUE)
  totalBreaths = totalBreaths + numBreathsInSession
}
print(sprintf("%s breaths summed over all sessions", totalBreaths))

# Check that the number of rows in the feature DF = the number of unique treatments:
featurisedDf <- FeaturiseACTData(labelledDf)
#featurisation
print(nrow(featurisedDf)) # 22 rows
print(nrow(unique(featurisedDf["treatmentId"]))) # 22 treatments
print(head(featurisedDf))

# Check that set count seems reasonable for a treatment:
print(unique(treatment["setId"])) # no sets for this treatment were identified! SEE HERE

# Check that breath features seems reasonable:
featuresForTreatment <- featurisedDf %>% filter(treatmentId == kTreatmentId)
breathColNames <- colnames(featuresForTreatment[4:length(featuresForTreatment)])

for (breathColName in breathColNames) {
  print(featuresForTreatment[breathColName])
}

##############################################################################################


# Find number of treatments that run over a day
meta <- dbGetQuery(kConn, sprintf("SELECT * from session_metadata"))
longTreatments <- meta %>%
  group_by(patientId, treatmentId) %>%
  arrange(sessionStart) %>% # btw this still has 1970
  filter(max(sessionEnd) - min(sessionStart) > days(1))