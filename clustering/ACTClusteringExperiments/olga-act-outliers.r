install.packages("corrplot")
install.packages("DMwR")

library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(DBI)
library(tidyr)
library(corrplot)
library(DMwR)

library(DBI)
library(ggplot2)
library(scales)
library(lubridate)
library(stringr)

source("~/scripts/2_cluster/utils/clustering_utils.R");
source("~/scripts/2_cluster/utils/validity_metrics_utils.R");
source("~/scripts/ACT_v2/utils/act_cleaning_utils.R");
#source("~/scripts/2_cluster/utils/dim_reduction_utils.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.r");
source("~/scripts/R_scripts/data_cleaning_utils.R");

plotBreathsWithPoints <- function(df, title = "") {
  print(ggplot(df) + 
    geom_line(aes(x= seq(length(pressurevalues)), y = pressurevalues)) + 
    labs(title = title) + 
    xlab("measurement"))
}

getPressureDf <- function(conn, sessionIds) {
  query <- sprintf("SELECT * from breath_test_all_data_month WHERE id IN ('%s') ORDER BY time", str_c(sessionIds, collapse = "','"))
  df <- dbGetQuery(conn, query)
  df %>% 
    mutate(
        pressurevalues = as.numeric(pressurevalues),
        time = lubridate::ymd_hms(time)
    )
}

kActFeatures <-  "breath_test_all_data_month_feat" #"breath_test_all_data_with_zeros_month_feat"

# Columns for clustering
clusterCols <- c("patient_record_id", "breathsDayAdherenceScore", "treatmentWeekAdherenceScore" , "meanBreathAmplitude", "breathCount")

actDB <- xap.conn %>% tbl(kActFeatures)
act <- actDB %>% collect()

# Remove identifiers to cluster all days (leave patient_record_id)
# Replace NAs by 0
cleanDF  <- act %>%
  #select(-patientId,  -actDate, -treatmentId) %>%
  select(-patientId,  -actDate) %>%
  replace(., is.na(.), 0) 

clusterDF <- cleanDF

#clusterDF <-  clusterDF %>% select(clusterCols)
#head(clusterDF)
#dim(clusterDF)
str(clusterDF)
nrow(clusterDF)

###################################################################################################
# Outliers
###################################################################################################
visualize = TRUE
dfOutliers <- clusterDF
   #DetectOutliers(dfOutliers, breathsDayAdherenceScore, "breathsDayAdherenceScore", visualize = visualize)
   #DetectOutliers(dfOutliers, treatmentWeekAdherenceScore, "treatmentWeekAdherenceScore", visualize = visualize)
   DetectOutliers(dfOutliers, meanBreathAmplitude, "meanBreathAmplitude", visualize = visualize)
   #DetectOutliers(dfOutliers, breathCount, "breathCount", visualize = visualize)

t <- dfOutliers[dfOutliers$"meanBreathAmplitude" >60, ]
patient_ids <- unique(t$patient_record_id)
nrow(t)
id <- 3
p <- patient_ids[id]
tr <- t[t$patient_record_id == p, ]
tr$treatmentId
id_t <- 2
currTreatId <- tr$treatmentId[id_t]


sessions <- xap.read_table("session_metadata")
str(sessions)
df_sessIds <- sessions[sessions$treatmentId %in% currTreatId, ]
kConn <- xap.conn
dfPress <- getPressureDf(kConn, df_sessIds$sessionId)

#nrow(dfPress)
#str(dfPress)
dfPress <- dfPress[order(dfPress$time), ]
#str(dfPress)

plotBreathsWithPoints(dfPress, sprintf("raw p= %s t= %s", p, currTreatId))
n_normal <- length(unique(clusterDF[clusterDF$patient_record_id == p,]$treatmentId))
cat(sprintf("N of all treatments = %s \n", n_normal))
cat(sprintf("N of outlier treatments = %s \n", length(tr$treatmentId)))

ok_treatments <- clusterDF[(clusterDF$patient_record_id == p) & !(clusterDF$treatmentId %in% tr$treatmentId) ,]

cleanedDfPres <- CleanRawACTData(dfPress)
plotBreathsWithPoints(cleanedDfPres$pressureDetrend, sprintf("p= %s t= %s", p, currTreatId))

cleanedDfPres_sorted <- cleanedDfPres[order(cleanedDfPres$time), ]
plotBreathsWithPoints(cleanedDfPres_sorted, sprintf("p= %s t= %s", p, currTreatId))