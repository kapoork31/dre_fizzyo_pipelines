##################################
# Load libraries and source files
###################################
install.packages("corrplot")
install.packages("DMwR")

library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(DBI)
library(tidyr)
library(corrplot)
library(Hmisc)
library(DMwR)
library(NMF)
library(rlang)

source("~/scripts/2_cluster/utils/outliers_utils.R");
source("~/scripts/2_cluster/utils/clustering_utils.R");
source("~/scripts/2_cluster/utils/dim_reduction_utils.R");
source("~/scripts/2_cluster/utils/clustering_tendency_utils.R");
source("~/scripts/R_scripts/data_cleaning_utils.R");
source("~/scripts/R_scripts/data_distribution_utils.R");
source("~/scripts/bianca_experiments/fitbit_utils.R");
source("~/scripts/2_cluster/utils/validity_metrics_utils.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.R");

# load the latest featurised fitbit dataset
kFitbitFeaturesv4 <- "fitbit_features_v4"

fitbitDB <- xap.conn %>% tbl(kFitbitFeaturesv4)

fitbit <- fitbitDB %>% collect()

dim(fitbit)

# wear percent for Q2 and Q3 (used for clustering filter)
# this is required as this is not yet a feature
fitbit %>%
  mutate(wearQ23Percent = (minsWearQ2 + minsWearQ3)/720 * 100)  -> fitbit

######################################################
# Clustering - check the results look similar to v3
######################################################

clusterExperiment <- function(df, clusterCols, nbClusters = 3){
  # This is the function which performs the cluster and PCA experiments
  #
  # Args:
  #   df: featurised fitbit dataset to perform clustering on
  #   clusterCols: vector of columns to perform PCA and clustering on
  #   nbClusters: number of clusters for Kmeans clustering
  #
  # Returns: a list with the following components
  #       pca: a list with the pca object and pca plots
  #       df: a dataset containing the PCA1, PCA2 and cluster assignments for each user-date
  #         userid <chr> | date <POSIXct> | PCA1 <num> | PCA2 <num> | clusterAssignment <num> |
  
  # convert all NAs and not finite numbers to 0
  df %>%
    mutate_if(is.numeric, funs(if_else(is.na(.), 0, .))) %>%
    mutate_if(is.numeric, funs(if_else(!is.finite(.), 0, .))) -> unscaledDf
  
  # scale each column using z-score transform
  # and then check the distribution
  clusterDf <- unscaledDf %>%
    mutate_if(is.numeric, funs(scale))
  
  clusterDf %>%
    mutate_if(is.numeric, funs(if_else(is.na(.), 0, .)))  -> clusterDf
  
  # run dimensionality reduction of the scaled
  # dataset (both PCA and UMAP) and plot the results
  pca <- PcaRunPlot(clusterDf[clusterCols])
  pca$plot
  
  umapScaled <- UmapRunPlot(clusterDf[clusterCols])
  umapScaled$plot
  
  # extract the PCA1 and PCA2 components for each user and date combinations
  pcaTransformed <- predict(pca$obj, clusterDf[clusterCols])
  clusterDf$PCA1 <- pcaTransformed[, 'PC1']
  clusterDf$PCA2 <- pcaTransformed[, 'PC2']
  
  # run Kmeans clustering
  kClusters <- KMeansClustering(clusterDf[clusterCols], nbClusters = nbClusters)
  
  # get the cluster assignments and plot them
  clusterDf$clusterAssignment <- kClusters$cluster
  
  kpca <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                  as.character(kClusters$cluster), title = "PCA - kmeans")
  kumap <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(kClusters$cluster),
                                   title = "umap - kmeans")
  
  # get the silhoutte values and plots for the clusters
  silMetrics <- SilhouetteValue(kClusters$cluster, clusterDf[clusterCols])
  silPlot <- silMetrics$plot
  
  print(plot_grid(kpca, kumap, silPlot))
  print(paste("CH Index for KMeans is", CHIndex(kClusters$cluster, clusterDf[clusterCols])))
  
  # print cluster centers and unscale them for the numbers to have meaning
  print(unscale(kClusters$centers, scale(unscaledDf[clusterCols])))
  
  # select columns for the function to return
  colsOutput <- c('userid', 'date', 'PCA1', 'PCA2', 'clusterAssignment')
  
  return(list("pca" = pca, "df" = clusterDf[colsOutput]))
}

################################################
# Select columns and data slice for clustering
################################################
stepCols <- c("activeMinsSteps5", "meanHourlyStepsDay", "lowActiveHours", "stepNormModVig")

hrCols <- c("activeMinsHr0", "CoefficientOfVariationHr", "switch100", "switch120", "medianHourly")

# filter the data 
fitbit %>%
  filter(wearQ23Percent >= 80) -> df

df %>%
  filter(stepCountNorm < 1000) -> df

##############################
# steps clustering
#
# Experiment results can be found in OneNote:
#   Clustering -> Fitbit Clustering Plan -> Hackfest - clustering
##############################

# any required data transformations
df$activeMinsSteps5 <- log1p(df$activeMinsSteps5)

# get results and plot
results <- clusterExperiment(df, stepCols, nbClusters = 4)

multiPlot <- PcaDetailedPlot(results$pca$obj)
plot_grid(multiPlot$explainedVariance, multiPlot$contribCircle, multiPlot$cos2Map, multiPlot$contribBar) 

# write results to dataset
outDf <- results$df
outDf$dateClustered <- Sys.time()
colnames(outDf)[colnames(outDf)=="userid"] <- "userId"

# check the cluster number based on PCA graph and cluster center and give them a label
outDf[outDf$clusterAssignment == "Insert cluster number here", ]$clusterAssignment <- 'L'
outDf[outDf$clusterAssignment == "Insert cluster number here", ]$clusterAssignment <- 'S'
outDf[outDf$clusterAssignment == "Insert cluster number here", ]$clusterAssignment <- 'XS'
outDf[outDf$clusterAssignment == "Insert cluster number here", ]$clusterAssignment <- 'M'

xap.db.writeframe(outDf, "Insert dataset name for the steps cluster results")

combined <- outDf[c("userId", "date", "clusterAssignment")]
colnames(combined)[colnames(combined)=="clusterAssignment"] <- "clusterSteps"

#########################################################################
# heartrate clustering
#
# Experiment results can be found in OneNote:
#   Clustering -> Fitbit Clustering Plan -> Hackfest - clustering
########################################################################

# filter the data 
fitbit %>%
  filter(wearQ23Percent >=80) -> df

df %>%
  filter(stepCountNorm < 1000) -> df

# any required data transformations
df$activeMinsHr0 <- log1p(df$activeMinsHr0)
df$switch120 <- log1p(df$switch120)

# get results and plot
results <- clusterExperiment(df, hrCols, nbClusters = 5)

multiPlot <- PcaDetailedPlot(results$pca$obj)
plot_grid(multiPlot$explainedVariance, multiPlot$contribCircle, multiPlot$cos2Map, multiPlot$contribBar) 

# write results to dataset
outDf5 <- results$df
colnames(outDf5)[colnames(outDf5)=="userid"] <- "userId"

# check the cluster number based on PCA graph and cluster center and give them a label
outDf5$clusterHr5 <- outDf5$clusterAssignment
outDf5[outDf5$clusterHr5 == "Insert cluster number here", ]$clusterHr5 <- 'M2'
outDf5[outDf5$clusterHr5 == "Insert cluster number here", ]$clusterHr5 <- 'L'
outDf5[outDf5$clusterHr5 == "Insert cluster number here", ]$clusterHr5 <- 'SM'
outDf5[outDf5$clusterHr5 == "Insert cluster number here", ]$clusterHr5 <- 'S'
outDf5[outDf5$clusterHr5 == "Insert cluster number here", ]$clusterHr5 <- 'M1'


xap.db.writeframe(outDf5, "Insert dataset name for the hr cluster results")

# combine both heartrate and steps clusters into a single file
hr1 <- outDf5[c("userId", "date", "clusterHr5")]
colnames(hr1)[colnames(hr1)=="clusterHr5"] <- "clusterHr"

combined %>% 
  inner_join(hr1, by = c("userId", "date")) %>% 
  select(-PCA1, -PCA2) -> clusterResults

xap.db.writeframe(clusterResults, "fitbit_clusters")

############################################
# Cluster results validations
############################################

# read cluster results
xap.conn %>% tbl("fitbit_clusters") %>% collect() -> clusterResults
colnames(clusterResults)[colnames(clusterResults)=="userId"] <- "userid"

xap.conn %>% tbl("footsteps_clusters") %>% collect() -> stepsCluster
colnames(stepsCluster)[colnames(stepsCluster)=="userId"] <- "userid"

xap.conn %>% tbl("hr_clusters") %>% collect() -> hrCluster
colnames(hrCluster)[colnames(hrCluster)=="userId"] <- "userid"

###############################
# check clusters boundaries
#
# Experiment results can be found in OneNote:
#   Clustering -> Fitbit Clustering Plan -> Cluster Interpretations
###############################

GetClusterBoundariers <- function(fitbit, clusterResults, colName, groupCol){
  # function to get the cluster boundaries (min, max, median and mean)
  
  print(colName)
  
  colName <- rlang::ensym(colName)
  groupCol <- rlang::ensym(groupCol)
  
  clusterResults %>%
    inner_join(fitbit, by = c("userid", "date")) %>% 
    group_by(!!groupCol) %>%
    summarise(minBoundary = min(!!colName, na.rm = TRUE),
              maxBoundary = max(!!colName, na.rm = TRUE),
              medianBoundary = median(!!colName, na.rm = TRUE),
              meanBoundary = mean(!!colName, na.rm = TRUE))
}

# step heartrate boundaries

# 3 clusters
GetClusterBoundariers(fitbit, clusterResults, "activeMinsHr0", "clusterHr3")
GetClusterBoundariers(fitbit, clusterResults, "CoefficientOfVariationHr", "clusterHr3")
GetClusterBoundariers(fitbit, clusterResults, "switch100", "clusterHr3")
GetClusterBoundariers(fitbit, clusterResults, "switch120", "clusterHr3")
GetClusterBoundariers(fitbit, clusterResults, "medianHourly", "clusterHr3")

# 4 cluster
GetClusterBoundariers(fitbit, clusterResults, "activeMinsHr0", "clusterHr4")
GetClusterBoundariers(fitbit, clusterResults, "CoefficientOfVariationHr", "clusterHr4")
GetClusterBoundariers(fitbit, clusterResults, "switch100", "clusterHr4")
GetClusterBoundariers(fitbit, clusterResults, "switch120", "clusterHr4")
GetClusterBoundariers(fitbit, clusterResults, "medianHourly", "clusterHr4")

# 5 cluster
GetClusterBoundariers(fitbit, clusterResults, "activeMinsHr0", "clusterHr5")
GetClusterBoundariers(fitbit, clusterResults, "CoefficientOfVariationHr", "clusterHr5")
GetClusterBoundariers(fitbit, clusterResults, "switch100", "clusterHr5")
GetClusterBoundariers(fitbit, clusterResults, "switch120", "clusterHr5")
GetClusterBoundariers(fitbit, clusterResults, "medianHourly", "clusterHr5")

#####################################################
# Individual days validation
#
# Graphs for the code below can be found in OneNote:
#   Clustering -> Fitbit Clustering Plan -> Cluster Validation
#####################################################

#id <- "4c36b8d7-994e-422d-be79-abab3e5fd4b0"
#id <- ids[1]
id <- "ec651a09-a7b9-4661-8d62-cf3dfc68d4d7"
sdate <- "2019-01-19"
#dates <- as.Date(c("2019-01-19", "2019-01-20", "2019-01-21", "2019-01-22", "2019-01-26"))
#dates <- as.Date(c("2018-10-13", "2018-11-11", "2018-10-16"))
#dates <- as.Date(c("2019-01-23", "2019-01-24", "2019-01-16", "2019-03-16", "2019-03-19"))

# display on steps pca
stepsCluster %>%
  filter(userid == id) %>%
  filter(date == sdate) -> pointVal

ggplot(stepsCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterAssignment)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

sdate <- "2018-10-16"

# display on hr pca
hrCluster %>%
  filter(userid == id) %>%
  filter(date == sdate) -> pointVal

p3 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr3)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p4 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr4)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p5 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr5)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

plot_grid(p3, p4, p5)

# print individual waveforms
fitbit %>%
  filter(userid == id) %>%
  filter(date %in% dates) %>%
  select_at(c("date", hrCols))

kStepsTable <- "foot_steps_granular_vals"
kHRTable <- "heart_rate_vals"

stepsDB <- xap.conn %>% tbl(kStepsTable)
hrDb <- xap.conn %>% tbl(kHRTable)

stepsDB %>% 
  filter(userid == id) %>% 
  filter(Date(time) == Date(sdate)) %>% 
  collect() %>%
  ggplot(aes(x = time, y = value)) + geom_line()

hrDb %>% 
  filter(userid == id) %>% 
  filter(Date(time) == Date(sdate)) %>% 
  collect() %>%
  ggplot(aes(x = time, y = value)) + geom_line()  


#############################################################################
# PCA displays for different subsests
#
# Experiment results can be found in OneNote:
#   Clustering -> Fitbit Clustering Plan -> Cluster validation
############################################################################

# activeMinsSteps20 > 0
stepsCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>%
  filter(activeMinsSteps20 > 0) -> pointVal

dim(pointVal)

ggplot(stepsCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterAssignment)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

# stepCountNorm > 30
stepsCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>% 
  filter(stepCountNorm > 30) -> pointVal

dim(pointVal)

ggplot(stepsCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterAssignment)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

# activeHours > 10
stepsCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>% 
  filter(activeHours > 10) -> pointVal

dim(pointVal)

ggplot(stepsCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterAssignment)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)


# activeMinsHrSteps20 > 0
stepsCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>% 
  filter(activeMinsHrSteps20 > 0) -> pointVal

dim(pointVal)

ggplot(stepsCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterAssignment)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

hrCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>% 
  filter(activeMinsHrSteps20 > 0) -> pointVal

p3 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr3)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p4 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr4)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p5 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr5)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

plot_grid(p3, p4, p5)


# activeMinsHr20 > 0
hrCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>% 
  filter(activeMinsHr20 > 0) -> pointVal

dim(pointVal)

p3 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr3)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p4 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr4)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p5 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr5)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

plot_grid(p3, p4, p5)

# activeMinsHr0 > 60
hrCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>% 
  filter(activeMinsHr0 > 60) -> pointVal

dim(pointVal)

p3 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr3)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p4 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr4)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p5 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr5)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

plot_grid(p3, p4, p5)

# switch140 > 10
hrCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>% 
  filter(switch140 > 10) -> pointVal

dim(pointVal)

p3 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr3)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p4 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr4)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p5 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr5)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

plot_grid(p3, p4, p5)

############################################################################
# Specific PCA conditions - investigation
#
# Experiment results can be found in OneNote:
#   Clustering -> Fitbit Clustering Plan -> Investigate specific users
############################################################################

stepCols <- c("activeMinsSteps5", "meanHourlyStepsDay", "lowActiveHours", "stepNormModVig")

sPCA2 <- min(stepsCluster$PCA2)

stepsCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>%
  filter(PCA2 < -2 & PCA1 < -2) %>% 
  select_at(stepCols)

stepsCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>%
  filter(PCA2 < -2 & PCA1 < -2) -> pointVal

ggplot(stepsCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterAssignment)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

kStepsTable <- "foot_steps_granular_vals"
stepsDB <- xap.conn %>% tbl(kStepsTable)

stepsDB %>% 
  filter(userid == pointVal$userid[1]) %>% 
  filter(Date(time) == Date(pointVal$date[1])) %>% 
  collect() %>%
  ggplot(aes(x = time, y = value)) + geom_line()

#### HR

hrCols <- c("activeMinsHr0", "CoefficientOfVariationHr", "switch100", "switch120", "medianHourly")

sPCA <- max(hrCluster$PCA2)

hrCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>%
  filter(PCA2 == sPCA) %>% 
  select_at(hrCols)

hrCluster %>%
  inner_join(fitbit, by = c("userid", "date")) %>%
  filter(PCA2 == sPCA) -> pointVal


p3 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr3)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p4 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr4)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

p5 <- ggplot(hrCluster, aes(x = PCA1, y = PCA2)) +
  geom_point(aes(color = clusterHr5)) +
  geom_point(data = pointVal, aes(x = PCA1, y = PCA2), shape = 4, size = 4)

plot_grid(p3, p4, p5)

kHRTable <- "heart_rate_vals"
hrDb <- xap.conn %>% tbl(kHRTable)
hrDb %>% 
  filter(userid == pointVal$userid) %>% 
  filter(Date(time) == Date(pointVal$date)) %>% 
  collect() %>%
  ggplot(aes(x = time, y = value)) + geom_line()  

######################################################
# Manual clustering
#
# Experiment results can be found in OneNote:
#   Clustering -> Fitbit Clustering Plan -> Manual clustering
######################################################

fitbit$hrCluster <- NA
fitbit[fitbit$activeMinsHr0 >= 30, ]$hrCluster <- "medium"
fitbit[fitbit$activeMinsHr0 >= 60, ]$hrCluster <- "large"
fitbit[fitbit$activeMinsHr0 < 30, ]$hrCluster <- "small"

fitbit %>%
  group_by(hrCluster) %>%
  count()

fitbit$swith120Cluster <- NA
fitbit[fitbit$switch120 >= 10, ]$swith120Cluster <- "medium"
fitbit[fitbit$switch120 >= 40, ]$swith120Cluster <- "large"
fitbit[fitbit$switch120 < 10, ]$swith120Cluster <- "small" 

fitbit %>%
  group_by(swith120Cluster) %>%
  count() 

fitbit$stepCountCluster <- NA
fitbit[fitbit$stepCount >= 5000, ]$stepCountCluster <- "medium"
fitbit[fitbit$stepCount >= 10000, ]$stepCountCluster <- "large"
fitbit[fitbit$stepCount < 5000, ]$stepCountCluster <- "small"     

fitbit %>%
  group_by(stepCountCluster) %>%
  count()   

fitbit$stepModVigCluster <- NA
fitbit[fitbit$stepNormModVig >= 50, ]$stepModVigCluster <- "medium"
fitbit[fitbit$stepNormModVig >= 70, ]$stepModVigCluster <- "large"
fitbit[fitbit$stepNormModVig < 50, ]$stepModVigCluster <- "small"     

fitbit %>%
  group_by(stepCountCluster) %>%
  count()
