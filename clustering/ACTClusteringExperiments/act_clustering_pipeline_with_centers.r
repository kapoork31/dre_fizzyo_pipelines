
# Script to cluster and visualise clustering of ACT data

################################################################################
#
# This script has the experimental flow for ACT data (meant to be run inside DRE). 
# It has the functionality to:
# - viz correlations
# - dim reduction (pca, umap and tsne)
# - analyze the outliers
# - scale
# - clustering tendency 
# - clustering (k-means, heirarchical) & viz clustering 
# - plot individual patients points onto the overall "cloud" of data.
# - write the cluster assignment to table for the viz app
################################################################################

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
library(Hmisc)

source("~/scripts/2_cluster/utils/clustering_utils.R");
source("~/scripts/2_cluster/utils/validity_metrics_utils.R");
source("~/scripts/2_cluster/utils/outliers_utils_tempest.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.r");
source("~/scripts/R_scripts/data_cleaning_utils.R");
source("~/scripts/R_scripts/data_distribution_utils.R");
source("~/scripts/2_cluster/utils/clustering_tendency_utils.R");

#name of single table
kActTableName <- "breath_all_data_feat_sept_may"
#"ACTfeatureTable_Feb-April_nicole1"



#names of multiples tables that need to be joined
kActFeatures1 <- "breath_test_all_data_1201_feat_v2"
kActFeatures2 <- "breath_test_all_data_0203_feat_v2"
kActFeatures3 <-  "breath_test_all_data_04_feat_v2"

kActTableNames = c(kActFeatures1, kActFeatures2, kActFeatures3)

# Optionally list all feature names: 
# List all columns (features) in table:
all_feature_names <- xap.conn %>% tbl(kActTableName) %>% colnames()
print(all_feature_names)

# specify features to cluster:
kFeatureNames <-  c("meanBreathDuration" , "meanBreathAmplitude", "breathCount", "breathRollWeekAdherenceScore")


###################################################################################################
# Load the featurised data from a single table
###################################################################################################

actDB <- xap.conn %>% tbl(kActTableName)
act <- actDB %>% collect()

###################################################################################################
# Load the featurised data from multiple tables
###################################################################################################


print("********** Loading data...")

act = NULL

for (t in kActTableNames) {
   
    actDB <- xap.conn %>% tbl(t)
    act_t <- actDB %>% collect() 
    act = rbind(act, act_t)
}


###################################################################################################
# Exclude certain data from clustering analysis
###################################################################################################

print("********** Cleaning data...")

# Cleaning
cleanDF  <- act %T>% {print(paste("Original ACT dims : ",(dim(.))))} %>%
  filter(treatmentId != "zeroT") %T>% {print(paste("Dims after removing zeroT : ",(dim(.))))} %>% # filter out non-treatments/empty calender days
  filter(pressuresDayCompletenessScore == 1) %T>% {print(paste("Dims after removing treatments<15  : ",(dim(.))))} %>%  # only keep completed treatments (>15s)
  filter(breathCount > 0) %T>% {print(paste("Dims after removing breathCount=0 : ",(dim(.))))} %>%  #filter out the treatments with zero breaths
  replace(., is.na(.), 0) # Replace NAs by 0, for now
  # Note: if breathCount=0, the breathAmplitude will be NA, so it should be replaced by zero


#cleanDF <- cleanDF %>% mutate(treatmentDuration = treatmentDuration/60)

clusterDF <-  cleanDF  %>% select(kFeatureNames) 

str(clusterDF)
print(paste("Number of rows in clusterDF: ", nrow(clusterDF)))

####################################################################################################
####################################################################################################
clusterExperiment <- function(df, clusterCols){
    # df %>%
    #     mutate_if(is.numeric, funs(if_else(is.na(.), 0, .))) %>%
    #     mutate_if(is.numeric, funs(if_else(!is.finite(.), 0, .))) -> unscaledDf
    
    # scale each column using z-score transform
    # and then check the distribution
    df -> unscaledDf
    clusterDf <- unscaledDf %>%
                mutate_if(is.numeric, funs(scale))
    
    # run dimensionality reduction of the scaled
    # dataset (both PCA and UMAP) and plot the results
    pca <- PcaRunPlot(clusterDf[clusterCols])
    pca$plot
    
    # umapScaled <- UmapRunPlot(clusterDf[clusterCols])
    # umapScaled$plot
    
    pcaTransformed <- predict(pca$obj, clusterDf[clusterCols])
    clusterDf$PCA1 <- pcaTransformed[, 'PC1']
    clusterDf$PCA2 <- pcaTransformed[, 'PC2']
    
    k <- 3
    kClusters <- KMeansClustering(clusterDf[clusterCols], nbClusters = k)
    #kClusters <- KMeansClustering(clusterDf, runs = TRUE, runsMaxRange = 5)
    
    clusterDf$clusterAssignment <- kClusters$cluster
    
    kpca <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                    as.character(kClusters$cluster), title = "PCA - kmeans")
    # kumap <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(kClusters$cluster),
    #                                  title = "umap - kmeans")
    
    # silMetrics <- SilhouetteValue(kClusters$cluster, clusterDf[clusterCols])
    # silPlot <- silMetrics$plot
    
    # print(plot_grid(kpca, kumap, silPlot))
    # print(paste("CH Index for KMeans is", CHIndex(kClusters$cluster, clusterDf[clusterCols])))
    
    # print cluster centers and unscale them for the numbers to have meaning
    featureCenters <- data.frame(unscale(kClusters$centers, scale(unscaledDf[clusterCols])))
    featureCenters %>% mutate(clusterAssignment = seq(1, k)) -> featureCenters
    print(featureCenters)
    
    pcaCenters <- predict(pca$obj, kClusters$centers) 
    pcaCenters <- data.frame(pcaCenters)
    pcaCenters %>% mutate(clusterAssignment = seq(1, k), 
                          patientId = 'centroid',
                          actDate = NA,
                          isCentroid = TRUE,
                          PCA1 = PC1,
                          PCA2 = PC2) -> pcaCenters
    
    colsOutput <- c('patientId', 'actDate', 'PCA1', 'PCA2', 'clusterAssignment')
    
    return(list("pca" = pca, "df" = rbind(clusterDf[colsOutput], pcaCenters[colsOutput]), "centroids"  = featureCenters))
}

# run clustering for steps
                    
results <- clusterExperiment(cleanDF, kFeatureNames)
outDf <- results$df
centroidsDf <- results$centroids

timeNow <- Sys.time()
outDf$dateClustered <- timeNow
centroidsDf$dateClustered <- timeNow

colnames(outDf)[colnames(outDf)=="actDate"] <- "date"
xap.db.writeframe(outDf, "act_kmeans_exp")
xap.db.writeframe(centroidsDf, "act_kmeans_centroids")


 
#xap.db.writeframe(actKmeansCluster, "act_kmeans")
