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
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.R");
source("~/scripts/R_scripts/data_cleaning_utils.R");
source("~/scripts/R_scripts/data_distribution_utils.R");
source("~/scripts/bianca_experiments/fitbit_utils.R");
source("~/scripts/2_cluster/utils/validity_metrics_utils.R");

################################################################
## Load the data
################################################################
kFitbitFeaturesv3 <- "fitbit_features_v3"

fitbitDB <- xap.conn %>% tbl(kFitbitFeaturesv3)

fitbit <- fitbitDB %>% collect()

dim(fitbit)

stepsCols <-   c("activeHours", "activeMinsSteps5",
                    "CoefficientOfVariationStep", "CoefficientOfVariationStepQ1", 
                    "CoefficientOfVariationStepQ2", "CoefficientOfVariationStepQ3",
                    "lowActiveHours", "meanHourlyStepsDay", "stepCountNorm",
                    "stepCountNormQ2", "stepCountNormQ3", "stepCountNormQ4", 
                    "stepNormModerate", "stepNormVigorous")

#################################################################
## Dimensionality reduction and feature selection
#################################################################

featureSelection <- function(df, stepsCols, numFeat = 8){
    
    stepsDf <- df[stepsCols]

    stepsDf %>%
        mutate_all(funs(if_else(is.na(.), 0, .))) %>%
        mutate_all(funs(if_else(!is.finite(.), 0, .))) -> unscaledStepsDf
    
    # scale each column using z-score transform
    # and then check the distribution
    stepsDf <- unscaledStepsDf %>%
                mutate_if(is.numeric, funs(scale))
    
    # run dimensionality reduction of the scaled
    # dataset (both PCA and UMAP) and plot the results
    pca <- PcaRunPlot(stepsDf)
    multiPlot <- PcaDetailedPlot(pca$obj)
    
    # compute the variable contributions
    pcaVar <- get_pca_var(pca$obj)
    relevantMat <- pcaVar$contrib[,1:2]
    
    # add up the contributions to the first two principal components
    relevantMat <- cbind(relevantMat, totalContrib=rowSums(relevantMat))
    relevantMat <- relevantMat[order(relevantMat[,3], decreasing=TRUE),]
    
    # select the most relevant features
    relevantFeatures <- row.names(relevantMat)[1:numFeat]
    return(list("relevantFeatures" = relevantFeatures,
                "plot" = multiPlot ))
}

featSelRes <- featureSelection(fitbit, stepsCols)

# columns of interest
clusterCols <- featSelRes$relevantFeatures
pcaPlots <- featSelRes$plot
pcaPlots

##########################################################################
## Partition Consistency Experiments
#########################################################################

consistencyExperiment <- function(df, clusterCols){

    clusterDf <- df[clusterCols]

    clusterDf %>%
        mutate_all(funs(if_else(is.na(.), 0, .))) %>%
        mutate_all(funs(if_else(!is.finite(.), 0, .))) -> unscaledClusterDf
    
    # scale each column using z-score transform
    # and then check the distribution
    clusterDf <- unscaledClusterDf %>%
                mutate_if(is.numeric, funs(scale))
    
    # run dimensionality reduction of the scaled
    # dataset (both PCA and UMAP) and plot the results
    pca <- PcaRunPlot(clusterDf)
    pca$plot
    
    umapScaled <- UmapRunPlot(clusterDf)
    umapScaled$plot
    
    kClusters <- KMeansClustering(clusterDf, nbClusters = 3)
    
    kpca <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                    as.character(kClusters$cluster), title = "PCA - kmeans", include.chull=TRUE)
    kumap <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(kClusters$cluster),
                                     title = "umap - kmeans")
    
    silMetrics <- SilhouetteValue(kClusters$cluster, clusterDf)
    silPlot <- silMetrics$plot
    
    print(plot_grid(kpca, kumap, silPlot))
    print(paste("CH Index for KMeans is", CHIndex(kClusters$cluster, clusterDf)))
    
    # print cluster centers and unscale them for the numbers to have meaning
    unscale(kClusters$centers, scale(unscaledClusterDf))
}

#############################################################
# Filter 1 : avg(quadrants) > 40
#############################################################
fitbit %>%
    mutate(wp = rowMeans(cbind(wearQ2Percent, wearQ3Percent, wearQ4Percent))) %>%
    filter(wp >= 40) -> df
                    
consistencyExperiment(df, clusterCols)

#############################################################
# Filter 2 : wearPercent > 80
#############################################################

fitbit %>%
    filter(wearPercent >= 80) -> df
                    
consistencyExperiment(df, clusterCols)

# #############################################################
# # Filter 3 : All data
# #############################################################

# df <- fitbit

# clusterExperiment(df, clusterCols)

# #############################################################
# # Filter 4 : wearPercent >= 50
# #############################################################

# fitbit %>%
#     filter(wearPercent >= 50) -> df

# dim(df)
                    
# clusterExperiment(df, clusterCols)

# #############################################################
# # Filter 5 : wearPercent < 50
# #############################################################

# fitbit %>%
#     filter(wearPercent < 50) -> df

# dim(df)
                    
# clusterExperiment(df, clusterCols)
    