##################################
# Load libraries and source files
###################################
install.packages("corrplot")
install.packages("DMwR")
install.packages("NMF")

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
source("~/scripts/R_scripts/data_cleaning_utils.R");
source("~/scripts/R_scripts/data_distribution_utils.R");
source("~/scripts/bianca_experiments/fitbit_utils.R");
source("~/scripts/2_cluster/utils/validity_metrics_utils.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.R");

# load the data
kFitbitFeatures <- "mvpa_count_prev_15"

fitbitDB <- xap.conn %>% tbl(kFitbitFeatures)

fitbit <- fitbitDB %>% collect()

dim(fitbit)

fitbit %>%
    filter(wearQ234Percent >= 80) -> df

# columns of interest
stepCols <- c("activeHours", "activeMinsSteps5", "CoefficientOfVariationStep",
                    "lowActiveHours", "meanHourlyStepsDay",
                    "stepNormModVig", "stepNormNonModerate")

hrCols <- c("Neighbour15Rolling14", "CoefficientOfVariationHr", "restingHrProxy")
                    
clusterExperiment <- function(df, clusterCols){
    df %>%
        mutate_if(is.numeric, funs(if_else(is.na(.), 0, .))) %>%
        mutate_if(is.numeric, funs(if_else(!is.finite(.), 0, .))) -> unscaledDf
    
    # scale each column using z-score transform
    # and then check the distribution
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
    kumap <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(kClusters$cluster),
    #                                  title = "umap - kmeans")
    
    silMetrics <- SilhouetteValue(kClusters$cluster, clusterDf[clusterCols])
    silPlot <- silMetrics$plot
    
    print(plot_grid(kpca, kumap, silPlot))
    print(paste("CH Index for KMeans is", CHIndex(kClusters$cluster, clusterDf[clusterCols])))
    
    # print cluster centers and unscale them for the numbers to have meaning
    print(unscale(kClusters$centers, scale(unscaledDf[clusterCols])))
    
    pcaCenters <- predict(pca$obj, kClusters$centers) 
    pcaCenters <- data.frame(pcaCenters)
    pcaCenters %>% mutate(clusterAssignment = seq(1, k), 
                          userid = 'centroid',
                          date = NA,
                          PCA1 = PC1,
                          PCA2 = PC2) -> pcaCenters
    
    colsOutput <- c('userid', 'date', 'PCA1', 'PCA2', 'clusterAssignment')
    
    return(list("pca" = pca, "df" = rbind(clusterDf[colsOutput], pcaCenters[colsOutput])))
}

# run clustering for steps
                    
results <- clusterExperiment(df, stepCols)

outDf <- results$df
outDf$dateClustered <- Sys.time()
colnames(outDf)[colnames(outDf)=="userid"] <- "userId"

xap.db.writeframe(outDf, "footsteps_kmeans_exp")

# run clustering for heartrate

results <- clusterExperiment(df, hrCols)

outDf <- results$df
outDf$dateClustered <- Sys.time()
colnames(outDf)[colnames(outDf)=="userid"] <- "userId"

#xap.db.writeframe(outDf, "heartrate_kmeans_exp")






