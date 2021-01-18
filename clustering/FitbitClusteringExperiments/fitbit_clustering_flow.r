################################################################################
#
# This script details for clustering flow for fitbit clustering for both
# footsteps and heartrate
#
################################################################################

source("~/scripts/2_cluster/clustering_flow.R") # <- one source that sources all the packages and all the utils

######################################################################
# Load the featurised fitbit data and do any required transformations
#####################################################################


kFitbitFeaturesv4 <- "fitbit_features_v4"

fitbitDB <- xap.conn %>% tbl(kFitbitFeaturesv4)

fitbit <- fitbitDB %>% collect()

dim(fitbit)

# wear percent for Q2 and Q3 (used for clustering filter)
# this is required as this is not yet a feature
fitbit %>%
  mutate(wearQ23Percent = (minsWearQ2 + minsWearQ3)/720 * 100)  -> fitbit


# specify features to cluster on for footsteps
stepCols <- c("activeMinsSteps5", "meanHourlyStepsDay", "lowActiveHours", "stepNormModVig", "userid", "date")

# specify features to cluster on for heartrate
hrCols <- c("activeMinsHr0", "CoefficientOfVariationHr", "switch100", "switch120", "medianHourly", "userid", "date")


# define the function which condenses all the experiments
ClusterWithViz <- function(data, 
                           clusterCols, # <- make sure id and date is among them
                           removeOutliers = T, 
                           doTendency=T, 
                           doKMeans = T, 
                           doHC = T,
                           nbClusters = 3,
                           out.thr = 0.03,
                           inpute.mean = T,
                           filters = NULL,
                           save = T){  
    ###################################################################################################
    # Load the data and select the clustering columns
    ###################################################################################################
    print("********** Selecting clustering columns...")
    clusterDf <- data %>% dplyr::select(clusterCols)
    print(str(clusterDf))
    
    ###################################################################################################
    # Data distribution and feature correlation analysis
    ###################################################################################################
    print("********** Running data distribution analysis...")
    CheckDatasetDistribution(clusterDf)  
    
    print("********** Running correlation analysis...")
    FeatureCorrelationAnalysis(clusterDf, title="Input data")
    
    ###################################################################################################
    # Remove outliers
    ###################################################################################################
    if (removeOutliers) {
        print("********** Removing outliers...")
        clusterDf <- RemoveOutliers(clusterDf, thr = out.thr, verbose=TRUE, visualize=TRUE)
        
        print("********** Re-running data distribution analysis...")
        CheckDatasetDistribution(clusterDf)
        print("********** Re-running correlation analysis...")
        FeatureCorrelationAnalysis(clusterDf, title="Input data after outliers")
        
    } else {
        print("********** Skipping outlier removal...")
    }    
    
    ###################################################################################################
    # Z-score scaling
    ###################################################################################################
    print("********** Impute missing values and standardize the features...")
    res <- ImputeScaleData(clusterDf, inpute.mean)
    scaledDf <- res$scaledDf
    unscaledNumDf <- res$unscaledNumDf
    
    ###################################################################################################
    # Clustering tendency
    ###################################################################################################
    if (doTendency){
        print("********** Assessing clustering tendency...")
        dissPlot <- DissimilarityPlot(scaledDg, nmax=250)
        print(dissPlot)
    } else {
        print("********** Skipping assessing clustering tendency")
    }
    
    ###################################################################################################
    # Dimensionality reduction and PCA visuals
    ###################################################################################################
    print("********** Performing dimensionality reduction...")
    # Run dimensionality reduction (PCA & UMAP) of the scaled dataset
    pcaRes <- PcaRunPlot(scaledDf)
    umapRes <- UmapRunPlot(scaledDf)
    print(plot_grid(pcaRes$plot, umapRes$plot))
    
    print("********** Plotting PCA details...")
    multiPlot <- PcaDetailedPlot(pcaRes$obj)
    print(plot_grid(multiPlot$explainedVariance, multiPlot$cos2Circle, multiPlot$cos2Map, multiPlot$cos2Bar)) 
    print(plot_grid(multiPlot$explainedVariance, multiPlot$contribCircle, multiPlot$contribMap, multiPlot$contribBar))
    
    ###################################################################################################
    # KMeans  Clustering
    ###################################################################################################
    if (doKMeans) {
        print('********** Performing KMeans Clustering...')
        km <- KMeansClustering(scaledDf, nbClusters)
        kmClusters <- km$cluster
        
        RunExperimentMetrics(kmClusters, scaledDf, unscaledNumDf)
        RunExperimentViz(kmClusters, pcaRes, umapRes, filters)
        
        if (save) {
            SaveClusteringResults(kmClusters, pcaRes, clusterDf, title="km_exp", type = "act")
        }
    } else {
        print("********** Skipping KMeans Clustering...")
    }
    
    ###################################################################################################
    # Hierarchical clustering
    ###################################################################################################
    if (doHC) {
        print('********** Performing Hierarchical Clustering...')
        hc <- HierarchicalClustering(scaledDf, method = "ward.D")
        plot(hc)
        hcClusters <- cutree(hc, k = 3)
       
        RunExperimentMetrics(kmCluster, scaledDf, unscaledNumDf)
        RunExperimentViz(kmCluster, pcaRes, umapRes, filters) 
        
        if (save) {
            SaveClusteringResults(hcClusters, pcaRes, clusterDf, title="hc_exp", type = "act")
        }
    
    } else {
        print('********* Skipping Hierarchical Clustering ...')
    }
    
    #####################################################################################################
    # Manual Clustering
    #####################################################################################################
    # TODO
    if (F) {
        mClusters <- ManualClustering()
        RunExperimentMetrics(mClusters, scaledDf, unscaledNumDf)
        RunExperimentViz(mClusters, pcaRes, umapRes, filters)  
    }
    
}

#############################################################
# Experiments
#############################################################

# filter the data 
fitbit %>%
  filter(wearQ23Percent >= 80) -> df

# remove very obvious outliers
df %>%
  filter(stepCountNorm < 1000) -> df


# required data transformations
## steps
df$activeMinsSteps5 <- log1p(df$activeMinsSteps5)
## heartrate
df$activeMinsHr0 <- log1p(df$activeMinsHr0)
df$switch120 <- log1p(df$switch120)

## Steps clustering
filters <- c("userid == 'ec651a09-a7b9-4661-8d62-cf3dfc68d4d7'" )
        #    "activeMinsSteps20 > 0",
         #   "stepCountNorm > 30",
        #    "activeHours > 10",
         #   "activeMinsHrSteps20 > 0",
           # "PCA2 < -2 & PCA1 < -2")

ClusterWithViz(df, stepCols, removeOutliers = F, inpute.mean = F, doTendency = F, doKMeans = T, doHC = F, nbClusters = 4, filters = filters, save = F)

## Heartrate clustering
filters <- c("userid == 'ec651a09-a7b9-4661-8d62-cf3dfc68d4d7'")
           # "activeMinsHr20 > 0",
           # "activeMinsHr0 > 60",
           # "switch140 > 10",
           # "activeMinsHrSteps20 > 0")

ClusterWithViz(df, hrCols, removeOutliers = F, inpute.mean = F, doTendency = F, doKMeans = T, doHC = F, nbClusters = 5, filters = filters, save = F)

























