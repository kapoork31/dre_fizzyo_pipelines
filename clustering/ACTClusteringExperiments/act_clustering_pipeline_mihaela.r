# Script to cluster and visualise clustering of ACT data

################################################################################
#
# This script has the experimental flow for ACT data (meant to be run inside DRE). 
# It has the functionality to:
# - viz correlations
# - dim reduction (pca, umap)
# - analyze the outliers
# - scale
# - clustering tendency 
# - viz clustering 
# - plot data filter points onto the overall "cloud" of data.
################################################################################

source("~/scripts/2_cluster/clustering_flow.R") # <- one source that sources all the packages and all the utils

#kActFeatures1 <- "breath_test_all_data_1201_feat_v2"
#kActFeatures2 <- "breath_test_all_data_0203_feat_v2"
#kActFeatures3 <-  "breath_test_all_data_04_feat_v2"
#kActTableNames = c(kActFeatures1, kActFeatures2, kActFeatures3)

  kActTableNames = c( "breath_all_data_feat_sept_may")

# specify features to cluster:
clusterCols <- c( "patientId", "actDate",
                     "breathCount", "meanBreathDuration", "sdBreathDuration", 
                     "meanBreathAmplitude",  "sdBreathAmplitude",
                    "treatmentRollWeekAdherenceScore", "breathRollWeekAdherenceScore",
                    "treatmentDurationMins")
                    

LoadACTData <- function(kActTableNames) {
    act = NULL
    for (t in kActTableNames) {
       
        actDB <- xap.conn %>% tbl(t)
        act_t <- actDB %>% collect() 
        act = rbind(act, act_t)
    }
    
    
    # Cleaning
    cleanDf  <- act %>%
      filter(treatmentId != "zeroT") %>% # optionally filter out non-treatments/empty days
      filter(pressuresDayCompletenessScore==1) %>% # only keep completed treatments
      filter(breathCount > 0) %>%
      replace(., is.na(.), 0) # Replace NAs by 0, for now!
    
    cleanDf <-  cleanDf
    print(str(cleanDf))
    return(cleanDf)
}



ClusterWithViz <- function(data, 
                           clusterCols, # <- make sure id and date is among them
                           removeOutliers = T, 
                           doTendency=T, 
                           doKMeans = T, 
                           doHC = T,
                           nbClusters = 4,
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
        
        # need to pick specific features to remove outliers from, do not want to remove them all.
        
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
# Experiment
#############################################################

data <- LoadACTData(kActTableNames)
filters <- c("patientId == '023f735c-f7d4-4ed2-abac-eb344d8b848c'", "patientId == '67607e84-7e82-484e-bb08-bf401c50729a'")

ClusterWithViz(data, clusterCols, removeOutliers = T, doTendency = F, doKMeans = T, doHC = T, nbClusters = 4, filters = filters, save = T)
