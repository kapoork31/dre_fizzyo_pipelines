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
# - viz clustering 
# - plot individual patients points onto the overall "cloud" of data.
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
source("~/scripts/2_cluster/utils/outliers_utils.R");
#source("~/scripts/2_cluster/utils/dim_reduction_utils.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.r");
source("~/scripts/R_scripts/data_cleaning_utils.R");
source("~/scripts/R_scripts/data_distribution_utils.R");
source("~/scripts/2_cluster/utils/clustering_tendency_utils.R");

kActTableName <- "breath_test_all_data_month_feat_v2"

# List all columns (features) in table:
all_feature_names <- xap.conn %>% tbl(kActTableName) %>% colnames()
print(all_feature_names)

# specify features to cluster:
kFeatureNames <- c("meanBreathDuration" , "meanBreathAmplitude", "breathCount", "setCount", "treatmentRollWeekAdherenceScore")


###############
#    actDB <- xap.conn %>% tbl(kActTableName)
#    act <- actDB %>% collect()
    
    # Cleaning
#    cleanDF  <- act %>%
#      filter(treatmentId != "zeroT") %>% # optionally filter out non-treatments/empty days
#      filter(pressuresDayCompletenessScore==1) %>% # only keep completed treatments
#      replace(., is.na(.), 0) # Replace NAs by 0, for now!
    
#    clusterDF <-  cleanDF  %>% select(kFeatureNames) 
###############


ClusterWithViz <- function(kActTableName, kFeatureNames){  
    
    actDB <- xap.conn %>% tbl(kActTableName)
    act <- actDB %>% collect()
    
    # Cleaning
    cleanDF  <- act %>%
      filter(treatmentId != "zeroT") %>% # optionally filter out non-treatments/empty days
      filter(pressuresDayCompletenessScore==1) %>% # only keep completed treatments
      replace(., is.na(.), 0) # Replace NAs by 0, for now!
    
    clusterDF <-  cleanDF  %>% select(kFeatureNames) 
    
    str(clusterDF)
    nrow(clusterDF)
    
    
    ###################################################################################################
    # Correlation and data distribution analysis
    ###################################################################################################
    
    CheckDatasetDistribution(clusterDF)  
    
    cormatrix <- rcorr(as.matrix(clusterDF), type = "pearson")
    
    pearsonPlot <- corrplot(cormatrix$r, type="upper", order="hclust", 
                            main = "Pearson Correlation Matrix for ACT data", tl.srt=45, tl.cex = 0.8,
                            p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
             
             
    cormatrix <- rcorr(as.matrix(clusterDF), type = "spearman")
    
    spearmanPlot <- corrplot(cormatrix$r, type="upper", order="hclust",
                             main = "Spearman Correlation Matrix for ACT data", tl.srt=45, tl.cex = 0.8,
                             p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
    
    
    ###################################################################################################
    # Outliers
    # Optionally remove outliers from some features
    ###################################################################################################
    
    visualize = FALSE
    dfOutliers <- clusterDF
       #DetectOutliers(dfOutliers, breathsDayAdherenceScore, "breathsDayAdherenceScore", visualize = visualize)
       #DetectOutliers(dfOutliers, treatmentWeekAdherenceScore, "treatmentWeekAdherenceScore", visualize = visualize)
       #DetectOutliers(dfOutliers, meanBreathAmplitude, "meanBreathAmplitude", visualize = visualize)
       #DetectOutliers(dfOutliers, breathCount, "breathCount", visualize = visualize)
    
    
       #DetectOutliers(dfOutliers, breathCount, "breathCount", visualize = FALSE, shouldRemove = TRUE)
       DetectOutliers(dfOutliers, breathCount, "breathCount", shouldRemove = TRUE)
       cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
       dfOutliers <- dfOutliers %>% drop_na
       cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))
       #DetectOutliers(dfOutliers, breathCount, "breathCount (after the 2nd drop of the outliers", visualize = visualize)
    
       DetectOutliers(dfOutliers, meanBreathAmplitude, "meanBreathAmplitude", shouldRemove = TRUE)
       cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
       dfOutliers <- dfOutliers %>% drop_na
       cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))
       #DetectOutliers(dfOutliers, meanBreathAmplitude, "meanBreathAmplitude (after the 2nd drop of the outliers", visualize = visualize)
    
       DetectOutliers(dfOutliers, meanBreathDuration, "meanBreathDuration", shouldRemove = TRUE)
       cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
       dfOutliers <- dfOutliers %>% drop_na
       cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))
       
       #DetectOutliers(dfOutliers, setCount, "setCount", visualize = visualize, shouldRemove = TRUE)
       #DetectOutliers(dfOutliers, breathRollWeekAdherenceScore, "breathRollWeekAdherenceScore", visualize = visualize, shouldRemove = TRUE)
    
    
    clusterDF <- dfOutliers
    str(clusterDF)
    
    
    ###################################################################################################
    # Scale
    ###################################################################################################
    
    # For now, replace missing values by column average
    # Then scale with the z-score transform
    unscaledDF <- clusterDF %>%
                mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
                mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
                #select(-patient_record_id) %>%
                replace_na(as.list(colMeans(., na.rm=T)))
                
    numDF <- unscaledDF %>% mutate_if(is.numeric, funs(scale))
    str(numDF)
    
    
    ###################################################################################################
    # Clustering tendency
    ###################################################################################################
    
    # create dissimilarity plot with scaled data:
    hopkinsStatistic <- HopkinsStatistic(numDF)
    print(paste("Hopkins statistic = ", hopkinsStatistic))
    
    DissimilarityPlot(numDF)
    
    ###################################################################################################
    # Dimensionality reduction
    ###################################################################################################
    
    # Run dimensionality reduction of the scaled
    # dataset (both PCA and UMAP) and plot the results
    pca <- PcaRunPlot(numDF)
    pca$plot
    
    umapScaled <- UmapRunPlot(numDF)
    umapScaled$plot
    
    # PCA viz
    multiPlot <- PcaDetailedPlot(pca$obj)
    plot_grid(multiPlot$explainedVariance, multiPlot$cos2Circle, multiPlot$cos2Map, multiPlot$cos2Bar) 
    plot_grid(multiPlot$explainedVariance, multiPlot$contribCircle, multiPlot$contribMap, multiPlot$contribBar)
    
    
    ###################################################################################################
    # Clustering
    ###################################################################################################
    
    set.seed(41)
    sizePlot <- 0.8
    alphaPlot <- 0.2
    kClusters <- KMeansClustering(numDF, runs = TRUE, runsMaxRange = 7)
    
    
    ###################################################################################################
    # Basic clustering plots
    ###################################################################################################
    
    kpca <- DimReductionScatterPlot(pca$results,
                                    "PC1", "PC2",
                                    as.character(kClusters$cluster),
                                    size = sizePlot,
                                    alpha = alphaPlot,
                                    #xLim = c(-15, 15),
                                    #yLim = c(-5, 5),
                                    title = "PCA - kmeans")
    #kpca                                
    kumap <- DimReductionScatterPlot(umapScaled$results,
                                     "X1", "X2", 
                                     as.character(kClusters$cluster),
                                     size = sizePlot,
                                     alpha = alphaPlot,
                                     #xLim = c(-15, 15),
                                     #yLim = c(-5, 10),
                                     title ="umap - kmeans")
    #kumap                                 
    #ktsne <- DimReductionScatterPlot(tsneRes,
    #                                 "X1", "X2",
    #                                 as.character(kClusters$cluster),
    #                                 size = sizePlot,
    #                                 alpha = alphaPlot,
    #                                 title = "TSNE 30 - kmeans")
    
    #plot_grid(kpca, kumap, ktsne)
    plot_grid(kpca, kumap)
    
    
    ###################################################################################################
    # Detailed clustering plots
    ###################################################################################################
    
    # Plot clustering on K Means
    kpcaFF <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                    as.character(kClusters$cluster), title = "PCA - kmeans", include.chull = F, include.ellipse=F)
    kpcaTF <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                    as.character(kClusters$cluster), title = "Cluster convex hull only", include.chull = T, include.ellipse=F)
    kpcaFT <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                    as.character(kClusters$cluster), title = "Cluster normal ellipse only", include.chull = F, include.ellipse=T)
    kpcaTT <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                    as.character(kClusters$cluster), title = "Convex hull and ellipse", include.chull = T, include.ellipse=T)
    plot_grid(kpcaFF, kpcaTF, kpcaFT, kpcaTT)                                
    
    # Plot clustering on UMAP                  
    kumapFF <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(kClusters$cluster),
                                     title = "umap - kmeans", include.chull = F, include.ellipse=F)
    
    # Overlay clustering centers
    pcaKCenters = data.frame(predict(pca$obj, kClusters$centers))
    umapKCenters = data.frame(predict(umapScaled$obj, kClusters$centers))
    
    kpca <- OverlayProjectionPoints(kpcaTF, pcaKCenters, "PC1", "PC2", color="black", size=1, shape=8)
    kumap <- OverlayProjectionPoints(kumapFF, umapKCenters, "X1", "X2", color="black", size=1, shape=8)
    
    silMetrics <- SilhouetteValue(kClusters$cluster, clusterDF)
    silPlot <- silMetrics$plot
    plot_grid(kpca, kumap, silPlot)
    print(paste("CH Index for KMeans is", CHIndex(kClusters$cluster, clusterDF)))
    
    

    
    ###################################################################################################
    # Patient overlaid onto the clusters
    ###################################################################################################
    
}

ClusterWithViz(kActTableName, kFeatureNames)
