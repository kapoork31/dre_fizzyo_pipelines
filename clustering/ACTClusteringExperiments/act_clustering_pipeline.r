
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
kFeatureNames <-  c("patientId", "actDate",  "meanBreathDuration" , "meanBreathAmplitude", "breathCount", "treatmentDuration",  "breathRollWeekAdherenceScore")


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
    
    
    cleanDF <- cleanDF %>% mutate(treatmentDuration = treatmentDuration/60)
    
    clusterDF <-  cleanDF  %>% select(kFeatureNames) 
    
    str(clusterDF)
    print(paste("Number of rows in clusterDF: ", nrow(clusterDF)))
    
    
    ###################################################################################################
    # Correlation and data distribution analysis
    ###################################################################################################
    
    print("********** Running correlation analysis...")
    
    corrDF <- clusterDF %>%  select(-patientId, -actDate)
    
    CheckDatasetDistribution(corrDF)  
    
    cormatrix <- rcorr(as.matrix(corrDF), type = "pearson")
    
    pearsonPlot <- corrplot(cormatrix$r, type="upper", order="hclust", 
                            main = "Pearson Correlation Matrix for ACT data", tl.srt=45, tl.cex = 0.8,
                            p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
             
             
    cormatrix <- rcorr(as.matrix(corrDF), type = "spearman")
    
    spearmanPlot <- corrplot(cormatrix$r, type="upper", order="hclust",
                             main = "Spearman Correlation Matrix for ACT data", tl.srt=45, tl.cex = 0.8,
                             p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
    
    
  
   #test1 = ggplot(data=test, aes(test$setCount)) + geom_histogram(bins = 200)
    
    #test2 = clusterDF %>% mutate(logDaysSkipped = log(daysSkipped + 1))
    
    #ggplot(data=test2, aes(test2$logDaysSkipped)) + geom_histogram(bins = 100)
    
    
    ###################################################################################################
    # Outliers
    # Optionally remove outliers from some features
    ###################################################################################################
    
    print("********** Removing outliers...")
    
    dfOutliers <- clusterDF
    
       # Manually check outliers without removing: 
       #DetectOutliers(dfOutliers, breathsDayAdherenceScore, "breathsDayAdherenceScore")
       #DetectOutliers(dfOutliers, treatmentRollWeekAdherenceScore, "treatmentRollWeekAdherenceScore")
       #DetectOutliers(dfOutliers, meanBreathAmplitude, "meanBreathAmplitude")
       #DetectOutliers(dfOutliers, breathCount, "breathCount")
       #DetectOutliers(dfOutliers, meanBreathDuration, "meanBreathDuration")
       #DetectOutliers(dfOutliers, daysSkipped, "daysSkipped")
    DetectOutliers(dfOutliers, breathRollWeekAdherenceScore, "breathRollWeekAdherenceScore")
    
    
       DetectOutliers(dfOutliers, breathCount, "breathCount", shouldRemove = TRUE)
       cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
       dfOutliers <- dfOutliers %>% drop_na
       cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))

       DetectOutliers(dfOutliers, meanBreathAmplitude, "meanBreathAmplitude", shouldRemove = TRUE)
       cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
       dfOutliers <- dfOutliers %>% drop_na
       cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))
    
       DetectOutliers(dfOutliers, meanBreathDuration, "meanBreathDuration", shouldRemove = TRUE)
       cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
       dfOutliers <- dfOutliers %>% drop_na
       cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))
       
       DetectOutliers(dfOutliers, treatmentDuration, "treatmentDuration", shouldRemove = TRUE)
       cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
       dfOutliers <- dfOutliers %>% drop_na
       cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))
       
       DetectOutliers(dfOutliers, breathRollWeekAdherenceScore, "breathRollWeekAdherenceScore", shouldRemove = TRUE)
       cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
       dfOutliers <- dfOutliers %>% drop_na
       cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))


    
        print(paste("Number of rows after outliers have been dropped (dfOutliers): ", nrow(dfOutliers)))  
        
        print(paste("Total dropped rows : ", nrow(clusterDF) - nrow(dfOutliers)))

        head(droppedRows)
        

    clusterDF <- dfOutliers
    str(clusterDF)
    
    
    #test1 = ggplot(data=clusterDF, aes(clusterDF$daysSkipped)) + geom_histogram(bins = 200)
    
    #test2 = clusterDF %>% mutate(logDaysSkipped = log(daysSkipped + 1))
    
    #ggplot(data=test2, aes(test2$logDaysSkipped)) + geom_histogram(bins = 100)
    
    ###################################################################################################
    # Scale
    ###################################################################################################
    
    print("********** Scaling...")
    
    # For now, replace missing values by column average
    # Then scale with the z-score transform
    unscaledDF <- clusterDF %>%
                select(-patientId, -actDate) %>%
                mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
                mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
                replace_na(as.list(colMeans(., na.rm=T)))
                
    numDF <- unscaledDF %>% mutate_if(is.numeric, funs(scale))
    str(numDF)
    
    
    ###################################################################################################
    # Clustering tendency
    ###################################################################################################
    
    print("********** Assessing clustering tendency...")
    
    # create dissimilarity plot with scaled data:
    hopkinsStatistic <- HopkinsStatistic(numDF)
    print(paste("Hopkins statistic = ", hopkinsStatistic))
    
    #DissimilarityPlot(numDF)
    
    ###################################################################################################
    # Dimensionality reduction
    ###################################################################################################
    
    print("********** Performing dimensionality reduction...")
    
    # Run dimensionality reduction of the scaled
    # dataset (both PCA and UMAP) and plot the results
    pca <- PcaRunPlot(numDF)
    #pca$plot
    
    umapScaled <- UmapRunPlot(numDF)
    #umapScaled$plot
    
    # PCA viz
    multiPlot <- PcaDetailedPlot(pca$obj)
    plot_grid(multiPlot$explainedVariance, multiPlot$cos2Circle, multiPlot$cos2Map, multiPlot$cos2Bar) 
    plot_grid(multiPlot$explainedVariance, multiPlot$contribCircle, multiPlot$contribMap, multiPlot$contribBar)
    
    ###################################################################################################
    # Save PCA1 and PCA2 for visualising
    ###################################################################################################

    clusterCols = c("meanBreathDuration" , "meanBreathAmplitude", "breathCount", "treatmentRollWeekAdherenceScore", "breathRollWeekAdherenceScore")
    
    pcaTransformed <- predict(pca$obj, numDF[clusterCols])
    pca1 <- pcaTransformed[, 'PC1']
    pca2 <- pcaTransformed[, 'PC2']
    
    actKmeansCluster <- clusterDF %>% select(patientId, actDate) %>% 
                                        rename("date" = actDate) %>%
                                        mutate(PCA1 = pca1, PCA2 = pca2)
                        
    
    ###################################################################################################
    # Clustering
    ###################################################################################################
    
    print("********** Performing clustering...")
    
    set.seed(41)
    sizePlot <- 0.8
    alphaPlot <- 0.2
    kClusters <- KMeansClustering(numDF, nbClusters = 3)
    #kClusters <- KMeansClustering(numDF, runs = TRUE, runsMaxRange = 7)
    
    ##################################################################################################
    # Save cluster assignment for visualisation
    ###################################################################################################
    
    # Used for viz app
    actKmeansCluster <- actKmeansCluster %>%
                        mutate(clusterAssignment = kClusters$cluster,
                                dateClustered = Sys.time())
                        
    
    ###################################################################################################
    # Basic clustering plots
    ###################################################################################################
    
    print("********** Basic clustering plots...")
    
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
    # Cluster centroids
    ###################################################################################################
    
    # Print cluster centers and unscale them for the numbers to have meaning
    cat("Cluster centers: \n")
    unscale(kClusters$centers, scale(unscaledDF))
    
    # Get clustering metric
    cat("Cluster metrics (for PCA clusters): \n")
    si <- SilhouetteValue(kClusters$cluster, numDF)
    si$plot
    ch <- CHIndex(kClusters$cluster, numDF)
    cat(sprintf("Silhouette Values: %s \n", si$clusterAvg))
    cat(sprintf("Calinski-Harabasz index: %s \n", ch))
    
    ###################################################################################################
    # Detailed clustering plots
    ###################################################################################################
    
    print("********** Detailed clustering plots...")
    
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
    
    
    ###################################################################################################
    # Hierarchical clustering
    ###################################################################################################
    
    # Run hierarchical clustering and show the clusters on pca, tsne and umap
    print('********** Performing Hierarchical Clustering...')
    # Run HC with the default euclidean distance and plot the tree.
    hc <- HierarchicalClustering(numDF, method = "ward.D")
    plot(hc)
    
    # Choose the number of clusters based on the tree.
    nbClusters <- 4
    hcClusters <- cutree(hc, k = nbClusters)
    
    print("********** HC basic clustering plots...")
    hcPcaPlot <- DimReductionScatterPlot(pca$results,"PC1","PC2",as.character(hcClusters),title = "PCA - HC")
    hcUmapPlot <- DimReductionScatterPlot(umapScaled$results,"X1","X2",as.character(hcClusters), title ="Umap - HC")
    print(plot_grid(hcPcaPlot, hcUmapPlot))


print("********** Compute internal validity metrics for HC clusters....")
        hcSi <- SilhouetteValue(hcClusters, numDF)
        hcCh <- CHIndex(hcClusters, numDF)

 
 
    ###################################################################################################
    # Overwrite cluster table to update viz app with new clustering results
    ################################################################################################### 
 
     xap.db.writeframe(actKmeansCluster, "act_kmeans")
