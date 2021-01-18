# Exclude Start
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
install.packages("factoextra")

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
source("~/scripts/2_cluster/utils/data_distribution_utils.R")
source("~/scripts/2_cluster/utils/clustering_tendency_utils.R");

#kActTableName <- "breath_test_all_data_month_feat_v2"

kActFeatures1 <- "breath_all_data_feat_sept_may"
#kActFeatures2 <- "breath_test_all_data_0203_feat_v2"
#kActFeatures3 <-  "breath_test_all_data_04_feat_v2"

#kActTableNames = c(kActFeatures1, kActFeatures2, kActFeatures3)
kActTableNames = c(kActFeatures1)
# List all col "umns (features) in table:""
#all_feature_names <- xap.conn %>% tbl(kActTableName) %>% colnames()
#print(all_feature_names)

# specify features to cluster:
#kFeatureNames <- c( "patientId", "actDate",
#                   "meanBreathDuration" , "meanBreathAmplitude", "breathCount", 
#                   "treatmentRollWeekAdherenceScore", "breathRollWeekAdherenceScore")
                   
kFeatureNames <- c("patientId", "actDate", 
                    "meanBreathDuration" , "meanBreathAmplitude", 
                    "breathCount",
                    "treatmentRollWeekAdherenceScore",
                    "relativeTreatmentDuration")
                    
                


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


ClusterWithViz <- function(kActTableNames, kFeatureNames, funcOutliers, removeOutliers = T, doTendency=T, doKMeans = T, doHC = T){  

removeOutliers = T
doTendency=F
doKMeans = T
doHC = F    
    print("********** Loading data...")
    
    act = NULL
    
    for (t in kActTableNames) {
       
        actDB <- xap.conn %>% tbl(t)
        act_t <- actDB %>% collect() 
        act = rbind(act, act_t)
    }
    
    
    # Cleaning
    nrow(act)
    cleanDF  <- act %>%
      filter(treatmentId != "zeroT") %>% # optionally filter out non-treatments/empty days
      filter(pressuresDayCompletenessScore==1) %>% # only keep completed treatments
      filter(breathCount > 0) %>%
      #filter(breathCount > 0 & breathCount < 300 & meanBreathDuration < 4 & sdBreathDuration < 3 & relativeTreatmentDuration < 2) %>% #filter out the treatments with zero breaths
      replace(., is.na(.), 0) # Replace NAs by 0, for now!

    cleanDF <- cleanDF %>%
      mutate(
        relativeTreatmentDuration = treatmentDuration/1200
      )
      
    clusterDF <-  cleanDF  %>% dplyr::select(kFeatureNames) 
    
    str(clusterDF)
    nrow(clusterDF)
    
    
    ###################################################################################################
    # Correlation and data distribution analysis
    ###################################################################################################
    
    print("********** Running correlation analysis...")
    
    CheckDatasetDistribution(clusterDF)  
    
    cormatrix <- rcorr(as.matrix(dplyr::select_if(clusterDF, is.numeric)), type = "pearson")
    
    pearsonPlot <- corrplot(cormatrix$r, type="upper", order="hclust", 
                            main = "Pearson Correlation Matrix for ACT data", tl.srt=45, tl.cex = 0.8,
                            p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
             
             
    cormatrix <- rcorr(as.matrix(dplyr::select_if(clusterDF, is.numeric)), type = "spearman")
    
    spearmanPlot <- corrplot(cormatrix$r, type="upper", order="hclust",
                             main = "Spearman Correlation Matrix for ACT data", tl.srt=45, tl.cex = 0.8,
                             p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
    
    
    ###################################################################################################
    # Outliers
    # Optionally remove outliers from some features
    ###################################################################################################
    bkp <-  clusterDF
    if (removeOutliers) {
       clusterDF <- funcOutliers(clusterDF, kFeatureNames)
       nrow(clusterDF)
    } else {
        print("********** Outliers are kept...")
    }    
    
    ###################################################################################################
    # Scale
    ###################################################################################################
    
    print("********** Scaling...")
    
    # For now, replace missing values by column average
    # Then scale with the z-score transform
    unscaledDF <- clusterDF %>%
                dplyr::mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
                dplyr::mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
                dplyr::select_if(is.numeric) %>%
                replace_na(as.list(colMeans(., na.rm=T))) # <- i don't know how to replace NA's with col means only for numerics
                
    numDF <- unscaledDF %>% dplyr::mutate_if(is.numeric, funs(scale))
    
    # add back non numeric columns, hacky <- figure out a way to change this
    idDF <- dplyr::select_if(clusterDF, negate(is.numeric))
    numDF <- cbind(idDF, numDF)
    
    ###################################################################################################
    # Clustering tendency
    ###################################################################################################
    
    if (doTendency){
        print("********** Assessing clustering tendency...")
        
        # create dissimilarity plot with scaled data:
        hopkinsStatistic <- HopkinsStatistic(numDF)
        print(paste("Hopkins statistic = ", hopkinsStatistic))
        
        DissimilarityPlot(numDF, nmax=250)
    } else {
        print("********** Skipping assessing clustering tendency")
    }
    
    ###################################################################################################
    # Dimensionality reduction
    ###################################################################################################
    
    print("********** Performing dimensionality reduction...")
    
    # Run dimensionality reduction of the scaled
    # dataset (both PCA and UMAP) and plot the results
    pca <- PcaRunPlot(numDF, D1 = "PC1", D2 = "PC2")
    umapScaled <- UmapRunPlot(numDF)
    
    print(plot_grid(pca$plot, umapScaled$plot))
    
    # PCA viz
    multiPlot <- PcaDetailedPlot(pca$obj)
    print(plot_grid(multiPlot$explainedVariance, multiPlot$cos2Circle, multiPlot$cos2Map, multiPlot$cos2Bar)) 
    print(plot_grid(multiPlot$explainedVariance, multiPlot$contribCircle, multiPlot$contribMap, multiPlot$contribBar))
    
    
    ###################################################################################################
    # KMeans  Clustering
    ###################################################################################################
    
    if (doKMeans) {
        print("********** Performing KMeans clustering...")
        
        set.seed(41)
        sizePlot <- 0.6
        alphaPlot <- 0.2
        kClusters <- KMeansClustering(numDF, nbClusters = 3)
        
        ###################################################################################################
        # Basic KMeans clustering plots
        ###################################################################################################
        
        print("********** KMeans Basic clustering plots...")
        
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
    
        print(plot_grid(kpca, kumap))
        
        ###################################################################################################
        # KMeans Cluster centroids
        ###################################################################################################
        
        # Print cluster centers and unscale them for the numbers to have meaning
        print("KMeans Cluster centers:")
        print(unscale(kClusters$centers, scale(unscaledDF)))
        
        # Get clustering metric
        print("Cluster metrics (for KMeans clusters)")
        
        siKM <- SilhouetteValue(kClusters$cluster, numDF)
        chKM <- CHIndex(kClusters$cluster, numDF)
        
        print(paste("Silhouette Value : ", siKM$clusterAvg))
        print(siKM$plot)
        print(paste("CH Index for KMeans is", chKM))
        
        ###################################################################################################
        # Detailed KMeans clustering plots
        ###################################################################################################
        
        print("********** KMeans detailed clustering plots...")
        
        # Plot clustering on PCA
        kpcaFF <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                        as.character(kClusters$cluster), title = "PCA - kmeans", include.chull = F, include.ellipse=F)
        kpcaTF <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                        as.character(kClusters$cluster), title = "Cluster convex hull only", include.chull = T, include.ellipse=F)
        kpcaFT <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                        as.character(kClusters$cluster), title = "Cluster normal ellipse only", include.chull = F, include.ellipse=T)
        kpcaTT <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                        as.character(kClusters$cluster), title = "Convex hull and ellipse", include.chull = T, include.ellipse=T)
        print(plot_grid(kpcaFF, kpcaTF, kpcaFT, kpcaTT))                                
        
        # Plot clustering on UMAP                  
        kumapFF <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(kClusters$cluster),
                                         title = "umap - kmeans", include.chull = F, include.ellipse=F)
        
        # Overlay clustering centers
        pcaKCenters = data.frame(predict(pca$obj, kClusters$centers))
        umapKCenters = data.frame(predict(umapScaled$obj, kClusters$centers))
        
        kpca <- OverlayProjectionPoints(kpcaTF, pcaKCenters, "PC1", "PC2", title = "Kmeans Centroids on PCA projection", color="black", size=1, shape=8)
        kumap <- OverlayProjectionPoints(kumapFF, umapKCenters, "X1", "X2", title = "Kmeans Centroids on UMAP projection", color="black", size=1, shape=8)
        
        print(plot_grid(kpca, kumap, siKM$plot))
        
        ###################################################################################################
        # Patient overlaid onto the clusters
        ###################################################################################################
        funcUID <- "023f735c-f7d4-4ed2-abac-eb344d8b848c"
        uPcaDf <-  pca$results %>% filter(patientId == funcUID)
        uUmapDf <- umapScaled$results %>% filter(patientId == funcUID)
        
         
        upca <- OverlayProjectionPoints(kpcaTF, uPcaDf, "PC1", "PC2", title = 'Functional user', size=1, alpha = 0.5)
        uumap <- OverlayProjectionPoints(kumapFF, uUmapDf, "X1", "X2", title = 'Functional user', size=1, alpha = 0.5) 
    
        print(plot_grid(upca, uumap))
        
        return(list(clusterDF = clusterDF, pca = pca, umapScaled = umapScaled, kpcaTF = kpcaTF, uPcaDf = uPcaDf, kumapFF = kumapFF, uUmapDf = uUmapDf))
        
    
    } else {
        print("********* Skipping KMeans Clustering ...")
    }    
    ###################################################################################################
    # Hierarchical clustering
    ###################################################################################################
    if (doHC) {
        # Run hierarchical clustering and show the clusters on pca and umap
        print('************ Performing KMeans Clustering')
        
        hc <- HierarchicalClustering(numDF, method = "ward.D")
        plot(hc)
    
        # Choose the number of clusters based on the tree.
        nbClusters <- 3
        clusters <- cutree(hc, k = nbClusters)
        
        # Compute the cluster means and medians
        hcClusterCenters <- ComputeClusterCenters(numDF, clusters)
        
        # Unscale them to be more meaningful
        print("HC Cluster means:")
        print(unscale(hcClusterCenters$mean, scale(unscaledDF)))
        
        print("HC Cluster medians:")
        print(unscale(hcClusterCenters$median, scale(unscaledDF)))
        
        # Get clustering metric
        print("Cluster metrics (for HC clusters)")
        
        siHC <- SilhouetteValue(clusters, numDF)
        chHC <- CHIndex(clusters, numDF)
        
        print(paste("Silhouette Value : ", siHC$clusterAvg))
        print(paste("CH Index for HC is", chHC))
        print(siHC$plot)
                                      
        ###################################################################################################
        # Detailed clustering plots
        ###################################################################################################
        
        print("********** Detailed clustering plots...")
        
        # Plot clustering PCA
        hcpcaFF <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                        as.character(clusters), title = "PCA - Hierarchical Clustering", include.chull = F, include.ellipse=F)
        hcpcaTF <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                        as.character(clusters), title = "Cluster convex hull only", include.chull = T, include.ellipse=F)
        hcpcaFT <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                        as.character(clusters), title = "Cluster normal ellipse only", include.chull = F, include.ellipse=T)
        hcpcaTT <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                        as.character(clusters), title = "Convex hull and ellipse", include.chull = T, include.ellipse=T)
        print(plot_grid(hcpcaFF, hcpcaTF, hcpcaFT, hcpcaTT))                                
        
        # Plot clustering on UMAP                  
        hcumapFF <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(clusters),
                                         title = "umap - kmeans", include.chull = F, include.ellipse=F)
        
    
        ##################################################################################################
        # Overlay clustering centers
        ##################################################################################################
        pcaHCmeans <- data.frame(predict(pca$obj, hcClusterCenters$mean))
        pcaHCmedians <- data.frame(predict(pca$obj, hcClusterCenters$median))
        
        umapHCmeans <- data.frame(predict(umapScaled$obj, hcClusterCenters$mean))
        umapHCmedians <- data.frame(predict(umapScaled$obj, hcClusterCenters$median))
        
        hcpcaMean <- OverlayProjectionPoints(hcpcaTF, pcaHCmeans, "PC1", "PC2", title = "HC means on PCA projection", color="black", size=1, shape=8)
        hcpcaMedian <- OverlayProjectionPoints(hcpcaTF, pcaHCmedians, "PC1", "PC2", title = "HC medians on PCA projection", color="black", size=1, shape=9)
        
        hcumapMean <- OverlayProjectionPoints(hcumapFF, umapHCmeans, "X1", "X2", title = "HC means on UMAP projection", color="black", size=1, shape=8)
        hcumapMedian <- OverlayProjectionPoints(hcumapFF, umapHCmedians, "X1", "X2", title = "HC medians on UMAP projection", color="black", size=1, shape=8)
        
        print(plot_grid(hcpcaMean, hcpcaMedian, hcumapMean, hcumapMedian))
        
        ###################################################################################################
        # Patient overlaid onto the clusters
        ###################################################################################################
        funcUID <- "023f735c-f7d4-4ed2-abac-eb344d8b848c"
        uPcaDf <-  pca$results %>% filter(patientId == funcUID)
        uUmapDf <- umapScaled$results %>% filter(patientId == funcUID)
        upca <- OverlayProjectionPoints(hcpcaTF, uPcaDf, "PC1", "PC2", title = 'Functional user', size=1, alpha = 0.5)
        uumap <- OverlayProjectionPoints(hcumapFF, uUmapDf, "X1", "X2", title = 'Functional user', size=1, alpha = 0.5) 
    
        print(plot_grid(upca, uumap))                              
    
    } else {
        print('********* Skipping Hierarchical Clustering ...')
    }
}

PatientInteventPlots <- function(funcUID, actDate_start, actDate_end, title, pca, umapScaled, kpcaTF, uPcaDf, kumapFF, uUmapDf) {
    shortID <- substr(funcUID, 1,3)
    uPcaDf <-  pca$results %>% filter(patientId == funcUID) %>%
                               filter(actDate >= actDate_start & actDate <= actDate_end)
    uUmapDf <- umapScaled$results %>% filter(patientId == funcUID) %>% 
                                      filter(actDate >= actDate_start & actDate <= actDate_end)
        
    plotTitle <- sprintf("id: %s, %s [%s, %s]", shortID, title, actDate_start, actDate_end)
    upca <- OverlayProjectionPoints(kpcaTF, uPcaDf, 
                                   "PC1", "PC2", title = plotTitle,
                                   size=1, alpha = 0.5) + theme(plot.title = element_text(size = 8))
    uumap <- OverlayProjectionPoints(kumapFF, uUmapDf, 
                                     "X1", "X2", title = plotTitle,
                                     size=1, alpha = 0.5) + theme(plot.title = element_text(size = 8))
   return(list(upca=upca, uumap = uumap))
}


OnePatientCluster <- function(clusterDF, pca, umapScaled, kpcaTF, uPcaDf, kumapFF, uUmapDf)
{
   # List of patient IDs (stored on DRE as CSV), hand-selected.
  kPatientFile <- "~/documents/interesting_patients_ACT_clusters.csv"
  patients <- read.csv(file = kPatientFile, header = FALSE, sep = ",", stringsAsFactors = FALSE)
  patients <- patients[, 1]
  str(clusterDF)
 
  # Pick a particular patient  for the list.
  n_patients <- length(patients) 
  i <- 1

  while (i <= n_patients) {
    funcUID <- patients[i] 
    cat(sprintf("Processsing patient # %s id=%s \n", i, funcUID))
    linkMoresecData <- xap.read_table("linkMoresecure")
    plinkMoresecData  <- linkMoresecData %>% filter(patient_record_id == funcUID)
  
    # No feedback plots
    actDate_start <- lubridate::date("2018-09-01")
    actDate_end <- lubridate::date(plinkMoresecData$date_feedback_start) - days(1)
    res_noIntervent <- PatientInteventPlots(funcUID, actDate_start, actDate_end,
                                            title = "no feedback",
                                            pca, umapScaled, kpcaTF, uPcaDf, kumapFF, uUmapDf)

    # Feedback has been introduced
    actDate_start <- actDate_end + days(1)
    actDate_end <- lubridate::date(plinkMoresecData$date_gaming_start) - days(1)
    res_feedb <- PatientInteventPlots(funcUID, actDate_start, actDate_end,
                                            title = "with feedback",
                                            pca, umapScaled, kpcaTF, uPcaDf, kumapFF, uUmapDf)
                                            
    # Gaming has been introduced                                        
    actDate_start <- actDate_end + days(1)
    actDate_end <- lubridate::date("2019-05-31")
    res_game <- PatientInteventPlots(funcUID, actDate_start, actDate_end,
                                            title = "with games",
                                            pca, umapScaled, kpcaTF, uPcaDf, kumapFF, uUmapDf)  
    print(plot_grid(res_noIntervent$upca,  res_noIntervent$uumap,
                    res_feedb$upca, res_feedb$uumap,
                    res_game$upca, res_game$uumap,
                    nrow = 3, ncol = 2))
    i <- i + 1
  }
}

OutlierFunc <- function(clusterDF, kFeatureNames) {
    IdentifyOutliers(clusterDF, kFeatureNames, verbose = TRUE)
    nrow(clusterDF)
    print("********** Removing outliers...")
    kOutlierNames <- kFeatureNames
    outlierDF <- clusterDF
    clusterDF <- RemoveOutliers(outlierDF, c("breathCount",  "relativeTreatmentDuration"), thr = 0.073, verbose=TRUE)
    nrow(clusterDF)
    outlierDF <- clusterDF
    clusterDF <- RemoveOutliers(outlierDF, kOutlierNames[!(kOutlierNames %in% c("breathCount",  "relativeTreatmentDuration"))] , thr = 0.05,                               verbose=TRUE)
    nrow(clusterDF)
    return(clusterDF)
}

kActFeatures1 <- "breath_all_data_feat_sept_may"

actTableNames = c(kActFeatures1)
    
                   
featureNames <- c("patientId", "actDate", 
                    "meanBreathDuration" , "meanBreathAmplitude", 
                    "breathCount",
                    "treatmentRollWeekAdherenceScore",
                    "relativeTreatmentDuration")
cRes <- ClusterWithViz(actTableNames, featureNames, OutlierFunc, removeOutliers = T, doTendency=F, doKMeans = T, doHC = F)

OnePatientCluster(cRes$clusterDF, cRes$pca, cRes$umapScaled, cRes$kpcaTF, cRes$uPcaDf, cRes$kumapFF, cRes$uUmapDf)

#clusterDF <- cRes$clusterDF
#pca <- cRes$pca
#umapScaled <- cRes$umapScaled
#kpcaTF <- cRes$kpcaTF
#uPcaDf <- cRes$uPcaDf
#kumapFF <- cRes$kumapFF
#uUmapDf <- cRes$uUmapDf
# Exclude End