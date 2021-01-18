link = xap.read_table('linkMoresecure')
#linkGos = link[substr(link$study_uid,1,3) != '102',]# Exclude Start
# Script to cluster and visualise clustering of ACT data
# Used for NeurIPS ML4Health submission

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
source("~/scripts/2_cluster/utils/data_utils.R");
source("~/scripts/2_cluster/utils/validity_metrics_utils.R");
source("~/scripts/2_cluster/utils/outliers_utils.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.r");
source("~/scripts/R_scripts/data_cleaning_utils.R");
source("~/scripts/2_cluster/utils/data_distribution_utils.R")
source("~/scripts/2_cluster/utils/clustering_tendency_utils.R");


kActFeatures1 <- "act_featurised_sept_2020_no_treatments_with_0_breaths"

kActTableNames = c(kActFeatures1)

ClusterWithViz <- function(kActTableNames, kFeatureNames, funcOutliers, 
                           removeOutliers = T, doTendency=T, doKMeans = T, doHC = T, nClusters = 3, random_sample_size = 1){  
  
    set.seed(41)
    print("********** Loading data...")
    
    act = NULL
    
    for (t in kActTableNames) {
       
        actDB <- xap.conn %>% tbl(t)
        act_t <- actDB %>% collect() 
        act = rbind(act, act_t)
    }
    
    act = act[act$actDate <= '2019-11-01',]
    
    
    #print(nrow(act))
    
    act = act[act$patientId %in% link$patient_record_id,]
    #print(nrow(act))
    
    # Raw numbers
    nrow(act)
    print(paste("Total treatments = ", nrow(act)))
    #24434
    
    #Time period:
    firstDate <- act %>% dplyr::select(actDate) %>% arrange(actDate) %>% slice(1)
    # 2018-09-10
    lastDate <- act %>% dplyr::select(actDate) %>% arrange(actDate) %>% slice(n())
    #2019-05-31
    
    #Unique participants:
    participants <- act %>% dplyr::select(patientId) %>% unique %>% summarise(n = n())
    print(paste("Number of participants in dataset = ", participants))
    #139
    
    # Cleaning
    cleanDF  <- act %>%
      filter(treatmentId != "zeroT") %>% # optionally filter out non-treatments/empty days
      filter(pressuresDayCompletenessScore==1) %>% # only keep completed treatments (more than 15 seconds long)
      filter(breathCount > 0) %>% #filter out the treatments with zero breaths
      replace(., is.na(.), 0) # Replace NAs by 0, for now!
      
      
    # Numbers after cleaning:  
    print(paste("Filtered treatments = ", nrow(cleanDF)))
    #14689
    
    # Treatments with sets:
    sets <- cleanDF %>% dplyr::select(setCount) %>% filter(setCount >0) %>% summarise (n = n())
    print(paste("Number of treatments with sets = ", sets))
    #2164

    if (random_sample_size > 0 & random_sample_size  < 1) {
        nrow(cleanDF)
        cleanDF <- cleanDF %>% sample_frac (size =random_sample_size)
        cat(sprintf("Size after sampling: %s \n",  nrow(cleanDF)))
    }
    
    cleanDF <- cleanDF %>%
      mutate(
        treatmentDurationMins = treatmentLength/60
      )
      
    clusterDF <-  cleanDF  %>% dplyr::select(kFeatureNames) 
    
    str(clusterDF)
    nrow(clusterDF)
    
    
    ###################################################################################################
    # Correlation and data distribution analysis
    ###################################################################################################
    
    #print("********** Running correlation analysis...")
    
    #CheckDatasetDistribution(clusterDF)  
    
    #cormatrix <- rcorr(as.matrix(dplyr::select_if(clusterDF, is.numeric)), type = "pearson")
    
    #pearsonPlot <- corrplot(cormatrix$r, type="upper", order="hclust", 
    #                        main = "Pearson Correlation Matrix for ACT data", tl.srt=45, tl.cex = 0.8,
    #                        p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
             
             
    #cormatrix <- rcorr(as.matrix(dplyr::select_if(clusterDF, is.numeric)), type = "spearman")
    
    #spearmanPlot <- corrplot(cormatrix$r, type="upper", order="hclust",
    #                         main = "Spearman Correlation Matrix for ACT data", tl.srt=45, tl.cex = 0.8,
    #                         p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
    
    
    ###################################################################################################
    # Outliers
    # Optionally remove outliers from some features
    ###################################################################################################
    #print(nrow(clusterDF))
    bkp <-  clusterDF
    if (removeOutliers) {
       clusterDF <- funcOutliers(clusterDF, kFeatureNames)
       nrow(clusterDF)
    } else {
        print("********** Outliers are kept...")
    }    
    #print(nrow(clusterDF))
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
    
    #print(plot_grid(pca$plot, umapScaled$plot))
    
    # PCA viz
    multiPlot <- PcaDetailedPlot(pca$obj)
    print(plot_grid(multiPlot$explainedVariance, multiPlot$cos2Circle, multiPlot$cos2Map, multiPlot$cos2Bar)) 
    print(plot_grid(multiPlot$explainedVariance, multiPlot$contribCircle, multiPlot$contribMap, multiPlot$contribBar))
    
    
    ###################################################################################################
    # KMeans  Clustering
    ###################################################################################################
    
    if (doKMeans) {
        print("********** Performing KMeans clustering...")

        sizePlot <- 0.6
        alphaPlot <- 0.2
        kClusters <- KMeansClustering(numDF, nbClusters = nClusters)
        
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
        kpca                                
        kumap <- DimReductionScatterPlot(umapScaled$results,
                                         "X1", "X2", 
                                         as.character(kClusters$cluster),
                                         size = sizePlot,
                                         alpha = alphaPlot,
                                         #xLim = c(-15, 15),
                                         #yLim = c(-5, 10),
                                         title ="umap - kmeans")
    
        #print(plot_grid(kpca, kumap))
        plot(kpca)
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
        
        #print(paste("Silhouette Value : ", siKM$clusterAvg))
        #print(siKM$plot)
        #print(paste("CH Index for KMeans is", chKM))
        
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
        #print(plot_grid(kpcaFF, kpcaTF, kpcaFT, kpcaTT))                                
        
        # Plot clustering on UMAP                  
        kumapFF <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(kClusters$cluster),
                                         title = "umap - kmeans", include.chull = F, include.ellipse=F)
        
        # Overlay clustering centers
        pcaKCenters = data.frame(predict(pca$obj, kClusters$centers))
        umapKCenters = data.frame(predict(umapScaled$obj, kClusters$centers))
        
        kpca <- OverlayProjectionPoints(kpcaTF, pcaKCenters, "PC1", "PC2", title = "Kmeans Centroids on PCA projection", color="black", size=1, shape=8)
        kumap <- OverlayProjectionPoints(kumapFF, umapKCenters, "X1", "X2", title = "Kmeans Centroids on UMAP projection", color="black", size=1, shape=8)
        
        #print(plot_grid(kpca, kumap, siKM$plot))
        
        ###################################################################################################
        # Patient overlaid onto the clusters
        ###################################################################################################
        funcUID <- "2c5f35f9-68ba-4469-aae3-9fad7e3dd274"
        uPcaDf <-  pca$results %>% filter(patientId == funcUID)
        uUmapDf <- umapScaled$results %>% filter(patientId == funcUID)
        
         
        upca <- OverlayProjectionPoints(kpcaTF, uPcaDf, "PC1", "PC2", title = 'Functional user', size=1, alpha = 0.5)
        uumap <- OverlayProjectionPoints(kumapFF, uUmapDf, "X1", "X2", title = 'Functional user', size=1, alpha = 0.5) 
    
        #print(plot_grid(upca, uumap))
        
         # Get info fo saving clusters
        k <- nClusters
        clusterCols <-  kFeatureNames[!(kFeatureNames %in% c("patientId", "actDate"))] 
        clusterDF$clusterAssignment <- kClusters$cluster
        pcaTransformed <- predict(pca$obj, numDF[clusterCols])#clusterDF[clusterCols])
        clusterDF$PCA1 <- pcaTransformed[, 'PC1']
        clusterDF$PCA2 <- pcaTransformed[, 'PC2']
        
        featureCenters <- data.frame(unscale(kClusters$centers, scale(unscaledDF[clusterCols])))
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
        outDf <- rbind(clusterDF[colsOutput], pcaCenters[colsOutput])
        
        return(list(clusterDF = clusterDF, pca = pca, umapScaled = umapScaled, kpcaTF = kpcaTF, uPcaDf = uPcaDf, kumapFF = kumapFF, uUmapDf = uUmapDf, outDf = outDf, "centroids"  = featureCenters))
        
    
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
        #plot(hc)
    
        # Choose the number of clusters based on the tree (or use param).
        nbClusters <- nClusters 
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
        #print(siHC$plot)
                                      
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
        #print(plot_grid(hcpcaFF, hcpcaTF, hcpcaFT, hcpcaTT))                                
        
        # Plot clustering on UMAP                  
        hcumapFF <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(clusters),
                                         title = "umap - kmeans", include.chull = F, include.ellipse=F)
        #print(plot_grid(hcpcaFF,  hcumapFF))                               
        
    
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
        
        #print(plot_grid(hcpcaMean, hcpcaMedian, hcumapMean, hcumapMedian))
        
        ###################################################################################################
        # Patient overlaid onto the clusters
        ###################################################################################################
        funcUID <- "2c5f35f9-68ba-4469-aae3-9fad7e3dd274"
        uPcaDf <-  pca$results %>% filter(patientId == funcUID)
        uUmapDf <- umapScaled$results %>% filter(patientId == funcUID)
        upca <- OverlayProjectionPoints(hcpcaTF, uPcaDf, "PC1", "PC2", title = 'Functional user', size=1, alpha = 0.5)
        uumap <- OverlayProjectionPoints(hcumapFF, uUmapDf, "X1", "X2", title = 'Functional user', size=1, alpha = 0.5) 
    
        #print(plot_grid(upca, uumap))                              
    
    } else {
        print('********* Skipping Hierarchical Clustering ...')
    }
}



###############################################################
# Individual plots:


PatientInteventPlots <- function(funcUID, actDate_start, actDate_end, title, pca, umapScaled, kpcaTF, uPcaDf, kumapFF, uUmapDf) {
    shortID <- substr(funcUID, 1,3)
    uPcaDf <-  pca$results %>% filter(patientId == funcUID) %>%
                               filter(actDate >= actDate_start & actDate <= actDate_end)
    uUmapDf <- umapScaled$results %>% filter(patientId == funcUID) %>% 
                                      filter(actDate >= actDate_start & actDate <= actDate_end)
        
    #plotTitle <- sprintf("id: %s, %s [%s, %s]", shortID, title, actDate_start, actDate_end)
    plotTitle <- title
    upca <- OverlayProjectionPoints(kpcaTF, uPcaDf, 
                                   "PC1", "PC2", title = plotTitle,
                                   size=1, alpha = 0.5) + theme(plot.title = element_text(size = 10))
    uumap <- OverlayProjectionPoints(kumapFF, uUmapDf, 
                                     "X1", "X2", title = plotTitle,
                                     size=1, alpha = 0.5) + theme(plot.title = element_text(size = 10))
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
                                            title = "No feedback",
                                            pca, umapScaled, kpcaTF, uPcaDf, kumapFF, uUmapDf)

    # Feedback has been introduced
    actDate_start <- actDate_end + days(1)
    actDate_end <- lubridate::date(plinkMoresecData$date_gaming_start) - days(1)
    res_feedb <- PatientInteventPlots(funcUID, actDate_start, actDate_end,
                                            title = "Feedback introduced",
                                            pca, umapScaled, kpcaTF, uPcaDf, kumapFF, uUmapDf)
                                            
    # Gaming has been introduced                                        
    actDate_start <- actDate_end + days(1)
    actDate_end <- lubridate::date("2019-05-31")
    res_game <- PatientInteventPlots(funcUID, actDate_start, actDate_end,
                                            title = "Games introduced",
                                            pca, umapScaled, kpcaTF, uPcaDf, kumapFF, uUmapDf)  
    print(plot_grid(res_noIntervent$upca, res_feedb$upca, res_game$upca,
                    res_noIntervent$uumap, res_feedb$uumap, res_game$uumap,
                    nrow = 2, ncol = 3))
    i <- i + 1
  }
}


OutlierFunc3 <- function(clusterDF, kFeatureNames) {
    IdentifyOutliers(clusterDF, kFeatureNames, verbose = TRUE)
    nrow(clusterDF)
    print("********** Removing outliers...")
    kOutlierNames <- kFeatureNames
    outlierDF <- clusterDF
    clusterDF <- RemoveOutliers(outlierDF, c("breathCount",  "treatmentDurationMins"), thr = 0.073, verbose=TRUE)
    nrow(clusterDF)
    
    outlierDF <- clusterDF
    clusterDF <- RemoveOutliers(outlierDF, kOutlierNames[!(kOutlierNames %in% c("breathCount",  "relativeTreatmentDuration"))] , thr = 0.05,                               verbose=TRUE)
    nrow(clusterDF)
    return(clusterDF)
}

kActFeatures1 <- "act_featurised_sept_2020_no_treatments_with_0_breaths"

actTableNames = c(kActFeatures1)
    
    
# Specific saved experiments:
    

###     Clustering on features as per July 25th feedback from GOSH
Experiment3 <- function() {
    featureNames <- c( "patientId", "actDate",
                     "breathCount", "meanBreathDuration", 
                     "meanBreathAmplitude",
                    "breathsWeekAdherenceScore",
                    "treatmentDurationMins")
    
    # Using the outlier func that knows how to deal with "treatmentDurationMins"               
    cRes <- ClusterWithViz(actTableNames, featureNames, OutlierFunc3, removeOutliers = T, doTendency=F, doKMeans = T, doHC = F, nClusters = 3)

    #OnePatientCluster(cRes$clusterDF, cRes$pca, cRes$umapScaled, cRes$kpcaTF, cRes$uPcaDf, cRes$kumapFF, cRes$uUmapDf)
    #SaveOutput(cRes$outDf, "act_kmeans_exp_viz2", cRes$centroids, "act_kmeans_centroids_viz2")
}

###     Clustering consistency (for expriment #3)
Experiment4 <- function() {
    featureNames <- c( "patientId", "actDate",
                     "breathCount", "meanBreathDuration", "sdBreathDuration", 
                     "meanBreathAmplitude",  "sdBreathAmplitude",
                    "treatmentRollWeekAdherenceScore", "breathRollWeekAdherenceScore",
                    "treatmentDurationMins")
    
    # Using the outlier func that knows how to deal with "treatmentDurationMins"               
    cRes <- ClusterWithViz(actTableNames, featureNames, OutlierFunc3, 
                           removeOutliers = T, doTendency=F, doKMeans = T, doHC = F, nClusters = 4,
                           random_sample_size = 0.8)

    OnePatientCluster(cRes$clusterDF, cRes$pca, cRes$umapScaled, cRes$kpcaTF, cRes$uPcaDf, cRes$kumapFF, cRes$uUmapDf)
}

SaveOutput <- function(outDf, tableNameOut, centroids, tableNameCentroids) {
    timeNow <- Sys.time()
    outDf$dateClustered <- timeNow
    centroids$dateClustered <- timeNow
    
    colnames(outDf)[colnames(outDf)=="actDate"] <- "date"

    xap.db.writeframe(outDf, tableNameOut)
    xap.db.writeframe(centroids,  tableNameCentroids)
}


ValidateOutputables <- function(tableNameOut, tableNameCentroids) {
   df <- xap.read_table(tableNameOut)
   centroids <- xap.read_table(tableNameCentroids)
   colsOutput <- c('patientId', 'actDate', 'PCA1', 'PCA2', 'clusterAssignment')
   
   colx <- 'PCA1'
   coly <- 'PCA2'
   title <- "validate output table"
   legend <- NULL
   xcol <- rlang::parse_expr(colx)
   ycol <- rlang::parse_expr(coly)
   ggplot(df, aes(x = !!xcol, y = !!ycol)) +
          geom_point(color = "blue") +
          ggtitle(title)

}

#data = xap.read_table('test_act_featurise_new')
#data2 = data[data$actDate <= as.Date('2019-08-31'),]
#xap.db.writeframe(data2,'featurised_august_31')
# Exclude Endv


link = xap.read_table('linkMoresecure')
#linkGos = link[substr(link$study_uid,1,3) == '101',]



fev = xap.read_table('patient_lung_function_results_spiro_gos_rlh_rbh')
#link = xap.read_table('linkMoresecure')
dataMerge = merge(cRes$clusterDF,link[,c('gos_id','patient_record_id','date_recruited','age_recruited')],by.x = 'patientId', by.y = 'patient_record_id', all.x = TRUE)
#results = data.frame()
#mvpa = xap.read_table('mvpa_count_prev_15')

#mvpa_summary = mvpa %>% 
#    group_by(userid) %>%
#    summarise(mean_mvpa = mean(neighbour15Rolling14), mean_wear_time = mean(minsWearTotal))

#mvpa_summary = merge(mvpa_summary,link[,c('patient_record_id','fizzyo_hub_id')],by.x = 'userid',by.y = 'fizzyo_hub_id')


fev2 = fev[fev$gos_id %in% link$gos_id,]
fev2 = fev2[!is.na(fev2$fev1_pct_pred),]
#fev2$date2 = as.Date(fev2$date_of_tes,t)
#unique(fev2$gos_id)


#closest <- function(date,g)
#{
#    fevP = fev2[fev2$gos_id == g,]
#    fevP$dist = abs(as.Date(fevP$date_of_test) - date)
#    fev1_pct_pred = fevP[fevP$dist == min(fevP$dist),'fev1_pct_pred'][1]
#    return(fev1_pct_pred)
#}##

#dataMerge %>%
#    group_by(actDate,date_recruited,gos_id) %>%
#        mutate(fev1_pct_pred = closest(date_recruited,gos_id))-> res
    
res = data.frame()

for(g in unique(dataMerge$gos_id)){
    
    dataMergeG = dataMerge[dataMerge$gos_id == g,]
    fevP = fev2[fev2$gos_id == g,]
    if(nrow(fevP)>0){
        date_recruited = link[link$gos_id == g,'date_recruited']
        fevP$dist = as.integer(abs(as.Date(fevP$date_of_test) - date_recruited))
        fev1_pct_pred = fevP[fevP$dist == min(fevP$dist),'fev1_pct_pred'][1]
        dataMergeG$fev1_pct_pred = fev1_pct_pred
        res = rbind(res,dataMergeG)
    }
    
    
}   

        
closest(dataMerge[1,]$date,dataMerge[1,]$gos_id )


#res = merge(res,link[,c('patient_record_id','age_recruited')],by.x = 'patientId',by.y = 'patient_record_id',all.x = TRUE)
#closest(dataMerge[1,]$date,dataMerge[1,]$gos_id ),
#dataMerge = merge(dataMerge, fev[,c('hospital_no','fev1_z')], by.y = 'hospital_no', by.x = 'gos_id', all.x = TRUE)
#res = merge(res,mvpa_summary,by.x = 'patientId',by.y = 'patient_record_id')

library(ggplot2)
ggplot(m, aes(x = activeMinsHr0, y = switch120, colour = clusterAssignment)) +
  geom_point() +
  facet_wrap( ~ clusterAssignment)
  
library(ggplot2)
ggplot(res, aes(x = clusterAssignment, y = fev1_pct_pred, colour = clusterAssignment)) +
  geom_point()


res %>%
  group_by(clusterAssignment) %>%
  #summarise_at(c('meanBreathDuration','meanBreathAmplitude','treatmentDurationMins','fev1_pct_pred','age_recruited'), funs(mean(., na.rm=TRUE), median(., na.rm=TRUE), min(., na.rm=TRUE),max(., na.rm=TRUE),count = n())) %>%
  #summarise_at(c('breathCount','meanBreathDuration','meanBreathAmplitude','treatmentDurationMins','fev1_pct_pred','age_recruited'), funs(mean(., na.rm=TRUE))) %>%
  summarise(meanBreathCount = mean(breathCount), meanBreathDuration = mean(meanBreathDuration),meanBreathAmplitude=mean(meanBreathAmplitude),
  treatmentDurationMins = mean(treatmentDurationMins), fev1_pct_pred = mean(fev1_pct_pred),age_recruited = mean(age_recruited),count = n()) %>%
  as.data.frame() -> summary

summary = summary[,1:26]
colnames(summary)[which(names(summary) == "meanBreathDuration_count")] <- "no_of_days"

xap.db.writeframe(summary,'act_clustering_fev_mvpa')


