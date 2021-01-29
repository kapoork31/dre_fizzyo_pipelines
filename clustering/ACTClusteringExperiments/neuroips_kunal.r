link = xap.read_table('linkMoresecure')
stages = xap.read_table('study_stages')
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
#install.packages("dplyr")
#install.packages("rlang")
install.packages("factoextra")
install.packages("corrplot")
install.packages("cowplot")
install.packages("DMwR")

# install packages
library(factoextra)
library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(DBI)
library(tidyr)
library(corrplot)
library(DMwR)
library(Hmisc)

# install libs

source("~/scripts/2_cluster/utils/clustering_utils.R");
source("~/scripts/2_cluster/utils/validity_metrics_utils.R");
source("~/scripts/2_cluster/utils/outliers_utils.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.r");
source("~/scripts/R_scripts/data_cleaning_utils.R");
source("~/scripts/2_cluster/utils/data_distribution_utils.R")
#source("~/scripts/2_cluster/utils/clustering_tendency_utils.R");

#source relevant scripts

kActFeatures1 <- "act_featurised_2020_12"
#act table name

kActTableNames = c(kActFeatures1)
# list of all act table names

# function below
ClusterWithViz <- function(kActTableNames, kFeatureNames, funcOutliers, 
                           removeOutliers = T, doTendency=T, doKMeans = T, doHC = T, nClusters = 3, random_sample_size = 1){  
  
    set.seed(41) # ensure reproducibility
    print("********** Loading data...")
    
    act = NULL
    
    for (t in kActTableNames) {
       
        actDB <- xap.conn %>% tbl(t)
        act_t <- actDB %>% collect() 
        act = rbind(act, act_t)
    }
    
    # pull all data

    #act = act[act$act_date <= '2019-11-01',]
    
    
    #print(nrow(act))
    # make sure all patient in link file
    act = act[act$patient_id %in% link$patient_record_id,]
    #print(nrow(act))
    
    # Raw numbers
    nrow(act)
    print(paste("Total treatments = ", nrow(act)))
    #print number of treatments, 51562
    
    #Time period:
    firstDate <- act %>% dplyr::select(act_date) %>% arrange(act_date) %>% slice(1)
    # firstDate
    lastDate <- act %>% dplyr::select(act_date) %>% arrange(act_date) %>% slice(n())
    # lastDate
    
    #Unique participants:
    participants <- act %>% dplyr::select(patient_id) %>% unique %>% summarise(n = n())
    print(paste("Number of participants in dataset = ", participants))
    #139
    
    # Cleaning
    cleanDF  <- act %>%
      filter(treatment_id != "zeroT") %>% # optionally filter out non-treatments/empty days
      filter(pressures_completeness_score==1) %>% # only keep completed treatments (more than 15 seconds long)
      filter(breath_count > 0) %>% #filter out the treatments with zero breaths
      replace(., is.na(.), 0) # Replace NAs by 0, for now!

    # Numbers after cleaning:  
    print(paste("Filtered treatments = ", nrow(cleanDF)))
    #51562
    
    # Treatments with sets:
    sets <- cleanDF %>% dplyr::select(set_count) %>% filter(set_count >0) %>% summarise (n = n())
    print(paste("Number of treatments with sets = ", sets))
    #24275

    if (random_sample_size > 0 & random_sample_size  < 1) {
        nrow(cleanDF)
        cleanDF <- cleanDF %>% sample_frac (size = random_sample_size)
        cat(sprintf("Size after sampling: %s \n",  nrow(cleanDF)))
    }
    # take random sample to cluster on
    
    cleanDF <- cleanDF %>%
      mutate(
        treatment_duration_mins = treatment_length/60
      )
    # convert treatment_length to treatment duration mins
    
    clusterDF <-  cleanDF  %>% dplyr::select(kFeatureNames) 
    # select features
    str(clusterDF)# diplay df
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
    if (removeOutliers) {
       clusterDF <- funcOutliers(clusterDF, kFeatureNames)
       nrow(clusterDF)
    } else {
        print("********** Outliers are kept...")
    }    
    #print(nrow(clusterDF))
    # outlier detected but threshold of 3% is breached so no outliers are removed

    ###################################################################################################
    # Scale
    ###################################################################################################
    
    print("********** Scaling...")

    # For now, replace missing values by column average
    # Then scale with the z-score transform
    #unscaledDF <- clusterDF %>%
    #            dplyr::mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
    #            dplyr::mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
    #            dplyr::select_if(is.numeric) %>%
    #            replace_na(as.list(colMeans(., na.rm=T))) # <- i don't know how to replace NA's with col means only for numerics
                
    unscaledDF <- clusterDF %>% mutate_if(is.numeric, funs(ifelse(is.na(.), mean(., na.rm = TRUE), .)))
    # na to column mean for all numeric columns
            
    numDF <- unscaledDF %>% dplyr::mutate_if(is.numeric, funs(scale))
    # scale data with z-transform so values now sd distance to mean
    # done to get scales of data to match
    unscaledDF_num <- unscaledDF %>% select_if(is.numeric)
    # add back non numeric columns, hacky <- figure out a way to change this
    #idDF <- dplyr::select_if(clusterDF, negate(is.numeric))
    #numDF <- cbind(idDF, numDF)
    
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
    #umapScaled <- UmapRunPlot(numDF)
    
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
        #kumap <- DimReductionScatterPlot(umapScaled$results,
        #                                 "X1", "X2", 
        #                                 as.character(kClusters$cluster),
        #                                 size = sizePlot,
        #                                 alpha = alphaPlot,
        #                                 #xLim = c(-15, 15),
        #                                 #yLim = c(-5, 10),
        #                                 title ="umap - kmeans")
    
        #print(plot_grid(kpca, kumap))
        #plot(kpca)
        ###################################################################################################
        # KMeans Cluster centroids
        ###################################################################################################
        
        # Print cluster centers and unscale them for the numbers to have meaning
        print("KMeans Cluster centers:")
        print(unscale(kClusters$centers, scale(unscaledDF_num)))
        
        # Get clustering metric
        print("Cluster metrics (for KMeans clusters)")
        
        siKM <- SilhouetteValue(kClusters$cluster, numDF)
        #chKM <- CHIndex(kClusters$cluster, numDF)
        
        print(paste("Silhouette Value : ", siKM$clusterAvg))
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
        #kumapFF <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", as.character(kClusters$cluster),
        #                                 title = "umap - kmeans", include.chull = F, include.ellipse=F)
        
        # Overlay clustering centers
        pcaKCenters = data.frame(predict(pca$obj, kClusters$centers))
        # get lcuster centers for each pc, just a cross multiplication of two matrices basically
        #umapKCenters = data.frame(predict(umapScaled$obj, kClusters$centers))
        
        kpca <- OverlayProjectionPoints(kpcaTF, pcaKCenters, "PC1", "PC2", title = "Kmeans Centroids on PCA projection", color="black", size=1, shape=8)
        #kumap <- OverlayProjectionPoints(kumapFF, umapKCenters, "X1", "X2", title = "Kmeans Centroids on UMAP projection", color="black", size=1, shape=8)
        
        #print(plot_grid(kpca, kumap, siKM$plot))
        #print(plot_grid(kpca, siKM$plot))

        ###################################################################################################
        # Patient overlaid onto the clusters
        ###################################################################################################
        funcUID <- "2c5f35f9-68ba-4469-aae3-9fad7e3dd274"
        uPcaDf <-  pca$results %>% filter(patient_id == funcUID)
        #uUmapDf <- umapScaled$results %>% filter(patient_id == funcUID)
        
         
        upca <- OverlayProjectionPoints(kpcaTF, uPcaDf, "PC1", "PC2", title = 'Functional user', size=1, alpha = 0.5)
        #uumap <- OverlayProjectionPoints(kumapFF, uUmapDf, "X1", "X2", title = 'Functional user', size=1, alpha = 0.5) 
    
        #print(plot_grid(upca, uumap))
        print(upca)
        
        # Get info fo saving clusters
        # get cluster columns, centers
        k <- nClusters
        clusterCols <-  kFeatureNames[!(kFeatureNames %in% c("patient_id", "act_date", "phase"))] 
        clusterDF$clusterAssignment <- kClusters$cluster
        # get assignment for each row

        pcaTransformed <- predict(pca$obj, numDF[clusterCols])#clusterDF[clusterCols])
        # convert scaled values to pca 1,2,3
        clusterDF$PCA1 <- pcaTransformed[, 'PC1']
        clusterDF$PCA2 <- pcaTransformed[, 'PC2']
        # retain pca1 and pc2 and places in clusterDF
        # clusterDF is what is returned
        
        featureCenters <- data.frame(unscale(kClusters$centers, scale(unscaledDF[clusterCols])))
        # get feature centers in values that make sense
        featureCenters %>% mutate(clusterAssignment = seq(1, k)) -> featureCenters
        # create clusterAssignment column
        print(featureCenters)
        
        pcaCenters <- predict(pca$obj, kClusters$centers)
        # get pca centers and place in df
        pcaCenters <- data.frame(pcaCenters)
        pcaCenters %>% mutate(clusterAssignment = seq(1, k), 
                          patient_id = 'centroid',
                          act_date = NA,
                          phase = NA,
                          isCentroid = TRUE,
                          PCA1 = PC1,
                          PCA2 = PC2) -> pcaCenters

        # add cluster assignment and other various columns 
    
        colsOutput <- c('patient_id', 'act_date', "phase", 'PCA1', 'PCA2', 'clusterAssignment')
        outDf <- rbind(clusterDF[colsOutput], pcaCenters[colsOutput])
        
        #return(list(clusterDF = clusterDF, pca = pca, umapScaled = umapScaled, kpcaTF = kpcaTF, uPcaDf = uPcaDf, kumapFF = kumapFF, uUmapDf = uUmapDf, outDf = outDf, "centroids"  = featureCenters))
        return(list(clusterDF = clusterDF, pca = pca, kpcaTF = kpcaTF, uPcaDf = uPcaDf, outDf = outDf, "centroids"  = featureCenters))
        
    
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
        uPcaDf <-  pca$results %>% filter(patient_id == funcUID)
        uUmapDf <- umapScaled$results %>% filter(patient_id == funcUID)
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
    uPcaDf <-  pca$results %>% filter(patient_id == funcUID) %>%
                               filter(act_date >= actDate_start & act_date <= actDate_end)
    uUmapDf <- umapScaled$results %>% filter(patient_id == funcUID) %>% 
                                      filter(act_date >= actDate_start & act_date <= actDate_end)
        
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


PatientInteventPlots_noumap <- function(funcUID, actDate_start, actDate_end, title, pca, kpcaTF, uPcaDf) {
    shortID <- substr(funcUID, 1,3)
    uPcaDf <-  pca$results %>% filter(patient_id == funcUID) %>%
                               filter(act_date >= actDate_start & act_date <= actDate_end)
        
    #plotTitle <- sprintf("id: %s, %s [%s, %s]", shortID, title, actDate_start, actDate_end)
    plotTitle <- title
    upca <- OverlayProjectionPoints(kpcaTF, uPcaDf, 
                                   "PC1", "PC2", title = plotTitle,
                                   size=1, alpha = 0.5) + theme(plot.title = element_text(size = 10))

   return(list(upca=upca))
}


OutlierFunc3 <- function(clusterDF, kFeatureNames) {
    IdentifyOutliers(clusterDF, kFeatureNames, verbose = TRUE)
    nrow(clusterDF)
    print("********** Removing outliers...")
    kOutlierNames <- kFeatureNames
    outlierDF <- clusterDF
    clusterDF <- RemoveOutliers(outlierDF, kFeatureNames, thr = 0.073, verbose=TRUE)
    nrow(clusterDF)
    
    #outlierDF <- clusterDF
    #clusterDF <- RemoveOutliers(outlierDF, kOutlierNames[!(kOutlierNames %in% c("breathCount",  "relativeTreatmentDuration"))] , thr = 0.05,                               verbose=TRUE)
    #nrow(clusterDF)
    return(clusterDF)
}

kActFeatures1 <- "act_post_featurised_2020_12"

actTableNames = c(kActFeatures1)
    
    
# Specific saved experiments:


OnePatientCluster_no_umap <- function(clusterDF, pca, kpcaTF, uPcaDf)
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
    res_noIntervent <- PatientInteventPlots_noumap(funcUID, actDate_start, actDate_end,
                                            title = "No feedback",
                                            pca, kpcaTF, uPcaDf)

    # Feedback has been introduced
    actDate_start <- actDate_end + days(1)
    actDate_end <- lubridate::date(plinkMoresecData$date_gaming_start) - days(1)
    res_feedb <- PatientInteventPlots_noumap(funcUID, actDate_start, actDate_end,
                                            title = "Feedback introduced",
                                            pca, kpcaTF, uPcaDf)
                                            
    # Gaming has been introduced                                        
    actDate_start <- actDate_end + days(1)
    actDate_end <- lubridate::date("2019-05-31")
    res_game <- PatientInteventPlots_noumap(funcUID, actDate_start, actDate_end,
                                            title = "Games introduced",
                                            pca, kpcaTF, uPcaDf)  
    print(plot_grid(res_noIntervent$upca, res_feedb$upca, res_game$upca,
                    nrow = 1, ncol = 3))
    i <- i + 1
  }
}

###     Clustering on features as per July 25th feedback from GOSH
Experiment3 <- function() {
    featureNames <- c( "patient_id", "act_date","phase",
                     "breath_count", "mean_breath_duration",
                     "mean_breath_amplitude")
    
    # Using the outlier func that knows how to deal with "treatmentDurationMins"               
    cRes <- ClusterWithViz(actTableNames, featureNames, OutlierFunc3, removeOutliers = T, doTendency=F, doKMeans = T, doHC = F, nClusters = 4)

    #OnePatientCluster_no_umap(cRes$clusterDF, cRes$pca, cRes$kpcaTF, cRes$uPcaDf)
    #SaveOutput(cRes$outDf, "act_kmeans_exp_viz2", cRes$centroids, "act_kmeans_centroids_viz2")

    # cRes$clusterDF contians all data with cluster features, pc1, pc2 and cluster assignment so this is the money dataframe
    # cRes$centroids contains center of each cluster for each cluster feature

    pca_data <- test_patient(cRes$clusterDF)
}

test_patient <- function(clusterDF)
{
    for (uid in unique(clusterDF$patient_id))
    {
        funcUID <- uid
        plinkMoresecData  <- link %>% filter(patient_record_id == funcUID)
        stage_date <- stages %>% filter(patient_id == funcUID)
        # No feedback plots
        #actDate_start <- lubridate::date("2018-09-01")
        #actDate_end <- lubridate::date(plinkMoresecData$date_feedback_start) - days(1)
        start_date = stage_date$date_start
        feedback_start =  stage_date$feedback_start
        gaming_start = stage_date$games_start
        gaming_end = stage_date$games_end
        feedback_end = stage_date$feedback_end
        withdrawn_date = stage_date$withdrawn
        end_date = stage_date$end
        uPcaDf <-  clusterDF %>% filter(patient_id == funcUID)
        title = uid
        p = ggplot(uPcaDf, aes(x=act_date, y=clusterAssignment, color= phase)) +
            geom_point() + geom_vline(xintercept = start_date) +
            geom_vline(xintercept = feedback_start) +
            geom_vline(xintercept = gaming_start) +
            geom_vline(xintercept = gaming_end) +
            geom_vline(xintercept = feedback_end) +
            geom_vline(xintercept = withdrawn_date) +
            geom_vline(xintercept = end_date) + 
            ggtitle(title)
        
        
        dir = '/home/sejjkk4/scripts/cluster_stages_images'
        name = paste(title,'act_cluster_by_stage.png',sep = '_')
        fname = paste(dir,name, sep ='/')
        ggsave(file = fname, p)

        
    }
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



###     Clustering consistency (for expriment #3)
Experiment4 <- function() {
    featureNames <- c( "patient_id", "act_date",
                     "breath_count", "mean_breath_duration",
                     "mean_breath_amplitude",
                    "treatment_week_adherence_score")

    # Using the outlier func that knows how to deal with "treatmentDurationMins"               
    cRes <- ClusterWithViz(actTableNames, featureNames, OutlierFunc3, 
                           removeOutliers = T, doTendency=F, doKMeans = T, doHC = F, nClusters = 4,
                           random_sample_size = 0.8)

    #OnePatientCluster(cRes$clusterDF, cRes$pca, cRes$umapScaled, cRes$kpcaTF, cRes$uPcaDf, cRes$kumapFF, cRes$uUmapDf)
    OnePatientCluster(cRes$clusterDF, cRes$pca, cRes$umapScaled, cRes$kpcaTF, cRes$uPcaDf, cRes$kumapFF, cRes$uUmapDf)

}

SaveOutput <- function(outDf, tableNameOut, centroids, tableNameCentroids) {
    timeNow <- Sys.time()
    outDf$dateClustered <- timeNow
    centroids$dateClustered <- timeNow
    
    colnames(outDf)[colnames(outDf)=="act_date"] <- "date"

    xap.db.writeframe(outDf, tableNameOut)
    xap.db.writeframe(centroids,  tableNameCentroids)
}


ValidateOutputables <- function(tableNameOut, tableNameCentroids) {
   df <- xap.read_table(tableNameOut)
   centroids <- xap.read_table(tableNameCentroids)
   colsOutput <- c('patient_id', 'act_date', 'PCA1', 'PCA2', 'clusterAssignment')
   
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
#data2 = data[data$act_date <= as.Date('2019-08-31'),]
#xap.db.writeframe(data2,'featurised_august_31')
# Exclude Endv


link = xap.read_table('linkMoresecure')
#linkGos = link[substr(link$study_uid,1,3) == '101',]



fev = xap.read_table('patient_lung_function_results_spiro_gos_rlh_rbh')
#link = xap.read_table('linkMoresecure')
dataMerge = merge(cRes$clusterDF,link[,c('gos_id','patient_record_id','date_recruited','age_recruited')],by.x = 'patient_id', by.y = 'patient_record_id', all.x = TRUE)
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
#    group_by(act_date,date_recruited,gos_id) %>%
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


#res = merge(res,link[,c('patient_record_id','age_recruited')],by.x = 'patient_id',by.y = 'patient_record_id',all.x = TRUE)
#closest(dataMerge[1,]$date,dataMerge[1,]$gos_id ),
#dataMerge = merge(dataMerge, fev[,c('hospital_no','fev1_z')], by.y = 'hospital_no', by.x = 'gos_id', all.x = TRUE)
#res = merge(res,mvpa_summary,by.x = 'patient_id',by.y = 'patient_record_id')

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
