################################################################################
# This script has the experimental flow for clustering flow. 
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
install.packages("ggcorrplot")
install.packages("DMwR")
install.packages("factoextra")
install.packages("tsne")
install.packages("umap")
install.packages("fpc")

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
source("~/scripts/2_cluster/utils/dim_reduction_utils.R");
source("~/scripts/2_cluster/utils/data_distribution_utils.R")
source("~/scripts/2_cluster/utils/clustering_tendency_utils.R");

ImputeScaleData <- function(df, inpute.mean = TRUE) {

    unscaledNumDf <- df %>%
                dplyr::mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
                dplyr::mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
                dplyr::select_if(is.numeric) # <- i don't know how to replace NA's with col means only for numerics
    if (inpute.mean) {
        # replace missing values by column averages
        unscaledNumDf <- unscaledNumDf %>% replace_na(as.list(colMeans(., na.rm=T))) 
    } else {
        # replace missing values by 0
       unscaledNumDf <- unscaledNumDf %>% mutate_if(is.numeric, funs(if_else(is.na(.), 0, .)))
    }       
                
    # scale with the z-score transform            
    scaledDf <- unscaledNumDf %>% dplyr::mutate_if(is.numeric, funs(scale))
    
    # add back non numeric columns, hacky <- figure out a way to change this
    idDf <- dplyr::select_if(df, negate(is.numeric))
    scaledDf <- cbind(idDf, scaledDf)
    
    return(list("scaledDf" = scaledDf, "unscaledNumDf" = unscaledNumDf))
}

FeatureCorrelationAnalysis <- function(df, title="") {
    df <- SelectNumericColumns(df)
    pearsonCorMat <- rcorr(as.matrix(df), type = "pearson")
    spearmanCorMat <- rcorr(as.matrix(df), type = "spearman")
    
    pearsonPlot <- corrplot(pearsonCorMat$r, type="upper", order="hclust", 
                            main = paste(title, ": Pearson Correlation Matrix"),
                            tl.srt=45, tl.cex = 0.8,
                            p.mat = pearsonCorMat$P, sig.level = 0.01, insig = "blank")
    
    spearmanPlot <- corrplot(spearmanCorMat$r, type="upper", order="hclust",
                             main = paste(title,": Spearman Correlation Matrix"), 
                             tl.srt=45, tl.cex = 0.8,
                             p.mat = spearmanCorMat$P, sig.level = 0.01, insig = "blank")                        
}

PlotDetailedClusterProjections <- function(clusters,
                                           projRes, # either PCA, TSNE or UMAP
                                           include.statistics = TRUE,
                                           include.chull = TRUE, # <- only for PCA
                                           include.ellipse = TRUE, # <- only for PCA
                                           filterExp = NULL, # <- only support for date and id filtering
                                           inputDf = NULL, # <- TODO add later to support more interesting filterinf based on input data
                                           info = "") {
    # Create the color coded projection plots
    if (projRes$class == "pca") {
        dim1 <- "PC1"
        dim2 <- "PC2"
    } else {
        dim1 <- "X1"
        dim2 <- "X2"
        include.chull <- FALSE
        include.ellipse <- FALSE
    }
    
    projPlot <- DimReductionScatterPlot(projRes$results, dim1, dim2, as.character(clusters), title = paste("Dimensionality reduction", info),
                                        include.chull = include.chull, include.ellipse = include.ellipse)
    if (include.statistics) { 
        dataClusterStats <- ComputeClusterStatistics(clusters, projRes$input)
        projClusterMean <- data.frame(predict(projRes$obj, dplyr::select(dataClusterStats$mean, -cluster)))
        projClusterMedian <- data.frame(predict(projRes$obj, dplyr::select(dataClusterStats$median, -cluster)))
        
        print(paste("********** Overlaying statistics over clusters on dimensionality reduction projections..."))
        statProjPlot <- OverlayProjectionPoints(projPlot, projClusterMean, dim1, dim2,
                                                title = "Means" , color="black", size=1, shape=8)
        statProjPlot <- OverlayProjectionPoints(statProjPlot, projClusterMedian, dim1, dim2,
                                                title = "Means and Medians", color="black", size=1, shape=13)                                        
    } else {statProjPlot <- NULL}
    
    # Filter the subset of rows corresponding to a custom filter
    if (!is.null(filterExp)) {
            filterExp <- rlang::parse_expr(filterExp)
            projFilterDf <- filter(projRes$results, !!filterExp)

            print(paste("********** Overlaying filter over dimensionality reduction projections..."))
            
            n <- nrow(projFilterDf)
            if (n < 50) {
                alpha <- 1
            } else {
                alpha <- sqrt(50/n)
            }
            filterProjPlot <- OverlayProjectionPoints(projPlot, projFilterDf, dim1, dim2,
                                                title = filterExp , color="black", size=1, shape=18, alpha = alpha)
    } else {filterProjPlot <- NULL}
    
    return(list("plot"= projPlot, "statPlot"= statProjPlot, "filterPlot"= filterProjPlot)) 
}


RunExperimentMetrics <- function(clusters, scaledDf, unscaledDf) {
    print("********** Compute internal validity metrics for clusters....")
    si <- SilhouetteValue(clusters, scaledDf)
    ch <- CHIndex(clusters, scaledDf)
    
    print(paste("Silhouette Value for KMeans is: ", si$clusterAvg))
    print(paste("CH Index for KMeans is", ch))
    print(si$plot)
    
    print("********* Compute cluster statistics....") 
    stats <- ComputeClusterStatistics(clusters, unscaledDf)
    print(stats)
}


RunExperimentViz <- function(clusters, pcaRes, umapRes, filters = NULL) {
    print("********** Basic clustering plots...")
    pcaPlot <- DimReductionScatterPlot(pcaRes$results,"PC1","PC2",as.character(clusters),title = "PCA - kmeans")
    umapPlot <- DimReductionScatterPlot(umapRes$results,"X1","X2",as.character(clusters), title ="Umap - kmeans")
    print(plot_grid(pcaPlot, umapPlot))
    
    print("********** Add cluster means and medians to cluster plots...")
    pcaCenters <- PlotDetailedClusterProjections(clusters, pcaRes)
    umapCenters <- PlotDetailedClusterProjections(clusters, umapRes)
    print(plot_grid(pcaCenters$statPlot, umapCenters$statPlot))
    
    if (!is.null(filters)) {
        print("********** Add filter overlays to cluster plots...")
        for (filterExp in filters) {
            print("Overlaying filter on top of dimensionality reduction plots")
            pcaFilter <-PlotDetailedClusterProjections(clusters, pcaRes, include.statistics=FALSE, filterExp=filterExp)
            umapFilter <-PlotDetailedClusterProjections(clusters, umapRes, include.statistics=FALSE, filterExp=filterExp)
            print(plot_grid(pcaFilter$filterPlot, umapFilter$filterPlot))
        }
    }
}

GetProjectedCentroidCoord <- function(clusters, projRes) {
    scaledClusterCentroids <- ComputeClusterStatistics(clusters, pcaRes$input)$mean
    projClusterCentroids <- data.frame(predict(projRes$obj, dplyr::select(scaledClusterCentroids, -cluster))) %>%
                            mutate(cluster = scaledClusterCentroids$cluster)
}

SaveClusteringResults <- function(clusters, pcaRes, inputData, title="cluster", type = NULL) {

    if (!(type %in% c('act', 'fitbit'))) {
        stop("Error: Select a valid data type. The valid types are 'act' and 'fitbit'")
    }
    timeNow = Sys.time()
    
    # compute the coordinates of the cluster centroids in the space spanned by the original features                           
    clusterCentroids <- ComputeClusterStatistics(clusters, inputData)$mean %>%
                        mutate(dateClustered = timeNow) %>%
                        rename("clusterAssignment" = "cluster")
    
    # extract the Pca projection on first 2 dimensions
    if (type == 'act') { # some column renames according to miniapp req
        pcaProj <- pcaRes$results %>% dplyr::select("patientId", "actDate", "PC1", "PC2") %>%
                rename("PCA1" = "PC1")  %>%
                rename("PCA2" = "PC2") %>%
                rename("date" = "actDate") %>%
                mutate(clusterAssignment = clusters) %>%
                mutate(dateClustered = timeNow)
    } else {
        pcaProj <- pcaRes$results %>% dplyr::select("userid", "date", "PC1", "PC2") %>%
                rename("PCA1" = "PC1")  %>%
                rename("PCA2" = "PC2") %>%
                rename("userId" = "userId") %>%
                mutate(clusterAssignment = clusters) %>%
                mutate(dateClustered = timeNow)
    }            
    
    # compute the coordinates of the cluster centroids in the space spanned by the pca projection
    pcaCentroids <-  GetProjectedCentroidCoord(clusters, pcaRes) %>%
                        dplyr::select("PC1", "PC2", "cluster") %>%
                        rename("PCA1" = "PC1")  %>%
                        rename("PCA2" = "PC2") %>%
                        rename("clusterAssignment" = "cluster") %>%
                        mutate(dateClustered = timeNow) %>%
                        mutate(date = NA)
    
    if (type == 'act') { # some column renames according to miniapp req
        pcaCentroids <- pcaCentroids %>% mutate(patientId = "centroid")
    } else {
        pcaCentroids <- pcaCentroids %>% mutate(userId = "centroid")
    }
    
    pcaCentroids <- pcaCentroids[names(pcaProj)] # reorder columns
    
    # bind the rows together
    pcaProjWithCentroid <- rbind(pcaProj, pcaCentroids)
    
    xap.db.writeframe(pcaProjWithCentroid, paste(title, "pca_with_centroids_viz", sep = "_"))
    xap.db.writeframe(clusterCentroids, paste(title, "centroids_viz", sep = "_"))
}
