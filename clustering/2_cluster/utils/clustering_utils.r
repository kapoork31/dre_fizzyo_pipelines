########################################################################
## Clustering Utils
##
## KMeansClustering: Perform K means clustering
## HierarchicalClustering: Perform hierarchical clustering
## DbscanClustering: Perform DBSCAN
## NMFClustering: Perform Non Negative Matrix Factorisation
##
########################################################################

library(dplyr)
library(purrr)
library(ggplot2)
library(rlang)

KMeansClustering <- function(df, nbClusters, runs = FALSE, runsMaxRange = NULL){
  # Performs kMeans clustering on  a given dataset
  #
  # Args:
  #   df: the dataset to perform kMeans clustering on
  #   nbClusters: number of clusters for kMeans
  #   runs: If FALSE, perfom kMeans with a specified number
  #         of clusters. If TRUE, perform multiple kMeans
  #         clustering operation and then estimates the
  #         optimal number of cluster
  #   runsMaxRange: the maximum number of clusters to try
  #                 out when the runs is selected
  #
  #
  # Returns:
  #   clustering object returned by kMeans

  if (!runs){
    kClusters <- kmeans(df, centers = nbClusters)
  } else {

    if (is.null(runsMaxRange)){
      stop("Please provide the maximum krange if you selected the runs functionality")
    }
    kClusters <- kmeansruns(df, krange = 2:runsMaxRange, iter.max = 100, runs = 100)
  }

  return(kClusters)

}


HierarchicalClustering <- function(df, dist = "euclidean"){
  # Perform Hierarchical Clustering on a given dataset
  # and distance metric
  #
  # Args:
  #   df: the dataset to perform hierarchical clustering on
  #   dist: distance metric to be used for hierarchical clustering
  #
  # Returns:
  #   clustering object returned by Hierarchical function in R

  if (dist == "euclidean"){
    distPoints <- dist(df)
    hc <- hclust(distPoints)
  } else {
    warning("other distance metrics are not yet implemented")
  }

  return(hc)
}


DbscanClustering <- function(df, eps = 1, MinPts = 5, scale = TRUE, method = "raw"){
  # Perform DBSCAN Clustering (from fpc package) on a given dataset
  # OUtliers are marked as cluster 0.
  #
  # Args:
  #   df: the dataset to perform DBSCAN clustering on
  #   eps: reachability distance (within this distance from each point)
  #   MinPts:  the minimum number of points required to form a dense region
  #   scale: if TRUE, scale the data
  #   method: "raw"  (takes in raw data and avoids calc. a distance matrix)
  #           "hybrid" (takes in raw data and calc. partial distance matrix)
  #           "dist" (accepts data as a distance matrix)
  #
  # Returns:
  #   clustering object returned by DBSCAN function in R

  clusters <- fpc::dbscan(df, eps = eps, MinPts = MinPts, scale = scale, method = method)

  return(clusters)
}


NMFClustering <- function(df, k = 3){
  # Perform NMF Clustering (from nmf packag) on a given dataset
  #
  # Args:
  #   df: the dataset to perform NMF on
  #   k: number of clusters
  #
  # Returns:
  #   data frame with a single column called "cluster", where each row corresponds
  #   to an observation in df

  # Need to transpose data as it's expected in the format: m features x n observations (!)
  dft <- t(data.matrix(df)) #easier to transpose matrix than data frame
  dft <- data.frame(dft)

  res <- nmf(dft, k)

  # Retrieve weights matrix h
  h <- coef(res)
  h <- data.frame(h)

  # Find the index of the max in each column:
  nmfClusters <- summarise_each(h, (function(x) which(x %in% max(x))))

  nmfClustersT <- t(nmfClusters) # transpose
  colnames(nmfClustersT) <- "cluster" # give the column a name

  return(nmfClustersT)
}
########################################################################
## Clustering Utils
##
## KMeansClustering: Perform K means clustering
## HierarchicalClustering: Perform hierarchical clustering
## DbscanClustering: Perform DBSCAN
## NMFClustering: Perform Non Negative Matrix Factorisation
##
########################################################################

install.packages("fpc")

library(dplyr)
library(purrr)
library(ggplot2)
library(rlang)
library(fpc)

KMeansClustering <- function(df, nbClusters, runs = FALSE, runsMaxRange = NULL){
  # Performs kMeans clustering on  a given dataset
  #
  # Args:
  #   df: the dataset to perform kMeans clustering on
  #   nbClusters: number of clusters for kMeans
  #   runs: If FALSE, perfom kMeans with a specified number
  #         of clusters. If TRUE, perform multiple kMeans
  #         clustering operation and then estimates the
  #         optimal number of cluster
  #   runsMaxRange: the maximum number of clusters to try
  #                 out when the runs is selected
  #
  #
  # Returns:
  #   clustering object returned by kMeans

  df <- select_if(df, is.numeric)
  if (!runs){
    kClusters <- kmeans(df, centers = nbClusters)
  } else {

    if (is.null(runsMaxRange)){
      stop("Please provide the maximum krange if you selected the runs functionality")
    }
    kClusters <- kmeansruns(df, krange = 2:runsMaxRange, iter.max = 100, runs = 100)
  }

  return(kClusters)

}


HierarchicalClustering <- function(df, dist = "euclidean", method = "complete"){
  # Perform Hierarchical Clustering on a given dataset
  # and distance metric
  #
  # Args:
  #   df: the dataset to perform hierarchical clustering on
  #   dist: distance metric to be used for hierarchical clustering
  #
  # Returns:
  #   clustering object returned by Hierarchical function in R
 
  df <- select_if(df, is.numeric) 
  if (dist == "euclidean"){
    distPoints <- dist(df)
    hc <- hclust(distPoints, method = method)
  } else {
    warning("other distance metrics are not yet implemented")
  }

  return(hc)
}


DbscanClustering <- function(df, eps = 1, MinPts = 5, scale = TRUE, method = "raw"){
  # Perform DBSCAN Clustering (from fpc package) on a given dataset
  # OUtliers are marked as cluster 0.
  #
  # Args:
  #   df: the dataset to perform DBSCAN clustering on
  #   eps: reachability distance (within this distance from each point)
  #   MinPts:  the minimum number of points required to form a dense region
  #   scale: if TRUE, scale the data
  #   method: "raw"  (takes in raw data and avoids calc. a distance matrix)
  #           "hybrid" (takes in raw data and calc. partial distance matrix)
  #           "dist" (accepts data as a distance matrix)
  #
  # Returns:
  #   clustering object returned by DBSCAN function in R
  
  df <- select_if(df, is.numeric)
  clusters <- fpc::dbscan(df, eps = eps, MinPts = MinPts, scale = scale, method = method)

  return(clusters)
}


NMFClustering <- function(df, k = 3){
  # Perform NMF Clustering (from nmf packag) on a given dataset
  #
  # Args:
  #   df: the dataset to perform NMF on
  #   k: number of clusters
  #
  # Returns:
  #   data frame with a single column called "cluster", where each row corresponds
  #   to an observation in df
  
  df <- select_if(df, is.numeric)
  
  # Need to transpose data as it's expected in the format: m features x n observations (!)
  dft <- t(data.matrix(df)) #easier to transpose matrix than data frame
  dft <- data.frame(dft)

  res <- nmf(dft, k)

  # Retrieve weights matrix h
  h <- coef(res)
  h <- data.frame(h)

  # Find the index of the max in each column:
  nmfClusters <- summarise_all(h, (function(x) which(x %in% max(x))))

  nmfClustersT <- t(nmfClusters) # transpose
  colnames(nmfClustersT) <- "cluster" # give the column a name

  return(nmfClustersT)
}


ComputeClusterCenters <- function(df, clusters) {
    if (nrow(df) != length(clusters)){
        message = paste("The number of rows in the data:", nrow(df), "is not equal to the length of the cluster vector:", length(clusters))
        stop(message)
    }
    df <- dplyr::select_if(df, is.numeric)
    df$cluster <- clusters
    df %>%
       dplyr::group_by(cluster) %>%
       dplyr::summarise_all(funs(mean)) %>%
       dplyr::ungroup() %>%
       dplyr::select(-cluster) -> meanDF
    
    df %>%
       dplyr::group_by(cluster) %>%
       dplyr::summarise_all(funs(median)) %>%
       dplyr::ungroup() %>%
       dplyr::select(-cluster)-> medianDF   
    
    return(list("mean" = meanDF, "median" = medianDF))   
} 

########################################################################
## General Data Utils
##
## DfSelectNumeric: Selects the numeric columns from a dataframe
##
########################################################################

SelectNumericColumns <- function(df, include.logical = FALSE) {
  # Select numeric column from a dataframe and throws an error if the
  # dataframe contains no numeric columns
  #
  # Args:
  #   df: dataframe
  #   include.logical: bool, if TRUE consider logical columns as numeric
  #                   (defalt FALSE)
  #
  # Returns: a subset of the dataframe containing only numeric values

  numDf <- select_if(df, is.numeric)
  if (include.logical) {
    logDf <- select_if(df, is.logical)
    numDf <- cbind(numDf, logDf)
  }
  if (ncol(numDf) == 0) {
    stop("Error: The dataframe doesn't contain any numerical columns")
  }
  numDf
}
