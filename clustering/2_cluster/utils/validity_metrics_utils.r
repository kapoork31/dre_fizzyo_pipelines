########################################################################
## Internal Validation Utils
##
## SilhouetteValue: Computes the Silhouette Value for a given clustering
## CHIndex: Computes Calinski-Harabasz index for a given clustering
## ComputeClusterStatistics: For each cluster computes the feature wise
##                           mean, median, min, max, sd statistics
########################################################################

library(cluster)
library(dplyr)
library(fpc)
library(factoextra)


SilhouetteValue <- function(clusters, df, distType = "euclidean"){
  # Computes the Silhouette Value for a given clustering
  #
  # Args:
  #   clusters: a nx1 integer vector with k different integer cluster codes
  #             Note that silhouette statistics are only defined if 2<=k<=n
  #   df: a numeric matrix, data frame or "dist" object
  #   distType: the distance measure to be used. This must be one of
  #             "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
  #             Any unambiguous substring can be given.
  #
  #
  # Returns: a list with following components:
  #     si: a nx1 numeric vector of the silhouette width of each data point
  #     clusterAvg: a kx1 numeric vector of cluster-wide averages of silhouettes values
  #     avg: double value, data wide average
  #     plot: ggplot object containing the silhouetted of each cluster

  # restrict to numeric columns
  df <- select_if(df, is.numeric)
  if (ncol(df) == 0) {
    stop("Error: The dataframe doesn't contain any numerical columns")
  }

  sil <- silhouette(clusters, dist(df, distType))
  silSummary <- summary(sil)
  # the 3rd column contains the silhouette widths for each example
  silWidthCol <- 3
  si <- sil[, silWidthCol]
  clusterAvg <- silSummary$clus.avg.widths
  avg <- silSummary$avg.width
  siPlot <- fviz_silhouette(sil, label = FALSE, print.summary = TRUE)
  return(list("si" = si, "clusterAvg" = clusterAvg, "avg" = avg, "plot" = siPlot))
}


CHIndex <- function(clusters, df, distType = "euclidean"){
  # Computes the Calinski-Harabasz index for a given clustering
  #
  # Args:
  #   clusters: a nx1 integer vector with k different integer cluster codes
  #             Note that silhouette statistics are only defined if 2<=k<=n
  #   df: a numeric matrix, data frame or "dist" object
  #   distType: the distance measure to be used. This must be one of
  #             "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
  #             Any unambiguous substring can be given.
  #
  #
  # Returns: float, the Calinski-Harabasz index

  df <- select_if(df, is.numeric)
  if (ncol(df) == 0) {
    stop("Error: The dataframe doesn't contain any numerical columns")
  }
  cluster.stats(d = dist(df, distType), clusters, silhouette = FALSE)$ch
}


ComputeClusterStatistics <- function(clusters, df) {
  # Compute the cluster statistics for a clustering assignment
  #
  # Args:
  #   clusters: integer array of cluster assignments
  #   df: dataframe of observations
  #
  # Returns: a list with the following components:
  #   mean: dataframe of cluster data centers
  #   median: dataframe of cluster data medians
  #   max: dataframe of cluster data max values
  #   min: dataframe of cluster data min values
  #   sd: dataframe of cluster data standard deviation values

  df <- select_if(df, is.numeric)
  if (ncol(df) == 0) {
    stop("Error: The dataframe doesn't contain any numerical columns")
  }

  if (nrow(df) != length(clusters)){
    message <- paste("The number of rows in the data:", nrow(df),
                    "is not equal to the length of the cluster vector:", length(clusters))
    stop(message)
  }

  df$cluster <- clusters

  df %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise_all(list(~ mean(.))) %>%
    dplyr::ungroup() -> meanDF

  df %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise_all(list(~ median(.))) %>%
    dplyr::ungroup() -> medianDF

  df %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise_all(list(~ max(.))) %>%
    dplyr::ungroup() -> maxDF

  df %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise_all(list(~ min(.))) %>%
    dplyr::ungroup() -> minDF

  df %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise_all(list(~ sd(.))) %>%
    dplyr::ungroup() -> sdDF

  return(list("mean" = meanDF,
              "median" = medianDF,
              "max" = maxDF,
              "min" = minDF,
              "sd" = sdDF))
}
