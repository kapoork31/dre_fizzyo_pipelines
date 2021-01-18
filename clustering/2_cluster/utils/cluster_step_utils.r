########################################################################
## Clustering Step Utils
##
## FetchFeatures: Query for selected feature columns on a feature table.
## RunKMeansClustering: Run kmeans clustering on a set of features.
## FetchConfig: Gets the clustering config which specifies details about
##              the cluster.
## SaveClusterResults: Append cluster results to a table.
##
########################################################################

if (!"config" %in% rownames(installed.packages())) install.packages("config")
library(dplyr)
library(dbplyr)
library(DBI)


source("~/scripts/2_cluster/utils/clustering_utils.R")


FetchFeatures <- function(conn, featureTableName, featureConfigPath) {
  # Fetches a dataframe of selected features from the database.
  #
  # Args:
  #   conn <PostgreSQL connection>
  #   featureTableName <chr> feature table name
  #   featureConfigPath <chr> path to feature config

  featureConfig <- FetchConfig(featureConfigPath)

  features <- tryCatch({
                tbl(conn, featureTableName) %>%
                  select_(.dots = featureConfig$features)
              }, error = function(e) {
                stop(paste("Error reading features from table. ", e))
              }) %>% collect()

  features
}


RunKMeansClustering <- function(features, clusterConfigPath, featureConfigPath) {
  # Runs kmeans clustering on a set of features.
  #
  # Args:
  #   features <df> all features
  #   clusterConfigPath <chr> path to cluster config
  #   featureConfigPath <chr> path to feature config

  clusterConfig <- FetchConfig(clusterConfigPath)
  featureConfig <- FetchConfig(featureConfigPath)

  # Remove NAs from features before clustering
  features <- na.omit(features)

  # remove identifiers before clustering
  clusterInput <- select(features, -one_of(featureConfig$ignoreDuringClustering))

  # scale with z-score transform
  if (clusterConfig$scale) {
    clusterInput <- clusterInput %>%
      mutate_if(is.numeric, funs(scale))
  }

  kClusters <- KMeansClustering(clusterInput, clusterConfig$params$numClusters)

  # format cluster output
  clusterOutput <- select(features, one_of(featureConfig$ignoreDuringClustering))
  clusterOutput %>%
    mutate(
      clusterAssignment = kClusters$cluster,
      dateRan = Sys.time()
    )
}


FetchConfig <- function(clusterConfigPath) {
  # Fetches a cluster config.
  #
  # Args:
  #   clusterConfigPath <chr> path to a cluster config

  tryCatch({
    config::get(file = clusterConfigPath, use_parent = FALSE)
  }, error = function(e) {
    stop(paste("Error reading config file. ", e))
  })
}


SaveClusterResults <- function(conn, clusterTableName, clusterOutput) {
  # Saves cluster results to a table.
  #
  # Args:
  #   conn <PostgreSQL connection>
  #   clusterOutput <df> cluster output DataFrame
  #   clusterTableName <chr> cluster table name

  if (!is.null(clusterOutput) & nrow(clusterOutput) > 0) {
    tryCatch({
      DBI::dbWriteTable(conn, clusterTableName, clusterOutput, append = TRUE,
                        row.names = FALSE)
    }, error = function(e) {
      stop(paste("Error writing cluster results to table. ", e))
    })
  }
}
