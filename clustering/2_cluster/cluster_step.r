################################################################################
# cluster_step.R
#
# This script is used as the implementation for the full workflow of clustering.
# Loads features, performs clustering, then appends cluster results to a table.
#
################################################################################


library(DBI)


source("~/scripts/2_cluster/utils/cluster_step_utils.R")


kConn <- xap.conn
kFeatureTableName <- "act_features_v1"

kClusterConfigPath <- "~/scripts/2_cluster/configs/kmeans.yaml"
kFeatureConfigPath <- "~/scripts/2_cluster/configs/breath_features.yaml"
kClusterTableName <- "act_cluster_output_v1"

writeLines("Fetching features for clustering...\n")
features <- FetchFeatures(kConn, kFeatureTableName, kFeatureConfigPath)

head(features)

writeLines("Running KMeans clustering...", sep = "\n")
clusterOutput <- RunKMeansClustering(features, kClusterConfigPath,
                                     kFeatureConfigPath)

head(clusterOutput)

writeLines(paste("Saving cluster results to table", kClusterTableName), sep = "\n")
SaveClusterResults(kConn, kClusterTableName, clusterOutput)

writeLines("Verifying that cluster results were saved...", sep = "\n")

tryCatch({
  DBI::dbGetQuery(kConn, sprintf("SELECT * FROM %s LIMIT 5", kClusterTableName))   
}, error = function(e) {
  writeLines(paste("Unable to view cluster output. ", e))
})