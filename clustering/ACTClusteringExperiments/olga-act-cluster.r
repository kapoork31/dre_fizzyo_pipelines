# Exclude Start

################################################################################
# act-cluster.R
#
# This script has the experimental flow for ACT data (meant to be run inside DRE). 
# It has the functionality to:
# - viz correlations
# - dim reduction (pca, umap and tsne)
# - analyze the outliers
# - viz clustering 
# - plot individual patients points onto the overall "cloud" of data.
################################################################################

###################################
# Load libraries and source files
###################################
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

source("~/scripts/2_cluster/utils/clustering_utils.R");
source("~/scripts/2_cluster/utils/validity_metrics_utils.R");
source("~/scripts/2_cluster/utils/outliers_utils.R");
#source("~/scripts/2_cluster/utils/dim_reduction_utils.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.r");
source("~/scripts/R_scripts/data_cleaning_utils.R");

kActFeatures1 <- "breath_test_all_data_1201_feat_v2"
kActFeatures2 <- "breath_test_all_data_0203_feat_v2"
kActFeatures3 <- "breath_test_all_data_04_feat_v2"


actDB <- xap.conn %>% tbl(kActFeatures)
act <- actDB %>% collect()

# Remove identifiers to cluster all days
df <- act %>%
  select(-patientId, -patient_record_id, -treatmentId, -actDate)

# Check for any NAs and Not finite values
AnyNAs(df)
AnyNotFinite(df)

naCount <- DisplayNACountPerColumn(df)
naCount[naCount > 0]

nfCount <- DisplayNotFiniteCountPerColumn(df)
nfCount[nfCount > 0]

# Replace NAs by 0 for now
cleanDF <- df %>% replace(., is.na(.), 0)

AnyNAs(cleanDF)
AnyNotFinite(cleanDF)

###################################################################################################
# Correlations
###################################################################################################

ComputeCorr <- function (cleanDF){
   corrMat <- cor(cleanDF)

   # compute the p-value of correlations
   cor.mtest <- function(mat, ...) {
      mat <- as.matrix(mat)
      n <- ncol(mat)
      p.mat<- matrix(NA, n, n)
      diag(p.mat) <- 0
      for (i in 1:(n - 1)) {
         for (j in (i + 1):n) {
               tmp <- cor.test(mat[, i], mat[, j], ...)
               p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
         }
      }

   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
   p.mat
   }
   # matrix of the p-value of the correlation
   p.mat <- cor.mtest(cleanDF)

   col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
   corrplot(corrMat, method="color", col=col(200),  
            type="upper", order="hclust", 
            #addCoef.col = "black", # Add coefficient of correlation
            tl.col="black", tl.srt=45, tl.cex = 0.5, #Text label color and rotation
            # Combine with significance
            p.mat = p.mat, sig.level = 0.01, insig = "blank", 
            # hide correlation coefficient on the principal diagonal
            diag=FALSE 
            ) 

}

ComputeCorr <- function (cleanDF)

# Columns for clustering
clusterCols <- c("patientId", "breathsDayAdherenceScore", "treatmentWeekAdherenceScore" , "meanBreathAmplitude", "breathCount")

actDB1 <- xap.conn %>% tbl(kActFeatures1)
act1 <- actDB1 %>% collect()
actDB2 <- xap.conn %>% tbl(kActFeatures2)
act2 <- actDB2 %>% collect()

act <- rbind(act1,act2)

# Remove identifiers to cluster all days (leave patient_record_id)
# Replace NAs by 0
cleanDF  <- act %>%
  select(-actDate, -treatmentId) %>%
  replace(., is.na(.), 0) 

clusterDF <- cleanDF

clusterDF <-  clusterDF %>% select(clusterCols)
#head(clusterDF)
#dim(clusterDF)
str(clusterDF)
nrow(clusterDF)

###################################################################################################
# Outliers
###################################################################################################
visualize = FALSE
dfOutliers <- clusterDF
   #DetectOutliers(dfOutliers, breathsDayAdherenceScore, "breathsDayAdherenceScore", visualize = visualize)
   #DetectOutliers(dfOutliers, treatmentWeekAdherenceScore, "treatmentWeekAdherenceScore", visualize = visualize)
   #DetectOutliers(dfOutliers, meanBreathAmplitude, "meanBreathAmplitude", visualize = visualize)
   #DetectOutliers(dfOutliers, breathCount, "breathCount", visualize = visualize)

   cat("Remove the outliers \n")
   DetectOutliers(dfOutliers, breathsDayAdherenceScore, "breathsDayAdherenceScore", shouldRemove = TRUE)
   cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
   dfOutliers <- dfOutliers %>% drop_na
   cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))
   DetectOutliers(dfOutliers, breathsDayAdherenceScore, "breathsDayAdherenceScore (after the 1st drop of the outliers)")

   #DetectOutliers(dfOutliers, breathCount, "breathCount", shouldRemove = TRUE)
   #cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
   #dfOutliers <- dfOutliers %>% drop_na
   #cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))
   #DetectOutliers(dfOutliers, breathCount, "breathCount (after the 2nd drop of the outliers")

   #DetectOutliers(dfOutliers, meanBreathAmplitude, "meanBreathAmplitude",  shouldRemove = TRUE)
   #cat(sprintf("Current N of rows: %s \n", nrow(dfOutliers)))
   #dfOutliers <- dfOutliers %>% drop_na
   #cat(sprintf("After dropping NAs,  N of rows: %s \n", nrow(dfOutliers)))
   #DetectOutliers(dfOutliers, meanBreathAmplitude, "meanBreathAmplitude (after the 2nd drop of the outliers")
   
clusterDF <- dfOutliers
str(clusterDF)

# For now, replace missing values by column average
# Then scale with the z-score transform
unscaledDF <- clusterDF %>%
            mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
            mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
            select(-patientId) %>%
            replace_na(as.list(colMeans(., na.rm=T)))
numDF <- unscaledDF %>% mutate_if(is.numeric, funs(scale))
str(numDF)
# Run dimensionality reduction of the scaled
# dataset (both PCA and UMAP) and plot the results
pca <- PcaRunPlot(numDF)
#pca$plot

umapScaled <- UmapRunPlot(numDF)
#umapScaled$plot

###################################################################################################
# TSNE
###################################################################################################

TsneRun <- function(df, table_pref = "olga_tsne623_%s", perpexities = c(2, 10, 20, 30, 50, 100)) {
  tsne <- TsneMultiplePerplexitiesRuns(df, perpexities)
  for (p in perpexities) {
    tableName <- sprintf(table_pref, p)
    resIdx <- toString(p)
    cat(sprintf("Saving tsne result for perplexity %s in the table %s \n", resIdx, tableName))
    dbWriteTable(xap.conn, tableName, tsne$results[[resIdx]],
                     row.names = FALSE, append = TRUE)
    }
  tsne
}

PlotTsneRuns<- function() {
  graphs <- list()
  legend <- NULL
  tsneRes2 <- xap.read_table("olga_tsne2_2")
  title <- paste("tsne - Perplexity", 2, sep = " ")
  p2 <- DimReductionScatterPlot(tsneRes2, "X1", "X2", legend = legend, title = title)
  graphs[[1]] <- p2

  tsneRes10 <- xap.read_table("olga_tsne2_10")
  title <- paste("tsne - Perplexity", 10, sep = " ")
  p10 <- DimReductionScatterPlot(tsneRes10, "X1", "X2", legend = legend, title = title)
  graphs[[2]] <- p10

  tsneRes20 <- xap.read_table("olga_tsne2_20")
  title <- paste("tsne - Perplexity", 20, sep = " ")
  p20 <- DimReductionScatterPlot(tsneRes20, "X1", "X2", legend = legend, title = title)
  graphs[[3]] <- p20

  tsneRes30 <- xap.read_table("olga_tsne2_30")
  title <- paste("tsne - Perplexity", 30, sep = " ")
  p30 <- DimReductionScatterPlot(tsneRes30, "X1", "X2", legend = legend, title = title)
  graphs[[4]] <- p30

  tsneRes50 <- xap.read_table("olga_tsne2_50")
  title <- paste("tsne - Perplexity", 50, sep = " ")
  p50 <- DimReductionScatterPlot(tsneRes50, "X1", "X2", legend = legend, title = title)
  graphs[[5]] <- p50

  tsneRes100 <- xap.read_table("olga_tsne2_100")
  title <- paste("tsne - Perplexity", 100, sep = " ")
  p100 <- DimReductionScatterPlot(tsneRes100, "X1", "X2", legend = legend, title = title)
  graphs[[6]] <- p100

  plot_grid(plotlist = graphs, ncol = 2)
}

perp <- c(30, 50, 100)
table_pref <- "olga_tsne623_%s"
#tsneRes <- TsneRun(numDF, table_pref, perp)
#tsneRes$plot

p <- 30
#tableName <- sprintf(table_pref, p)
#tsneRes <- xap.read_table(tableName)

###################################################################################################
# Clustering
###################################################################################################

set.seed(41)
sizePlot <- 0.8
alphaPlot <- 0.2
kClusters <- KMeansClustering(numDF, runs = TRUE, runsMaxRange = 7)

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


# Add the cluster label back to the dataframe.
unscaledDF$cluster <- kClusters$cluster

# Run hierarchical clustering and show the clusters on pca, tsne and umap

# Run HC with the default euclidean distance and plot the tree.
hc <- HierarchicalClustering(numDF)
plot(hc)

# Choose the number of clusters based on the tree.
nbClusters <- 5
clusters <- cutree(hc, k = nbClusters)

hcpca <- DimReductionScatterPlot(pca$results,
                                "PC1",
                                "PC2",
                                as.character(clusters),
                                title = "PCA - hc")
hcumap <- DimReductionScatterPlot(umapScaled$results,
                                  "X1",
                                  "X2",
                                  as.character(clusters),
                                  title ="umap - hc")
                                  
                                  
hctsne <- DimReductionScatterPlot(tsneRes,
                                  "X1",
                                  "X2",
                                  as.character(clusters),
                                  title ="tsne 30 - hc")
plot_grid(hcpca, hcumap, hctsne)

# Run DBSCAN clustering and show the clusters on pca and umap.
db <- DbscanClustering(numDF, eps=0.6, MinPts=10, scale=TRUE)

# Get the cluster labels.
dclusters <- db$cluster

pcaPlot <- DimReductionScatterPlot(pca$results,
                                "PC1",
                                "PC2",
                                as.character(dclusters),
                                title = paste("DBSCAN PCA:", db$eps))
umapPlot <- DimReductionScatterPlot(umapScaled$results,
                                  "X1",
                                  "X2",
                                  as.character(dclusters),
                                  title = paste("DBSCAN umap:", db$eps))
                                  
tsnePlot <- DimReductionScatterPlot(tsneRes,
                                  "X1",
                                  "X2",
                                  as.character(dclusters),
                                  title = paste("DBSCAN tsne 30:", db$eps))                                  
plot_grid(pcaPlot, umapPlot, tsnePlot)


OnePatientCluster <- function(clusterDF, pca, umapScaled)
{
   # List of patient IDs (stored on DRE as CSV), hand-selected.
  kPatientFile <- "~/documents/interesting_patients_ACT.csv"
  patients <- read.csv(file = kPatientFile, header = FALSE, sep = ",", stringsAsFactors = FALSE)
  patients <- patients[, 1]
  str(clusterDF)
  patients_inter <- intersect(unique(clusterDF$patient_record_id), patients) 

  # Pick a particular patient  for te list.
  id <- 1
  #while (id < 9) {
  
     UID <- patients_inter[id]

     alpha <- clusterDF$patient_record_id == UID
     alpha <- (19*alpha +1)/20
     upca <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                            as.character(clusterDF$patientId == UID), 
                            alpha = alpha,
                            title = paste("PCA projection: functional test user", id)) +
                            scale_color_brewer(palette="Set1")
                            
  
     uumap <- DimReductionScatterPlot(umapScaled$results, "X1", "X2", 
                             as.character(clusterDF$patientId == UID),
                             alpha = alpha,
                             title = paste("Umap projection: functional test user", id)) +
                             scale_color_brewer(palette="Set1")
     plot_grid(upca, uumap)
     
    # id <- id + 1
  #}
}

OnePatientCluster(clusterDF, pca, umapScaled)

# Exclude End