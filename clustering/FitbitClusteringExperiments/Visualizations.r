#############################################################
# Load libraries and source files
#############################################################
install.packages("corrplot")
install.packages("DMwR")

library(dplyr)
library(lubridate)
library(ggplot2)
library(purrr)
library(DBI)
library(tidyr)
library(corrplot)
library(Hmisc)
library(DMwR)
library(rlang)
library(fpc)
library(cluster)

source("~/scripts/2_cluster/utils/outliers_utils.R");
source("~/scripts/2_cluster/utils/clustering_utils.R");
source("~/scripts/2_cluster/utils/clustering_tendency_utils.R");
source("~/scripts/2_cluster/utils/validity_metrics_utils.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.R");
source("~/scripts/R_scripts/data_cleaning_utils.R");
source("~/scripts/R_scripts/data_distribution_utils.R");
source("~/scripts/bianca_experiments/fitbit_utils.R");

###############################################################
# Load the datasets
###############################################################
kFitbitFeaturesv3 <- "fitbit_features_v3"
fitbitDB <- xap.conn %>% tbl(kFitbitFeaturesv3)
fitbit <- fitbitDB %>% collect()

dim(fitbit)

AnyNAs(fitbit)
AnyNotFinite(fitbit)
DisplayNACountPerColumn(fitbit)
DisplayNotFiniteCountPerColumn(fitbit) -> infs
DisplayNaNCountPerColumn(fitbit) -> nans

###############################################################
# Wear Percent
###############################################################

hist(fitbit$wearPercent)
hist(fitbit$wearQ1Percent)
hist(fitbit$wearQ2Percent)
hist(fitbit$wearQ3Percent)
hist(fitbit$wearQ4Percent)

fitbit %>%
    mutate(wp = rowMeans(cbind(wearQ2Percent, wearQ3Percent, wearQ4Percent))) %>%
    filter(wp >= 40) -> df


# columns for clustering
footstepsCols <-   c("activeHours", "activeMinsSteps0", "activeMinsSteps10",
                    "activeMinsSteps2", "activeMinsSteps20", "activeMinsSteps5",
                    "stepCount", "stepCountQ2", "stepCountQ3", "stepCountQ4",
                    "CoefficientOfVariationStep", "CoefficientOfVariationStepQ1", 
                    "CoefficientOfVariationStepQ2", "CoefficientOfVariationStepQ3", "CoefficientOfVariationStepQ4",
                    "lowActiveHours", "meanHourlyStepsDay", "stepCountNorm",
                    "stepCountNormQ2", "stepCountNormQ3", "stepCountNormQ4", "stepNormModerate", "stepNormVigorous")

fildf <- df[footstepsCols]
head(fildf)
dim(fildf)

# for now, replace all of those with 0
fildf %>%
    mutate_all(funs(if_else(is.na(.), 0, .))) %>%
    mutate_all(funs(if_else(!is.finite(.), 0, .))) -> fildf

AnyNAs(fildf)
AnyNotFinite(fildf)

###########################################################
# Correlation and data distribution analysis
###########################################################

CheckDatasetDistribution(fildf)  

cormatrix <- rcorr(as.matrix(fildf), type = "pearson")

pearsonPlot <- corrplot(cormatrix$r, type="upper", order="hclust", 
                        main = "Pearson Correlation Matrix for Footstep data", tl.srt=45, tl.cex = 0.8,
                        p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
         
         
cormatrix <- rcorr(as.matrix(fildf), type = "spearman")

spearmanPlot <- corrplot(cormatrix$r, type="upper", order="hclust",
                         main = "Spearman Correlation Matrix for Footstep data", tl.srt=45, tl.cex = 0.8,
                         p.mat = cormatrix$P, sig.level = 0.01, insig = "blank")
                         
######################################################
# Dimensionality reduction
######################################################
clusterCols <- c("activeMinsSteps5", "CoefficientOfVariationStep",
	           "lowActiveHours", "meanHourlyStepsDay", "stepCountNorm",
                "stepNormVigorous")
                    
clusterDf <- df[clusterCols]

clusterDf %>%
    mutate_all(funs(if_else(is.na(.), 0, .))) %>%
    mutate_all(funs(if_else(!is.finite(.), 0, .))) -> unscaledClusterDf

# scale each column using z-score transform
# and then check the distribution
clusterDf <- unscaledClusterDf %>%
            mutate_if(is.numeric, funs(scale)) %>%
            mutate(userid = df$userid) %>%
            mutate(date = df$date)


# run dimensionality reduction of the scaled
# dataset (both PCA and UMAP) and plot the results
pca <- PcaRunPlot(clusterDf)
umapScaled <- UmapRunPlot(clusterDf)
plot_grid(pca$plot, umapScaled$plot)

# run the PCA detailed plot
multiPlot <- PcaDetailedPlot(pca$obj)
plot_grid(multiPlot$explainedVariance, multiPlot$cos2Circle, multiPlot$cos2Map, multiPlot$cos2Bar) 
plot_grid(multiPlot$explainedVariance, multiPlot$contribCircle, multiPlot$contribMap, multiPlot$contribBar)

##########################################################
# Assessing Clustering tendencies
##########################################################
diss <- DissimilarityPlot(clusterDf, nmax=250, title="Scaled data", method="euclidean", hopkins=TRUE)
dissPca <- DissimilarityPlot(pca$results, nmax = 250, title = "PCA components", hopkins=TRUE)
dissUmap <- DissimilarityPlot(umapScaled$results, nmax = 250, title = "Umap components", hopkins=TRUE)
plot_grid(diss, dissPca, dissUmap)

##########################################################
# K-Means Clustering
##########################################################
# run kmeans clustering and show the clusters on pca and umap
kClusters <- KMeansClustering(clusterDf, nbClusters = 3)

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

kpca <- OverlayProjectionPoints(kpcaTF, pcaKCenters, "PC1", "PC2", title="K-Means Centroids on PCA projection", color="black", size=1, shape=8)
kumap <- OverlayProjectionPoints(kumapFF, umapKCenters, "X1", "X2", color="black", title="K-Means Centroids on UMAP projection", size=1, shape=8)

print(paste("CH Index for KMeans is", CHIndex(kClusters$cluster, clusterDf)))
silMetrics <- SilhouetteValue(kClusters$cluster, clusterDf)
silPlot <- silMetrics$plot
plot_grid(kpca, kumap, silPlot)


##################################################################
# Add patient trajectory
##################################################################
# these are two patients labeled as having interesting activity by 
# the clinical team
funcUID1 <- "4c36b8d7-994e-422d-be79-abab3e5fd4b0"
funcUID2 <- "ec651a09-a7b9-4661-8d62-cf3dfc68d4d7"

uPcaDf1 <- pca$results %>% filter(userid == funcUID1) 
uPcaDf2 <- pca$results %>% filter(userid == funcUID2)

uUmapDf1 <- umapScaled$results %>% filter(userid == funcUID1) 
uUmapDf2 <- umapScaled$results %>% filter(userid == funcUID2)

upca1 <- OverlayProjectionPoints(kpcaTF, uPcaDf1, "PC1", "PC2", title = 'Functional user 1', size=1, alpha = 0.5)
upca2 <- OverlayProjectionPoints(kpcaTF, uPcaDf2, "PC1", "PC2", title = 'Functional user 2', size=1, alpha = 0.5)  

uumap1 <- OverlayProjectionPoints(kumapFF, uUmapDf1, "X1", "X2", title = 'Functional user 1', size=1, alpha = 0.5) 
uumap2 <- OverlayProjectionPoints(kumapFF, uUmapDf2, "X1", "X2", title = 'Functional user 2', size=1, alpha = 0.5) 

plot_grid(upca1, uumap1)
plot_grid(upca2, uumap2)

####################################################################
# Add gold label set for validation
####################################################################
# this are the days that we are fairly confident to be high activity
# because they contain at least one period of prelonged activity
# idealy these points should cluster in the areas of perceived high activity
# in such way this vizualization can be used as an empirical validation of clustering results

filterExprStr = "activeMinsStep20 > 20" # <- replace this with any other expression of interest
filterExpr <- rlang::parse_expr(filterExprStr)
df %>% filter(!!filterExpr) %>% select(userid, date) -> activeDays
activePcaDf <- inner_join(pca$results, activeDays, by=c("userid", "date"))
activeUmapDf <- inner_join(umapScaled$results, activeDays, by=c("userid", "date"))

activePca <- OverlayProjectionPoints(kpcaTF, activePcaDf, "PC1", "PC2", title=paste(filterExprStr,"- PCA projection"), size=0.6, alpha=0.4)
activeUmap <- OverlayProjectionPoints(kumapFF, activeUmapDf, "X1", "X2", title=paste(filterExprStr, "- UMAP projection"), size=0.6, alpha=0.4)
plot_grid(activePca, activeUmap) 

# compare with the unscaled cluster centers
unscale(kClusters$centers, scale(unscaledClusterDf))

##############################################################################################################################



# ---------------- The rest of this script is still work in progress ---------------------
###############################################################################################################################

# run hierarchical clustering and show the clusters on pca and umap

# run HC with the default euclidean distance and plot the tree
hc <- HierarchicalClustering(clusterDf, method = "ward.D")
plot(hc)

# choose the number of clusters based on the tree
nbClusters <- 6
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
                                  title = "umap - hc")

silMetrics <- SilhouetteValue(clusters, clusterDf)
silPlot <- silMetrics$plot
plot_grid(hcpca, hcumap, silPlot)
print(paste("CH Index for HC is", CHIndex(clusters, clusterDf)))

#####################
### DBSCAN
#####################
dbs <- DbscanClustering(clusterDf, eps=0.7, MinPts=10, scale=FALSE)

dbspca <- DimReductionScatterPlot(pca$results,
                                "PC1",
                                "PC2",
                                as.character(dbs$cluster),
                                title = "PCA - dbscan")
dbsumap <- DimReductionScatterPlot(umapScaled$results,
                                  "X1",
                                  "X2",
                                  as.character(dbs$cluster),
                                  title = "umap - dbscan")

silMetrics <- SilhouetteValue(dbs$cluster, clusterDf)
silPlot <- silMetrics$plot
plot_grid(dbspca, dbsumap, silPlot)
print(paste("CH Index for DBSCAN is", CHIndex(dbs$cluster, clusterDf)))
 

##################################################################
# try without outliers

dfOutliers <- unscaledClusterDf

DetectOutliers(dfOutliers, "activeHours", clusterCols[1], visualize = FALSE)
#################################################################
# other features