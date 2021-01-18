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
library(DMwR)
library(corrplot)

source("~/scripts/2_cluster/utils/clustering_utils.R");
source("~/scripts/FitbitClusteringExperiments/new_dim_reduction_utils.r");
source("~/scripts/R_scripts/data_cleaning_utils.R");
source("~/scripts/2_cluster/utils/outliers_utils.R");
source("~/scripts/2_cluster/utils/data_distribution_utils.R");

###################################
# Define all constants to modify
###################################

kFitbitFeatures <- "fitbit_features_v3"

###################################
# load the datasets
###################################

fitbitDB <- xap.conn %>% tbl(kFitbitFeatures)

fitbit <- fitbitDB %>% collect()

# remove idetifiers to cluster all days
df <- fitbit %>%
  select(-userid, -date)

# remove non-numeric features
df <- df %>% select(-startWindow, -endWindow)

# only keep the data if wear percent is above 40%
df <- df %>% filter(wearPercent >= 40)
paste("Initial dataset contains", nrow(fitbit), 'rows, the filtered dataset has' , nrow(df))

# check for any NAs and Not finite values
AnyNAs(df)
AnyNotFinite(df)

naCount <- DisplayNACountPerColumn(df)
naCount[naCount > 0]

nfCount <- DisplayNotFiniteCountPerColumn(df)
nfCount[nfCount > 0]

# replace NAs by column mean
cleanScaledDF <- df %>% 
            mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
            mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
            replace_na(as.list(colMeans(df, na.rm=T))) %>%
            mutate_if(is.numeric, funs(scale))

#################################
# Compute correlation matrix
#################################

# # compute correlation matrix
# corrMat <- cor(cleanScaledDF)

# # compute the p-value of correlations
# cor.mtest <- function(mat, ...) {
#     mat <- as.matrix(mat)
#     n <- ncol(mat)
#     p.mat<- matrix(NA, n, n)
#     diag(p.mat) <- 0
#     for (i in 1:(n - 1)) {
#         for (j in (i + 1):n) {
#             tmp <- cor.test(mat[, i], mat[, j], ...)
#             p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
#         }
#     }
#   colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
#   p.mat
# }
# # matrix of the p-value of the correlation
# p.mat <- cor.mtest(cleanScaledDF)

# col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
# corrplot(corrMat, method="color", col=col(200),  
#          type="upper", order="hclust", 
#          #addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, tl.cex = 0.5, #Text label color and rotation
#          # Combine with significance
#          p.mat = p.mat, sig.level = 0.01, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE 
#          ) 

# # restrict the correlation matrix to one user
# UID <- "ef69a529-cdac-4234-bb7a-3bff828f78b9"
# userDF <- fitbit %>% 
#             filter(userid==UID) %>% 
#             select(-userid, -date, -startWindow, -endWindow, -activeMinsHrSteps20) %>% 
#             mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
#             mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
#             replace_na(as.list(colMeans(., na.rm=T)))

# # compute correlation matrix
# userCorrMat <- cor(userDF)
# # matrix of the p-value of the correlation
# userp.mat <- cor.mtest(userDF)
# # plot the correlation matrix
# corrplot(userCorrMat, method="color", col=col(200),  
#          type="upper", order="hclust", 
#          #addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, tl.cex = 0.5, #Text label color and rotation
#          # Combine with significance
#          p.mat = userp.mat, sig.level = 0.01, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE 
#          ) 

# # restrict to one day
# monday <- lubridate::make_datetime(2019, 4, 14)
# monDF <- fitbit %>% 
#             filter(date==monday) %>% 
#             select(-userid, -date, -startWindow, -endWindow, -activeMinsSteps20, -activeMinsHrSteps20) %>% 
#             mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
#             mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
#             replace_na(as.list(colMeans(., na.rm=T)))

# # compute correlation matrix
# monCorrMat <- cor(monDF)
# # matrix of the p-value of the correlation
# monp.mat <- cor.mtest(monDF)
# # plot the correlation matrix
# corrplot(monCorrMat, method="color", col=col(200),  
#          type="upper", order="hclust", 
#          #addCoef.col = "black", # Add coefficient of correlation
#          tl.col="black", tl.srt=45, tl.cex = 0.5, #Text label color and rotation
#          # Combine with significance
#          p.mat = monp.mat, sig.level = 0.01, insig = "blank", 
#          # hide correlation coefficient on the principal diagonal
#          diag=FALSE 
#          ) 

# # Visual analysis of the full clean dataset
# pca <- PcaRunPlot(cleanScaledDF)
# umap <- UmapRunPlot(cleanScaledDF)

# plot_grid(pca$plot, umap$plot)
# summary(pca)
        
# # Find the super correlated variables
# features <- names(cleanScaledDF)
# nFeatures <- length(features)
# for (idx1 in seq(1, nFeatures)) {
#     for (idx2 in seq(idx1+1, nFeatures)){
#         feat1 <- features[idx1]
#         feat2 <- features[idx2]
#         if (idx2 != nFeatures + 1) {
#             if(abs(corrMat[feat1, feat2]) >= 0.85) {
#                 print(paste(feat1, feat2, corrMat[feat1, feat2]))
#             }
#         }
#     }
# }
##############################################################################
# Clustering
##############################################################################

# columns for clustering
clusterCols <-  c("userid", "date", 
                "stepCountNorm", "stepCountNormQ2", "stepCountNormQ3", "stepCountNormQ4", "stepNormModerate", "stepNormVigorous", "meanHourlyStepsDay",
                "activeHours", "lowActiveHours", "activeMinsSteps5", "activeMinsHr5",  "activeMinsHrSteps5",  "activeMinsSteps10", "activeMinsHr20",  "activeMinsHrSteps10",
                "switch100", "switch120", "switch140", "restingHrProxy",
                "CoefficientOfVariationStepQ1", "CoefficientOfVariationStepQ2", "CoefficientOfVariationStepQ3", "CoefficientOfVariationStepQ4",
                "CoefficientOfVariationHrQ1", "CoefficientOfVariationHrQ2", "CoefficientOfVariationHrQ3", "CoefficientOfVariationHrQ4"
                )

# limit to HR data                
clusterCols <-  c("userid", "date", 
                #"stepCountNorm", "stepCountNormQ2", "stepCountNormQ3", "stepCountNormQ4", "stepNormModerate", "stepNormVigorous", "meanHourlyStepsDay",
                #"activeHours", "lowActiveHours", "activeMinsSteps5", "activeMinsSteps10", 
                #"activeMinsHrSteps5", "activeMinsHrSteps10",
                "activeMinsHr5", "activeMinsHr20",
                "switch100", "switch120", "switch140", "restingHrProxy",
                #"CoefficientOfVariationStepQ1", "CoefficientOfVariationStepQ2", "CoefficientOfVariationStepQ3", "CoefficientOfVariationStepQ4",
                #"CoefficientOfVariationHrQ1", <- remove because of lots of NAs
                "CoefficientOfVariationHrQ2", "CoefficientOfVariationHrQ3", "CoefficientOfVariationHrQ4"
                )                

clusterDF <- fitbit  %>% 
            filter(wearPercent >= 40) %>%
            select(clusterCols)
head(clusterDF)
dim(clusterDF)
CheckDatasetDistribution(select(clusterDF, -userid, -date))  


# for now, replace missing values by column average
# then scale with the z-score transform
unscaledClusterDF <- clusterDF %>%
            mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
            mutate_if(is.numeric, list(~na_if(., -Inf))) %>% 
            select(-date, -userid) %>%
            replace_na(as.list(colMeans(., na.rm=T)))

# check the data distribution
CheckDatasetDistribution(unscaledClusterDF)            
            
scaledClusterDF <- unscaledClusterDF %>% mutate_if(is.numeric, funs(scale))

# run dimensionality reduction of the scaled
# dataset (both PCA and UMAP) and plot the results
pca <- PcaRunPlot(scaledClusterDF)
umap <- UmapRunPlot(scaledClusterDF)
plot_grid(pca$plot, umap$plot)

# run kmeans clustering and show the clusters on pca and umap

#kClusters <- KMeansClustering(scaledClusterDF, runs = TRUE, runsMaxRange = 10)
kClusters <- kmeans(scaledClusterDF, centers=3, nstart = 20, algorithm = 'Lloyd', iter.max=30)

kpca <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                                as.character(kClusters$cluster), title = "PCA - kmeans")
kumap <- DimReductionScatterPlot(umap$results, "X1", "X2", as.character(kClusters$cluster),
                                 title = "Umap - kmeans")

plot_grid(kpca, kumap)

# print cluster centers and unscale them for the numbers to have meaning
unscale(kClusters$centers, scale(unscaledClusterDF))

# add the cluster label back to the datagrame
unscaledClusterDF$cluster <- kClusters$cluster
featVars <- unscaledClusterDF %>% 
                    group_by(cluster) %>%
                    summarize_if(is.numeric, funs(sd)) %>%
                    ungroup()

# run hierarchical clustering and show the clusters on pca, tsne and umap

# run HC with the default euclidean distance and plot the tree
hc <- HierarchicalClustering(scaledClusterDF)
plot(hc)

# choose the number of clusters based on the tree
nbClusters <- 3
clusters <- cutree(hc, k = nbClusters)

hcpca <- DimReductionScatterPlot(pca$results,
                                "PC1",
                                "PC2",
                                as.character(clusters),
                                title = "PCA - hc")
hcumap <- DimReductionScatterPlot(umap$results,
                                  "X1",
                                  "X2",
                                  as.character(clusters),
                                  title = "Umap - hc")

plot_grid(hcpca, hcumap)

# run DBSCAN clustering and show the clusters on pca and umap

db <- DbscanClustering(scaledClusterDF, eps=3, MinPts=10, scale=TRUE)

# get the cluster labels
dclusters <- db$cluster
# set the noise output as outliers
unscaledClusterDF$validPoint <- dclusters

dbpca <- DimReductionScatterPlot(pca$results,
                                "PC1",
                                "PC2",
                                as.character(dclusters),
                                title = paste("PCA:", db$eps))
dbumap <- DimReductionScatterPlot(umap$results,
                                  "X1",
                                  "X2",
                                  as.character(dclusters),
                                  title = paste("Umap:", db$eps))
plot_grid(dbpca, dbumap)

# remove the outliers
validUnscaledDF <- unscaledClusterDF %>%
                    filter(validPoint != 0) %>%
                    select(-cluster, -validPoint) %>%
                    replace_na(as.list(colMeans(., na.rm=T)))

validScaledDF <- validUnscaledDF %>% mutate_if(is.numeric, funs(scale))
                    
pca <- PcaRunPlot(validScaledDF)
umap <- UmapRunPlot(validScaledDF)

plot_grid(pca$plot, umap$plot)                    

#########
#filter for one user id
#########
funcUID1 <- "4c36b8d7-994e-422d-be79-abab3e5fd4b0"
funcUID2 <- "ec651a09-a7b9-4661-8d62-cf3dfc68d4d7"
UID <- funcUID1
id <- 1
alpha <- clusterDF$userid == UID
alpha <- (19*alpha +1)/20
upca <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                            as.character(clusterDF$userid == UID), 
                            alpha = alpha,
                            title = paste("PCA projection: functional test user", id))
        
uumap <- DimReductionScatterPlot(umap$results, "X1", "X2", 
                             as.character(clusterDF$userid == UID),
                             alpha = alpha,
                             title = paste("Umap projection: functional test user", id))

plot_grid(upca, uumap)

UID <- funcUID2
id <- 2
alpha <- clusterDF$userid == UID
alpha <- (19*alpha +1)/20
upca <- DimReductionScatterPlot(pca$results, "PC1", "PC2",
                            as.character(clusterDF$userid == UID), 
                            alpha = alpha,
                            title = paste("PCA projection: functional test user", id))
        
uumap <- DimReductionScatterPlot(umap$results, "X1", "X2", 
                             as.character(clusterDF$userid == UID),
                             alpha = alpha,
                             title = paste("Umap projection: functional test user", id))

plot_grid(upca, uumap)    
