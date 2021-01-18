####################################################################
## Dimensionality Reduction Utils
##
## DimReductionScatterPlot: Scatter plot for a 2-d dataset
## TsneRunPlot: TSNE for a given perplexity and plots
## TsneMultiplePerplexitiesRuns: Run TSNE for multiple perplexities
## PcaRunPlot: PCA on normalised data and plots
## UmapRunPlot: UMAP and plots
##
#####################################################################

install.packages("cowplot")
install.packages("tsne")
install.packages("umap")
install.packages("factoextra")
install.packages("ggcorrplot")

library(factoextra)
library(dplyr)
library(ggplot2)
library(rlang)
library(cowplot)
library(tsne)
library(umap)
library(ggcorrplot)

DimReductionScatterPlot <- function(df, colx, coly, legend = NULL, alpha = NULL, size = 0.4, 
                                    xLim = NULL,  yLim = NULL, include.chull=FALSE, include.ellipse=FALSE,
                                    title = "Dimensionality Reduction Plot"){
  # Creates a scatter plot for the reduced dataset
  #
  # Args:
  #   df: the dataset to plot from
  #   colx: name of the column to be displayed on the x axis
  #   coly: name of the column to be displayed on the y axis
  #   legend: if NULL the colour of the points are blue, if
  #           not NULL colour/legend of the points when displayed
  #   title: title to display (default given)
  #
  # Returns:
  #   ggplot object for the scatter plot

  xcol <- rlang::parse_expr(colx)
  ycol <- rlang::parse_expr(coly)
  if (is.null(alpha)) {
      alpha = 0.1
  }
  if (is.null(legend)){
      p <- ggplot(df, aes(x = !!xcol, y = !!ycol)) +
            geom_point(color = "blue", alpha = alpha, size = size) 
  } else {
      df <- df %>% mutate(clust = legend)
      p <- ggplot(data=NULL, aes(x = !!xcol, y = !!ycol)) +
           geom_point(data = df, aes(color = factor(clust)), alpha = alpha, size = size)
      if (include.chull){
        # Update the plot with a cluster convex hull
        clusterHull <- df %>%
                    group_by(clust) %>%
                    slice(chull(!!xcol, !!ycol))
        p <- p + geom_polygon(data = clusterHull, aes(fill=factor(clust), color=factor(clust)), alpha = 0.1) 
      }
      if (include.ellipse) {    
        p <- p + stat_ellipse(data = df, aes(fill=factor(clust), color=factor(clust)), type="norm", geom = "polygon", alpha = 0.1)
      }
    }
  p + ggtitle(title) + coord_cartesian(xlim = xLim, ylim = yLim) 
}


TsneRunPlot <- function(df, perplexity, legend = NULL){
  # Runs the t-SNE algorithm for dimensionality reduction
  # for a given perplexity
  #
  # Args:
  #   df: the dataset to perform t-SNE on
  #   perplexity: perplexity argument used in t-SNE
  #   legend: if NULL the colour of the points are blue, if
  #           not NULL colour/legend of the points when displayed
  #
  # Returns:
  #   list(results, plot) containing the reduced dataset and a
  #   ggplot object for plotting the results
  
  idDf <- select_if(df, negate(is.numeric))
  df <- select_if(df, is.numeric)
  
  tsne <- tsne(df, perplexity = perplexity)

  resultsTsne <- data.frame(tsne)
  resultsTsne <- bind_cols(idDf, resultsTsne)

  title <- paste("tsne - Perplexity", perplexity, sep = " ")

  p <- DimReductionScatterPlot(resultsTsne, "X1", "X2", legend = legend, title = title)

  return(list("results" = resultsTsne, "plot" = p, "obj" = tsne))
}


TsneMultiplePerplexitiesRuns <- function(df, perplexities = c(10, 20, 30,  50), legend = NULL){
  # Runs the t-SNE algorithm for dimensionality reduction
  # for a list of perplexities
  #
  # Args:
  #   df: the dataset to perform t-SNE on
  #   perplexities: list of perplexities to run t-SNE for
  #   legend: if NULL the colour of the points are blue, if
  #           not NULL colour/legend of the points when displayed
  #
  # Returns:
  #   list(results, plot) containing the reduced dataset and a
  #   plot object containing all the returned plots in a grid
  
  graphs <- list()
  results <- list()

  for (i in 1:length(perplexities)){
    tsne <-  TsneRunPlot(df, perplexity = perplexities[i], legend = legend)

    results[[as.character(perplexities[i])]] <- tsne$results
    graphs[[i]] <- tsne$plot
  }

  p <- plot_grid(plotlist = graphs, ncol = 2)

  return(list("results" = results, "plot" = p))
}


PcaRunPlot <- function(df, legend = NULL, D1 = "PC1", D2 = "PC2"){
  # Computes PCA on the provided dataset which is
  # centered and scaled as part of this function
  #
  # Args:
  #   df: the dataset to perform PCA on
  #   legend: if NULL the colour of the points are blue, if
  #           not NULL colour/legend of the points when displayed
  #
  # Returns:
  #   list(results, plot, pca.obj) containing the reduced dataset (centered and scaled)
  #   a ggplot object for plotting the results, as well as the full PCA object

  idDf <- select_if(df, negate(is.numeric))
  df <- select_if(df, is.numeric)
  pcaNormalised <- prcomp(df, center = TRUE, scale. = TRUE)
  summary(pcaNormalised)

  results <- data.frame(pcaNormalised$x)
  results <- bind_cols(idDf, results)

  p <- DimReductionScatterPlot(results, D1, D2, legend, title = "PCA")

  return(list("results" = results, "plot" = p, "obj" = pcaNormalised))
}

PcaDetailedPlot <- function(pca.obj){
    # Plots details of the feature contributions to the principal components
    # 
    # Args:
    #   pca.obj: an object of class PCA
    #
    # Returns:
    #   list(variancePlot, eigenPlot, contribDiscPlot, contribBarPlot)
    
    pcaVar <- get_pca_var(pca.obj)
    numVar <- nrow(pcaVar$contrib)
    explainedVariance <- fviz_eig(pca.obj, choice = "variance", geom = c("bar", "line"),
                            ncp = 10, addlabels = TRUE, xlab = "PCA Components",
                            main = "Explained Variance")
    explainedVariance <- explainedVariance + scale_y_continuous(sec.axis=sec_axis(~.*numVar/100, name="Eigenvalue"))+geom_hline(yintercept=100/numVar, linetype="dashed", color = "red")
    
    cos2Circle <- fviz_pca_var(pca.obj, axes = c(1, 2), geom = c("arrow", "text"),
                            repel = TRUE, col.var = "cos2", 
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))                        
    contribCircle <- fviz_pca_var(pca.obj, axes = c(1, 2), geom = c("arrow", "text"),
                            repel = TRUE, col.var = "contrib", 
                            gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
    cos2Map <- ggcorrplot(pcaVar$cos2, method = "square", hc.order = FALSE,
                                colors = c("white", "white", "steelblue"), 
                                title="Quality of representation") + theme(legend.position = "none")
    contribMap <- ggcorrplot(pcaVar$contrib/100, method = "square", hc.order = FALSE,
                                colors = c("white", "white", "steelblue"), 
                                title="Contributions to PCs") + theme(legend.position = "none")                            
    
    cos2Bar <- fviz_cos2(pca.obj, choice = "var", axes = c(1,2), top = 20)
    contribBar <- fviz_contrib(pca.obj, choice = "var", axes = c(1,2), top = 20)
    
    return(list("explainedVariance" = explainedVariance, 
                "cos2Circle" = cos2Circle,
                "contribCircle" =  contribCircle,
                "cos2Map" = cos2Map,
                "contribMap" = contribMap,
                "cos2Bar" = cos2Bar,
                "contribBar" = contribBar))
} 

UmapRunPlot <- function(df, legend = NULL){
  # Computes Uniform Manifold Approximation and Projection (UMAP)
  # dimensionality reduction technique on the provided dataset
  # - default arguments are used, no customisation
  #
  # Args:
  #   df: the dataset to perform UMAP on
  #   legend: if NULL the colour of the points are blue, if
  #           not NULL colour/legend of the points when displayed
  #
  # Returns:
  #   list(results, plot) containing the reduced dataset
  #   and a ggplot object for plotting the results
  
  idDf <- select_if(df, negate(is.numeric))
  df <- select_if(df, is.numeric)
  
  umapReduction <- umap(df, method = "naive")

  resultsUmap <- data.frame(umapReduction$layout)
  resultsUmap <- bind_cols(idDf, resultsUmap)

  p <- DimReductionScatterPlot(resultsUmap, "X1", "X2", legend, title = "Umap")

  return(list("results" = resultsUmap, "plot" = p, "obj" = umapReduction))

}

OverlayProjectionPoints <- function(p, df, colx, coly, title = NULL, ...) {
  xcol <- rlang::parse_expr(colx)
  ycol <- rlang::parse_expr(coly)
  
  p <- p + geom_point(data = df,  aes(x=!!xcol, y=!!ycol), ...) 
  if (!is.null(title)) {
      p + ggtitle(title)
  }

}

