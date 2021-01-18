#####################################################################
## Visualization Utils for assessing clustering tendency in the data
##
## HopkinsStatistic: Hopkins Statistics of clustering tendency
## DissimilarityPlot: Plot of the ordered dissimilarity matrix
##
#####################################################################
library(factoextra)

HopkinsStatistic <- function(df, n = 100) {
  # The Hopkins statistic (Lawson and Jurs 1990) is used to assess the
  # clustering tendency of a data set by measuring the probability
  # that a given data set is generated by a uniform data distribution.
  # In other words, it tests the spatial randomness of the data.
  # The smaller the value the stronger clustering behaviour is present in
  # the data. A value of 0.5 indicates the lack of clustering tendency.
  #
  # Args:
  #   df: dataframe, the dataset to compute Hopkins Statistic on
  #   n: integer, size of the downsampled dataset to compute the statistic
  #         (default 100)
  #
  # Returns:
  #   float, the value of the Hopkins statistic

  df <- select_if(df, is.numeric)
  if (ncol(df) == 0) {
    stop("Error: The dataframe doesn't contain any numerical columns")
  }
  n <- min(n, nrow(df) - 1)
  if (n < 2) {
    stop("Error: Not enough rows tp compute Hopkins Statistic")
  }
  clustTend <- get_clust_tendency(df, n, graph = FALSE)
  clustTend$hopkins_stat
}

DissimilarityPlot <- function(df, nmax = 500,
                              title = "Dissimilarity Ordered Plot",
                              method = "euclidean", hopkins = TRUE) {
  # The dissimilary plot can be used to visually inspect the clustering
  # tendency of a dataset. The algorithm works as follows:
  # First it computes the dissimilarity (DM) matrix between the individuals
  # in the dataset using some distance measure (e.g. Euclidean).
  # Subsequently reorder the DM so that similar objects are close to one
  # another, creating an ordered dissimilarity matrix (ODM).
  # This function outputs a colorcoded ggplot of the resulting ODM
  #
  # Args:
  #   df: dataframe, the dataset to assess clustering tendency on
  #   nmax: integer, size of the downsampled dataset to compute the ODM
  #         (default 500)
  #   title: string, title of the plot (default given)
  #   method: string, distance measure to be used. This must be one of
  #           "euclidean", "maximum", "manhattan", "canberra", "binary",
  #           "minkowski", "pearson", "spearman" or "kendall"
  #           (default "euclidean")
  #   hopkins: bool, whether or not to include the hopkins statistic in
  #            the title of the plot (default TRUE)
  #
  # Returns:
  #   ggplot object of the ordered dissimilarity matrix plot

  # restrict to numeric columns
  df <- select_if(df, is.numeric)
  if (ncol(df) == 0) {
    stop("Error: The dataframe doesn't contain any numerical columns")
  }

  nmax <- min(nmax, nrow(df))
  df <- sample_n(df, nmax)

  distMat <- get_dist(df, stand = FALSE, method = method)

  p <- fviz_dist(distMat, order = TRUE, show_labels = FALSE,
                 gradient = list(low = "#FC4E07",
                                 mid = "white",
                                 high = "#00AFBB"))
  if (hopkins) {
    h <- HopkinsStatistic(df, 100)
    title <- paste(title, "| Hopkins Statistic =", round(h, digits = 3))
  }

  p + ggtitle(title)
}
