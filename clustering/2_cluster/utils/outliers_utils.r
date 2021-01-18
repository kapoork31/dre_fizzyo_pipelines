###################################################################################################
# Outlier Handling Utilities
#
# IsTukeyOutlier: Helper function, used to determine whether an observation is an outlier 
#                 according to Tukey's method
# PlotColumnDistribution: Helper function that returns 2 plots correspondsing to the boxplots and 
#                         histograms of a data column
# IdentifyOutliers: Compute the outliers for each column and display outlier statistics
# RemoveOutliers: Remove outliers from specified columns in the data
##################################################################################################

library(cowplot)
library(dplyr)
library(ggplot2)

IsTukeyOutlier <- function(x, k = 1.5, na.rm = TRUE) {
  # Determines whether an observation is an outlier according to Tukey's boxplot method
  # 
  # Args: 
  #   x: (n x 1) numeric vector
  #   k: float, inter-quartile-range multiplier do define the data whiskers
  #      (default 1.5)
  #   na.rm: bool, if TRUE, remove outliers before computing data quantiles
  #          (default TRUE)
  #
  # Returns: (n x 1) logical vector, a TRUE value indicates that the observation is an outlier

  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  (x <= quar[1] - k * iqr) | (x >= quar[2] + k * iqr)
}


PlotColumnDistribution <- function(df, col, title = NULL) {
  # Returns the boxplots and histograms of a data column
  #
  # Args:
  #   df: dataframe
  #   col: symbol, desired column (as returned by rlang::ensym())
  #
  # Returns: a list with the following ggplots:
  #   boxPlot: violin and boxplot of the data
  #   histPlot: histogram of the data 
  
  boxPlot <- ggplot(data = df, aes(y = !!col, x= "Distribution")) +
             geom_violin(alpha = 0.4, position = position_dodge(width = 0.7),
                         size = 0.7, color="black", fill = "blue") +
             geom_boxplot(notch = TRUE, outlier.size = 1, size = 0.5, width = 0.1,
                          fill = "blue", color = "black", alpha = 0.4,
                          outlier.color = "red", outlier.alpha = 0.5)

  histPlot <- ggplot(data = df, aes(x = !!col)) +
              geom_histogram(alpha = 0.5, color = "black", fill = "tomato", bins=30)

  if (!is.null(title)) {
    boxPlot <- boxPlot + ggtitle(title)
    histPlot <- histPlot + ggtitle(title)
  }

  return(list("boxPlot" = boxPlot, "histPlot" = histPlot))
}

IdentifyOutliers <- function(df, cols, verbose = FALSE, visualize = FALSE) {
  # Identifies outliers in the columns of a dataframe
  #
  # Args:
  #   df: dataframe
  #   cols: a single string, or a list of strings, corresponds to columns to identify outliers on
  #   verbose: bool, if TRUE the function prins column level statistics
  #            (default FALSE)
  #   visualize: bool, if verbose and visualize TRUE, plot boxplots and histogram
  #              (default FALSE)
  #
  # Returns: an extension of the original dataframe with the following schema
  #          df + |colName_1_isOut <lgl>|colName_2_isOut <lgl>|...|colName_k_isOut <lgl>|

  numericDf <- SelectNumericColumns(df)
  numericCols <- intersect(colnames(numericDf), cols)
  if (length(numericCols) == 0) {
    stop("Error: Please provide at least one valid column name")
  }
  numericDf %>% dplyr::transmute_at(vars(numericCols), funs(isOut = IsTukeyOutlier)) -> outlierDf
  if (length(numericCols) == 1) {
    # rename isOut column to colName_isOut
    colnames(outlierDf)[colnames(outlierDf)=="isOut"] <- paste(numericCols, "isOut", sep = "_")
  }
  
  # bind everything together to create a full dataframe
  fullDf <- bind_cols(df, outlierDf)

  if (verbose) {
    for (colStr in setdiff(cols, numericCols)) {
      print(paste(colStr, "was not used to compute outliers"))
      print('************************************************************************')
    }
    
    for (colStr in numericCols) {
      outColStr <- paste(colStr, 'isOut', sep='_')
      
      col <- rlang::ensym(colStr)
      outCol <- rlang::ensym(outColStr)

      fullDf %>% 
        dplyr::select(!!col, !!outCol) %>%
        dplyr::group_by(!!outCol) %>%
        dplyr::summarise(n = n(), mean = mean(!!col)) -> summaryDf
      
      # Compute number of outliers for this column
      nOut <- summaryDf %>% dplyr::filter(!!outCol == TRUE) %>% dplyr::pull(n) 
      if (length(nOut) == 0) {
        print(paste("There are no outliers identified in", colStr))
        if (visualize) {
          p <- PlotColumnDistribution(fullDf, col,
                                      title=paste(colStr, ": No outliers identified"))
          print(plot_grid(p$boxPlot, p$histPlot))
        }
      } else {
        pOut <- (nOut/nrow(fullDf)) * 100
        meanOut <- summaryDf %>% dplyr::filter(!!outCol == TRUE) %>% dplyr::pull(mean)
        meanNoOut <- summaryDf %>% dplyr::filter(!!outCol == FALSE) %>% dplyr::pull(mean)
        meanTotal <- fullDf %>% summarise(mean = mean(!!col)) %>% pull(mean)

        print(paste("Outliers identified for", col, ":", nOut, "from", nrow(fullDf), "observations"))
        print(paste("Proportion (%) of outliers is ", round(pOut, digits =6 )))
        print(paste("Mean of the outliers",  round(meanOut, digits = 6)))
        print(paste("Mean without removing the outliers:", round(meanTotal, digits = 6)))
        print(paste("Mean if we remove outliers:", round(meanNoOut, digits = 6)))

        if (visualize) {
          p1 <- PlotColumnDistribution(fullDf, col, title = paste(colStr, ": With outliers"))
          p2 <- PlotColumnDistribution(dplyr::filter(fullDf, !!outCol == FALSE),
                                       col, title = paste(colStr, ": Without outliers"))
          print(plot_grid(p1$boxPlot, p2$boxPlot, p1$histPlot, p2$histPlot))
        }
      }
      print('************************************************************************')
    }
  }
  return(fullDf)
}

RemoveOutliers <- function(df, cols = NULL, thr = 0.03, verbose=FALSE, visualize=FALSE) {
  # Remove outliers from the specified columns of a dataframe
  #
  # Args:
  #   df: dataframe
  #   cols: a single string, or a list of strings, corresponds to columns to identify outliers on
  #   thr: float, the threshold value for removing outliers; outliers are removed from a column
  #        only if their ratio is below this value (defaut 0.03 ~ 3% outliers)
  #   verbose: bool, if TRUE the function prins column level statistics
  #            (default FALSE)
  #   visualize: bool, if verbose and visualize TRUE, plot boxplots and histogram
  #              (default FALSE)
  #
  # Returns:

  if (is.null(cols)) {
      cols <- colnames(df)
  }
  print("Computing feature wise outliers according to Tukey method")
  print('************************************************************************')
  fullDf <- IdentifyOutliers(df, cols, verbose, visualize)

  print("Removing outliers for the features where the ratio of outliers is below the threshold")
  print('************************************************************************')
  isOutlierVec <- fullDf %T>% {print(paste('Initial number of rows is', nrow(.)))} %>%
    dplyr::select_if(function(col) is.logical(col) && mean(col) < thr) %T>% {
        print(paste('Removing outliers from', colnames(.)))} %>%
    dplyr::mutate(isOut = (rowSums(.) >= 1)) %>%
    dplyr::pull(isOut) #TRUE if the observation is outlier for at least one column

  fullDf %>% dplyr::mutate(isOut = isOutlierVec) %>%
    dplyr::filter(isOut == FALSE) %T>% {print(paste('Final number of rows is', nrow(.)))} %>%
    dplyr::select(colnames(df))
}