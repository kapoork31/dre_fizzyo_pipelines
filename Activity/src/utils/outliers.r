###################################################################################################
# Outlier Handling Utilities
#
# is_tukey_outlier: Helper function, used to determine whether an observation is an outlier 
#   according to Tukey's method
# IdentifyOutliers: Compute the outliers for each column and display outlier statistics
# remove_outliers: Remove outliers from specified columns in the data
# outlier_func3: label outliers in given columns in a dataframe
##################################################################################################

library(dplyr)

is_tukey_outlier <- function(x, k = 1.5, na.rm = TRUE) {
  # Determines whether an observation is an outlier
  #     according to Tukey's boxplot method
  #
  # Args:
  #   x: (n x 1) numeric vector
  #   k: float, inter-quartile-range multiplier do define the data whiskers
  #      (default 1.5)
  #   na.rm: bool, if TRUE, remove outliers before computing data quantiles
  #          (default TRUE)
  #
  # Returns: (n x 1) logical vector,
  #     a TRUE value indicates that the observation is an outlier

  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- diff(quar)
  (x <= quar[1] - (k * iqr)) | (x >= quar[2] + (k * iqr))
}

identify_outliers <- function(df, cols) {
  # Identifies outliers in the columns of a dataframe
  #
  # Args:
  #   df: dataframe
  #   cols: a single string, or a list of strings,
  #     corresponds to columns to identify outliers on
  #   verbose: bool, if TRUE the function prins column level statistics
  #            (default FALSE)
  #   visualize: bool, if verbose and visualize TRUE,
  #     plot boxplots and histogram
  #              (default FALSE)
  #
  # Returns: an extension of the original dataframe with the following schema
  #          df + |colName_1_is_out <lgl>|colName_2_is_out
  #          <lgl>|...|colName_k_is_out <lgl>|

  numeric_df <- SelectNumericColumns(df)
  numeric_cols <- intersect(colnames(numeric_df), cols)

  if (length(numeric_cols) == 0) {

    stop("Error: Please provide at least one valid column name")
  }

  numeric_df %>% dplyr::transmute_at(vars(numeric_cols),
    funs(is_out = is_tukey_outlier)) -> outlier_df

  if (length(numeric_cols) == 1) {
    # rename is_out column to colName_is_out
    colnames(outlier_df)[colnames(outlier_df) == "is_out"] <-
        paste(numeric_cols, "is_out", sep = "_")
  }

  # bind everything together to create a full dataframe
  full_df <- bind_cols(df, outlier_df)

  return(full_df)
}

remove_outliers <- function(df, cols = NULL, thr = 0.03, visualize=FALSE) {
  # Remove outliers from the specified columns of a dataframe
  #
  # Args:
  #   df: dataframe
  #   cols: a single string, or a list of strings,
  #     corresponds to columns to identify outliers on
  #   thr: float, the threshold value for removing outliers,
  #     outliers are removed from a column
  #        only if their ratio is below this value (defaut 0.03 ~ 3% outliers)
  #   verbose: bool, if TRUE the function prins column level statistics
  #            (default FALSE)
  #   visualize: bool, if verbose and visualize TRUE,
  #     plot boxplots and histogram
  #              (default FALSE)
  #
  # Returns:

  if (is.null(cols)) {
      cols <- colnames(df)
  }
  print("Computing feature wise outliers according to Tukey method")
  print("********************************")
  full_df <- identify_outliers(df, cols)

  print("Removing outliers where the ratio of outliers is below the threshold")
  print("**********************************")
  is_outlier_vec <- full_df %T>% {

    print(paste("Initial number of rows is", nrow(.)))
  } %>%
  dplyr::select_if(function(col) is.logical(col) &&
  mean(col) < thr) %T>% {

    print(paste("Removing outliers from", colnames(.)))
  } %>%
  dplyr::mutate(is_out = (rowSums(.) >= 1)) %>%
  dplyr::pull(is_out)
  #TRUE if the observation is outlier for at least one column

  full_df %>% dplyr::mutate(is_out = is_outlier_vec) %>%
  dplyr::filter(is_out == FALSE) %T>% {

    print(paste("Final number of rows is", nrow(.)))
  } %>%
  dplyr::select(colnames(df))

  return (full_df)
}

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

  num_df <- select_if(df, is.numeric)
  if (include.logical) {
    log_df <- select_if(df, is.logical)
    num_df <- cbind(num_df, log_df)
  }
  if (ncol(num_df) == 0) {
    stop("Error: The dataframe doesn't contain any numerical columns")
  }
  num_df
}

outlier_func3 <- function(cluster_df, k_feature_names) {

    # Select numeric column from a dataframe and throws an error if the
    # dataframe contains no numeric columns
    #
    # Args:
    #   cluster_df: dataframe
    #   k_feature_names: names of features to check for outliers
    #                   (defalt FALSE)
    #
    # Returns: cluster_df: df with outliers labeled per row

    identify_outliers(cluster_df, k_feature_names)
    nrow(cluster_df)
    print("********** Removing outliers...")
    k_outlier_names <- k_feature_names
    outlier_df <- cluster_df

    outlier_df <- cluster_df
    cluster_df <- remove_outliers(outlier_df, k_outlier_names, thr = 0.05)
    nrow(cluster_df)
    return(cluster_df)
}
