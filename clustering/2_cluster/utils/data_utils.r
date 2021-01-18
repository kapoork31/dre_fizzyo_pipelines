########################################################################
## General Data Utils
##
## DfSelectNumeric: Selects the numeric columns from a dataframe
##
########################################################################

DfSelectNumeric <- function(df, include.logical = FALSE) {
  # Select numeric column from a dataframe and throws an error if the
  # dataframe contains no numeric columns
  #
  # Args:
  #   df: dataframe
  #   include.logical: bool, if TRUE consider logical columns as numeric
  #                   (defalt FALSE)
  #
  # Returns: a subset of the dataframe containing only numeric values

  numDf <- select_if(df, is.numeric)
  if (include.logical) {
    logDf <- select_if(df, is.logical)
    numDf <- cbind(numDf, logDf)
  }
  if (ncol(numDf) == 0) {
    stop("Error: The dataframe doesn't contain any numerical columns")
  }
  return(numDf)
}
