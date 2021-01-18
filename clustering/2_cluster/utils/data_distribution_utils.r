install.packages("moments")
install.packages("goft")
library(moments)
library(goft)

CheckColumnDistribution <- function(column, name){
  
  skew <- skewness(column)
  kurt <- kurtosis(column)
  
  hist(column, main = name)
  print(paste("Skewness of ", name, " is ", skew, sep = ""))
  print(paste("Kurtosis of ", name, " is ", kurt, sep = ""))
  
}

CheckDatasetDistribution <- function(df){
  df <- select_if(df, is.numeric)
  
  df %>%
    purrr::iwalk(~CheckColumnDistribution(.x, .y))
  
}

GetColumnNormality <- function(downSampledColumn){
    normal_test(downSampledColumn)$p.value
}

SampleColumn <- function(column, nSamples=400){
    sample(na.omit(column), nSamples, replace=T)
}

PlotColumnNormality <- function(df, pValue=1) {
    df <- select_if(df, is.numeric)
    
    features <- names(df)
    for (feature in features) {
        column <- df %>% pull(feature)
        downSampledColumn <- SampleColumn(column)
        pval <- GetColumnNormality(downSampledColumn)
        print(paste("P Value of", feature, "is", pval))
        if (pval < pValue){
            hist(column, main=paste(feature, "p value=", pval))
        }
    }
}