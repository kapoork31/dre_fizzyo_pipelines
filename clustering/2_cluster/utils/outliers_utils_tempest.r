# Exclude Start

###################################################################################################
# outliers_utils.R

# Heavily inspired by: 
#  https://datascienceplus.com/identify-describe-plot-and-removing-the-outliers-from-the-dataset/
# uses the Tukey's method:
# - it is not dependent on the distribution of data
# - ignores the mean and standard deviation, which are influenced by the extreme values (outliers).
##################################################################################################


DetectOutliers <- function(dt, var, info, visualize = TRUE, shouldRemove = FALSE) {
  var_name <- eval(substitute(var),eval(dt))
  tot <- sum(!is.na(var_name))
  na1 <- sum(is.na(var_name))
  m1 <- mean(var_name, na.rm = T)
  
  ##############################################
  # Fix for plotting issues, provided by Aridhia
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  ############################################  
  
  
  par(mfrow=c(2, 2), oma=c(0,0,3,0))
  b <- boxplot(var_name, main="With outliers")
  h <- hist(var_name, main="With outliers", xlab=NA, ylab=NA)
  
  if (visualize) {
   b
   h
  }
    
  outlier <- boxplot.stats(var_name)$out
  mo <- mean(outlier)
  var_name <- ifelse(var_name %in% outlier, NA, var_name)
  
  if (visualize) {
    boxplot(var_name, main="Without outliers")
    hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
  }
  
  title(sprintf("Outlier Check for %s", info), outer=TRUE)
  na2 <- sum(is.na(var_name))
  message("Outliers identified for ", info, ": ", na2 - na1, " from ", tot, " observations")
  message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
  message("Mean of the outliers: ", mo)
  m2 <- mean(var_name, na.rm = T)
  message("Mean without removing outliers: ", m1)
  message("Mean if we remove outliers: ", m2)
  
  if(shouldRemove){
    dt[as.character(substitute(var))] <- invisible(var_name)
    assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
    message("Outliers successfully removed", "\n")
    return(invisible(dt))
  } else{
    message("Nothing changed", "\n")
    return(invisible(var_name))
  }
}

IsOutlierTukey <- function(x, k = 1.5, na.rm = TRUE) {
# Given a vector x returns a vector of T/F of the same length 
# corresponding to weather or not the observation is an outlier 
# according to Tukey method (same a s boxplot method)

  quar <- quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)
  (x <= quar[1] - k * iqr) | (x >= quar[2] + k * iqr)
}

IdentifyOutliers <- function(df, cols, verbose = FALSE, visualize = FALSE) {

    numericDf <- dplyr::select_if(df, is.numeric) 
    numericCols <- intersect(colnames(numericDf), cols) 
    numericDf %>% dplyr::transmute_at(vars(numericCols), funs(isOut = IsOutlierTukey)) -> outlierDf 
    
    fullDf <- bind_cols(df, outlierDf) # bind everything together to create a full Df
    
    if (verbose) {
        logicCols <- colnames(select_if(fullDf, is.logical))
        
        for (colStr in cols) {
            if (is.element(colStr, numericCols)) {
                outColStr <- paste(colStr, 'isOut', sep='_')
                if (!is.element(outColStr, logicCols)) {
                    outColStr = 'isOut'
                }
                
                col <- rlang::ensym(colStr)
                outCol <- rlang::ensym(outColStr)
                fullDf %>% 
                    dplyr::select(!!col, !!outCol) %>%
                    dplyr::group_by(!!outCol) %>%
                    dplyr::summarise(n = n(), mean = mean(!!col)) -> summaryDf
                
                nOut <- summaryDf %>% dplyr::filter(!!outCol == TRUE) %>% dplyr::pull(n) 
                if (length(nOut) > 0){
                    pOut <- (nOut/nrow(fullDf)) * 100
                    meanOut <- summaryDf %>% dplyr::filter(!!outCol == TRUE) %>% dplyr::pull(mean)
                    meanNoOut <- summaryDf %>% dplyr::filter(!!outCol == FALSE) %>% dplyr::pull(mean)
                    meanTotal <- fullDf %>% summarise(mean = mean(!!col)) %>% pull(mean)
                
                    print(paste("Outliers identified for", col, ":", nOut, "from", nrow(fullDf), "observations"))
                    print(paste("Proportion (%) of outliers is ", round(pOut, digits=6)))
                    print(paste("Mean of the outliers",  round(meanOut, digits=6)))
                    print(paste("Mean without removing the outliers:", round(meanTotal, digits=6)))
                    print(paste("Mean if we remove outliers:", round(meanNoOut, digits=6)))
                    
                    if (visualize) {
                        print("TODO: Boxplots and histograms: Not yet implemented")
                    }
                }
                else {
                    print(paste("There are no outliers identified in", colStr))
                }
                
            } else {
                print(paste(colStr, "was not used to identify outliers"))
            }
            print('************************************************************************')
        }
    }
    return(fullDf)
}

RemoveOutliers <- function(df, cols, thr = 0.03, verbose=TRUE) {

    fullDf <- IdentifyOutliers(df, cols, verbose)
    isOutlierVec <- fullDf %>%
        dplyr::select_if(function(col) is.logical(col) && mean(col) < thr) %>%
        dplyr::mutate(isOut = (rowSums(.) >= 1)) %>%
        dplyr::pull(isOut)

    fullDf %>% dplyr::mutate(isOut = isOutlierVec) %>% 
               dplyr::filter(isOut == FALSE) %>%
               dplyr::select(colnames(df))
}
# Exclude End