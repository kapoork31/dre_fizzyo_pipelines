# This script needs to be run before library(NMF) is called.
# NMF tries to check available cores, but if the container doesn't
# have access to the ("parallel") functions to query cores, then
# the query returns NA, causing NMF to fail.
# This is a workaround from Aridhia helpdesk


new_getMaxCores <- function(limit=TRUE){
  #ceiling(parallel::detectCores()/2)
  nt <- n <- parallel::detectCores()
  ## EDIT
  # If the number of cores detected is NA then try again with base R functions
  # Note that this assumes R is running on linux-gnu (R.version$os)
  if(is.na(nt)) {
    nt <- n <- length(grep("^processor", readLines("/proc/cpuinfo")))
  }
  ## EDIT
  # limit to number of cores specified in options if asked for
  if( limit ){
    if( !is.null(nc <- getOption('cores')) ) n <- nc # global option
    else if( !is.null(nc <- nmf.getOption('cores')) ) n <- nc # NMF-specific option
    else if( n > 2 ) n <- n - 1L # leave one core free if possible
  }
  # forces limiting maximum number of cores to 2 during CRAN checks
  if( n > 2 && isCHECK() ){
    message("# NOTE - CRAN check detected: limiting maximum number of cores [2/", nt, "]")
    n <- 2L
  }
  n
}

assignInNamespace("getMaxCores", new_getMaxCores, ns = "NMF")
