################################################################################
## This file contains the following fitbit feature extraction functions:
##
## - StepCount : Total number of steps in the dataframe
## - MeanHourlySteps: Average of hourly number of steps
## - ActiveHoursCount: Calculate number of active hours
## - LowActiveHoursCount: Calculate number of low activity hours
## - GetActiveWindow: Return the timestamp of the start and end of activity
## - RestingHrProxy: Calculate resting heartrate features
## - HourlyInterval: Extract hourly min, max, and median heartrate metrics
## - MinuteWearCount: Number of minutes the fitbit was worn based on
##                    the filter condition
## - CountSwitches: Number of switches during a time interval
## - ValidateFitbitDataframe: checks that fitbit data is a non-empty data frame
##                            with no inf/-inf values
## - CustomFilter: Applies a filter on top of data if a filter expression is given
## - CountMinutes: Count number of consecutive active minutes
## - ActiveMinutes: Number of minutes with HR > threshold
##                  after n consecutive minutes with HR > threshold
## - CoefficientOfVariation: Calculates Coefficient of variation
## - CombineFootstepsHR: Combine the footsteps and heartrate together
## - StepNormActivityPeriods: Normalised step count for an activity period
## - ActiveMinutesHeartrateSteps: Number of values above the active threshold
## - FeaturiseFitbitSteps: Feature fitbit footsteps function
## - FeaturiseFitbitHeartRate: Feature fitbit heartrate
## - FeaturiseFitbitCombined: Featurise fitbit combine data (steps and HR)
################################################################################

library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(rlang)

FitbitFeaturesSchema <- data.frame(
  userid = character(),
  date = lubridate::ymd(),
  stepCount = numeric(),
  stepCountQ2 = numeric(),
  stepCountQ3 = numeric(),
  stepCountQ4 = numeric(),
  stepCountQ234 = numeric(),
  meanHourlyStepsDay = numeric(),
  activeHours = numeric(),
  lowActiveHours = numeric(),
  activeMinsSteps0 = numeric(),
  activeMinsSteps2 = numeric(),
  activeMinsSteps5 = numeric(),
  activeMinsSteps10 = numeric(),
  activeMinsSteps20 = numeric(),
  activeMinsSteps0AP = numeric(),
  CoefficientOfVariationStep = numeric(),
  CoefficientOfVariationStepQ1 = numeric(),
  CoefficientOfVariationStepQ2 = numeric(),
  CoefficientOfVariationStepQ3 = numeric(),
  CoefficientOfVariationStepQ4 = numeric(),
  CoefficientOfVariationStepQ234 = numeric(),
  restingHrProxy = numeric(),
  restingHr722 = numeric(),
  RegressionMVPAThreshold = numeric(),
  RegressionMVPAPrev15 = numeric(),
  activeMinsHrRegressionThresh = numeric(),
  activeMinsHrRegressionThresh1 = numeric(),
  activeMinsHrRegressionThresh2 = numeric(),
  activeMinsHrRegressionThresh5 = numeric(),
  activeMinsHrRegressionThresh10 = numeric(),
  activeMinsHrRegressionThresh20 = numeric(),
  activeMinsHr0 = numeric(),
  activeMinsHr1 = numeric(),
  activeMinsHr2 = numeric(),
  activeMinsHr5 = numeric(),
  activeMinsHr10 = numeric(),
  activeMinsHr20 = numeric(),
  activeMinsHr0AP = numeric(),
  activeMinsHrThreshAP = numeric(),
  CoefficientOfVariationHr = numeric(),
  CoefficientOfVariationHrQ1 = numeric(),
  CoefficientOfVariationHrQ2 = numeric(),
  CoefficientOfVariationHrQ3 = numeric(),
  CoefficientOfVariationHrQ4 = numeric(),
  CoefficientOfVariationHrQ234 = numeric(),
  minsWearTotal = numeric(),
  minsWear611 = numeric(),
  minsWear710 = numeric(),
  minsWearQ1 = numeric(),
  minsWearQ2 = numeric(),
  minsWearQ3 = numeric(),
  minsWearQ4 = numeric(),
  minsWearQ234 = numeric(),
  wearPercent = numeric(),
  wearQ1Percent = numeric(),
  wearQ2Percent = numeric(),
  wearQ3Percent = numeric(),
  wearQ4Percent = numeric(),
  wearQ234Percent = numeric(),
  gapPercent = numeric(),
  gapPercentQ1 = numeric(),
  gapPercentQ2 = numeric(),
  gapPercentQ3 = numeric(),
  gapPercentQ4 = numeric(),
  wearDuringSleep = numeric(),
  switchThresh = numeric(),
  switch100 = numeric(),
  switch120 = numeric(),
  switch140 = numeric(),
  activeMinsHrStepsThresh = numeric(),
  activeMinsHrStepsThresh2 = numeric(),
  activeMinsHrStepsThresh5 = numeric(),
  activeMinsHrStepsThresh10 = numeric(),
  activeMinsHrStepsThresh20 = numeric(),
  activeMinsHrSteps0 = numeric(),
  activeMinsHrSteps2 = numeric(),
  activeMinsHrSteps5 = numeric(),
  activeMinsHrSteps10 = numeric(),
  activeMinsHrSteps20 = numeric(),
  stepCountNorm = numeric(),
  stepCountNormQ2 = numeric(),
  stepCountNormQ3 = numeric(),
  stepCountNormQ4 = numeric(),
  stepNormModerateThresh = numeric(),
  stepNormModVigThresh = numeric(),
  stepNormSedentaryThresh = numeric(),
  stepNormModerate = numeric(),
  stepNormVigorous = numeric(),
  stepNormModVig = numeric(),
  stepNormSedentary = numeric(),
  GovRec = numeric(),
  startWindow = lubridate::ymd_hms(),
  endWindow = lubridate::ymd_hms(),
  maxHourly = numeric(),
  minHourly = numeric(),
  medianHourly = numeric(),
  
  stringsAsFactors = FALSE # Parameter, not a feature
)

# define constants for time intervals
kMinsDay <- 24 * 60
kMinsQuadrant <- kMinsDay / 4
kMinsActiveQs <- kMinsQuadrant*3

# quadrants filter expressions for heartrate data
filterHrQ1 <- "time_hour < 6"
filterHrQ2 <- "time_hour >= 6 & time_hour < 12"
filterHrQ3 <- "time_hour >= 12 & time_hour < 18"
filterHrQ4 <- "time_hour >= 18"

# quadrants filter expressions for combined hr and steps
filterCombQ1 <- "lubridate::hour(time) < 6"
filterCombQ2 <- "lubridate::hour(time) >= 6 & lubridate::hour(time) < 12"
filterCombQ3 <- "lubridate::hour(time) >= 12 & lubridate::hour(time) < 18"
filterCombQ4 <- "lubridate::hour(time) >= 18"
filterCombQ5 <- "lubridate::hour(time) >= 6 & lubridate::hour(time) < 23"
filterCombQ6 <- "lubridate::hour(time) >= 7 & lubridate::hour(time) < 22"
filterCombQ234 <- "lubridate::hour(time) >= 6 & lubridate::hour(time) < 24"

###################################
# FootSteps Specific Utils
###################################

StepCount <- function(df,
                       valCol = "value",
                       filterExpr = NULL){
  # Total number of steps in the data.frame
  #
  # Args:
  #   df: data.frame containing a column 'value' to sum
  #     ... | "valCol" <num> | ... -> variable schema
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   filterExpr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #   StepCount <num>

  if (valCol %in% names(df)){

    valCol <- rlang::ensym(valCol)

    df %>%
      CustomFilter(filterExpr) %>%
      summarise(stepCount = sum(!!valCol, na.rm = TRUE)) %>%
      pull(stepCount)
  } else {
    stop("data.frame does not contain the column specified")
  }
}


MeanHourlySteps <- function(df,
                            valCol = "value",
                            timeCol = "time"){
  # Average of hourly number of steps
  #
  # Args:
  #   df: data.frame with time, value columns (variable schema)
  #   ... | "valCol" <num> | "timeCol" <POSIXCt>|...
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   timeCol: string, default "time" column
  #           name of the column which contains the timestamps for values
  #
  # Returns:
  #   meanHourlyStepsDay <num>

  if (valCol %in% names(df) & timeCol %in% names(df)){

    valCol <- rlang::ensym(valCol)
    timeCol <- rlang::ensym(timeCol)

    df %>%
      group_by(hour = lubridate::hour(!!timeCol)) %>%
      summarise(stepHour = sum(!!valCol, na.rm = TRUE)) %>%
      summarise(meanHourlyStepsDay = mean(stepHour, na.rm = TRUE)) %>%
      ungroup() %>%
      pull(meanHourlyStepsDay)
  } else {
    stop("data.frame does not contain the columns specified")
  }
}

ActiveHoursCount <- function(df,
                             valCol = "value",
                             timeCol = "time",
                             activeThreshold = 500){
  # Calculate number of active hours
  #
  # Args:
  #   df: data.frame with time, value columns (variable schema)
  #     ... | "valCol" <num> | "timeCol" <POSIXCt>|...
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   timeCol: string, default "time" column
  #           name of the column which contains the timestamps for values
  #   activeThreshold: numeric, minimum number of steps for a hour to be active
  #
  # Returns:
  #   number of active hours <num>

  if (valCol %in% names(df) & timeCol %in% names(df)){

    valCol <- rlang::ensym(valCol)
    timeCol <- rlang::ensym(timeCol)

    df %>%
      group_by(hour = lubridate::hour(!!timeCol)) %>%
      summarise(stepHour = sum(!!valCol, na.rm = TRUE)) %>%
      filter(stepHour > activeThreshold) %>%
      summarise(activeHours = n()) %>%
      ungroup() %>%
      pull(activeHours)
  } else {
    stop("data.frame does not contain the columns specified")
  }
}

LowActiveHoursCount <- function(df,
                                valCol = "value",
                                timeCol = "time",
                                lowActivityThreshold = 500){
  # Calculate number of low activity hours
  #
  # Args:
  #   df: data.frame with time, value columns (variable schema)
  #     ... | "valCol" <num> | "timeCol" <POSIXCt>|...
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   timeCol: string, default "time" column
  #           name of the column which contains the timestamps for values
  #   lowActivityThreshold: numeric, max number of steps
  #               for a hour to have low activity
  #
  # Returns:
  #   number of low activity hours <num>

  if (valCol %in% names(df) & timeCol %in% names(df)){

    valCol <- rlang::ensym(valCol)
    timeCol <- rlang::ensym(timeCol)

    df %>%
      group_by(hour = lubridate::hour(!!timeCol)) %>%
      summarise(stepHour = sum(!!valCol, na.rm = TRUE)) %>%
      filter(stepHour <= lowActivityThreshold & stepHour > 0) %>%
      summarise(lowActiveHours = n()) %>%
      ungroup() %>%
      pull(lowActiveHours)
  } else {
    stop("data.frame does not contain the columns specified")
  }
}

GetActiveWindow <- function(df,
                            valCol = "value",
                            timeCol = "time",
                            sleepThres = 25,
                            awakeThres = 5){
  # Get the activity window for the day
  #
  # Args:
  #   df: data frame with footsteps values (variable schema)
  #     ... | "valCol" <num> | "timeCol" <POSIXCt>|...
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   timeCol: string, default "time" column
  #           name of the column which contains the timestamps for values
  #   sleepThres: integer, default 25
  #               min number of steps to count when in sleep period
  #   awakeThres: integer, default 5
  #             min number of steps to count when in awake period
  #
  # Returns:
  #   data frame with start and end timestamps of activity
  #     startWindow <POSIXct> | endWindow <POSIXct>

  timeCol <- rlang::ensym(timeCol)
  valCol <- rlang::ensym(valCol)

  df %>%
    filter(lubridate::hour(!!timeCol) >= 6
           | (lubridate::hour(!!timeCol) <= 6 &
                !!valCol > sleepThres)) %>%
    filter(!!valCol > awakeThres) %>%
    arrange(!!timeCol) %>%
    summarise(startWindow = first(!!timeCol),
              endWindow = last(!!timeCol)) %>%
    select(startWindow, endWindow)

}

###################################
# Heartrate Specific Utils
###################################

RestingHrProxy <- function(df,
                           valCol = "value",
                           byGroupSortFlag = FALSE,
                           timeFilter = FALSE) {
  # Calculate resting heartrate proxy
  #
  # Args:
  #   df: heartrate data.frame (variable schema)
  #     ... | "valCol" <num> | ...
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   byGroupSortFlag: boolean, default FALSE
  #            if TRUE, sort based on group_by groups
  #
  # Returns:
  #   resting heartrate column <num>
 
  if (!any(is.finite(unlist(df[valCol], use.names = FALSE))) | nrow(df) == 0){
    return(0)
  }

  valColSym <- rlang::ensym(valCol)
  if( timeFilter == FALSE){    
      df %>%
        arrange_at(valCol, .by_group = byGroupSortFlag) %>%
        head(., 5) %>%
        ungroup() %>%
        summarise(restHrProxy = mean(!!valColSym, na.rm = TRUE)) %>%
        pull(restHrProxy)
  }else{
      df %>%
        filter(lubridate::hour(time) >= 7 & lubridate::hour(time) < 22) %>%
        arrange_at(valCol, .by_group = byGroupSortFlag) %>%
        head(., 5) %>%
        ungroup() %>%
        summarise(restHrProxy = mean(!!valColSym, na.rm = TRUE)) %>%
        pull(restHrProxy)
  }
}

HourlyInterval <- function(df,
                           valCol = "value",
                           grpExpr = NULL) {
  # Extract hourly min, max, and median heartrate metrics
  #
  # Args:
  #   df: heartrate data.frame (variable schema)
  #     ... | "valCol" <num> | ...
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   grpExpr: string, default NULL
  #           column name for grouping or grouping expression
  #
  # Returns:
  #   a data frame with heartrate metrics

  if (!any(is.finite(unlist(df[valCol], use.names = FALSE))) | nrow(df) == 0){
    return(tibble(maxHourly = 0, minHourly = 0, medianHourly = 0))
  }

  valCol <- rlang::ensym(valCol)

  if (!is.null(grpExpr)){

    df %>%
      group_by(!!rlang::parse_expr(grpExpr)) -> df

  }

  df %>%
    summarise(hourAvg = mean(!!valCol, na.rm = TRUE)) %>%
    summarise(maxHourly = max(hourAvg, na.rm = TRUE),
              minHourly = min(hourAvg, na.rm = TRUE),
              medianHourly = median(hourAvg, na.rm = TRUE)) %>%
    ungroup() %>%
    select(maxHourly, minHourly, medianHourly)
}

MinuteWearCount <- function(df,
                            hrCol = "value_hr",
                            filterExpr = NULL){
  # Number of minutes the fitbit was worn based on the filter condition
  #   Note: the minutes are counted when there are heartrate value
  #
  # Args:
  #   df: fitbit data frame (variable schema)
  #       ... | "hrCol" <num> | ...
  #   hrCol: string, default "value_hr column
  #         name of column containing hr values to check for NAs
  #   filterExpr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #   MinutesWearCount <num>

  hrCol <- rlang::ensym(hrCol)

  df %>%
    CustomFilter(filterExpr) %>%
    filter(!is.na(!!hrCol)) %>%
    count() %>%
    pull(n)
}

CountSwitches <-  function(df,
                           valCol = "value",
                           threshold = 120,
                           direction = "up",
                           sortCols = c("time_hour", "time_minute"),
                           filterExpr = NULL){
  # Number of switches during a time interval
  #
  #  Switch = value going above or below a threshold i.e. it switches from below a
  #           threshold value to above the threshold)
  #
  # Args:
  #   df: heartrate data.frame with heartrate data (variable schema)
  #     ... | "valCol" <num> | "sortCols" <num>|...
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   threshold: integer, default 120
  #             threshold for switch value
  #   direction: string vector, default "up"
  #             "up": count switches only in up direction
  #             "down": count switches only in down directions
  #             "both": count switches in both up and down directions
  #   sortCols: vector of string, default c("time_hour", "time_minute")
  #             columns to sort values by
  #   filterExpr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #   Number of threshold switches <num>

  valCol <- rlang::ensym(valCol)

  df %>%
    CustomFilter(filterExpr) %>%
    arrange_at(sortCols) %>%
    mutate(isHigh = if_else(!!valCol >= threshold, 1, 0, missing = 0),
           isSwitch = dplyr::case_when(direction == "both" ~ if_else(isHigh != lead(isHigh),
                                                              1, 0, missing = 0),
                                      direction == "up" ~ if_else(isHigh < lead(isHigh),
                                                                  1, 0, missing = 0),
                                      direction == "down" ~ if_else(isHigh > lead(isHigh),
                                                              1, 0, missing = 0))) %>%
    summarise(numSwitches = sum(isSwitch, na.rm = TRUE)) %>%
    pull(numSwitches)
}

###################################
# Common Utils
###################################

ValidateFitbitDataframe <- function(df) {
  # Checks for correct input data frame format such as:
  # df is a non-empty data frame with no inf/-inf values
  #
  # Args:
  #     df <dataframe> : a dataframe of fitbit data (variable schema)
  #
  # Returns:
  #     Error raised if df empty, not data.frame or contains Inf/-Inf

  if (!is.data.frame(df)) {
    stop("Please provide a data frame as input")
  } else if (nrow(df) == 0) {
    stop("Please provide a non-empty data frame")
  } else if (any(is.infinite(unlist(df, use.names = FALSE)))) {
    stop("data frame contains Inf/-Inf values")
  }
}

CustomFilter <- function(df,
                        filterExpr = NULL){
  # Applies a filter on top of data if a filter expression is passed in
  #
  # Args:
  #   df: data.frame (variable schema)
  #   filterExpr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #   Filtered data frame if filterExpr exists and original data frame if filterExpr is NULL

  if (!is.null(filterExpr)) {
    filterExpr <- rlang::parse_expr(filterExpr)
    df <- filter(df, !!filterExpr)
  }
  return(df)
}

CountMinutes <- function(isHigh, consecNb = 5) {
  # Counts number of consecutive instances of active minutes
  #
  # Args:
  #   isHigh: a vector of 0's and 1's representing active minutes
  #   consecNb: int, default 5
  #             minimum number of consec minute to start counting
  #
  # Returns:
  #   an int representing the number of consecutive active minutes

  consecMin <- with(rle(isHigh), lengths[values > 0])
  minTotal <- sum(consecMin[consecMin > consecNb])
  return(minTotal)
}

activeMinutesRegThresh <- function(patientId,date){
    
    patientData <- tbl(conn, 'linkMoresecure') %>%
        filter(fizzyo_hub_id == patientId) %>%
        select(date_recruited,age_recruited) %>%
        collect()
    
    rd = patientData$date_recruited
    ad = patientData$age_recruited

    df = as.integer(date - rd)/365.25
    age = ad + df
    
    df <- dbGetQuery(conn, sprintf("SELECT * FROM \"%s\"", 'age_mvpa_thresh'))
    index = which.min(abs(df$age_recruited - age))
    thresh = df[index,'daily_mvpa_thresh']
    return(thresh)
    
}


ActiveMinutes <- function(df,
                          consecNb = 5,
                          threshold = 120,
                          valCol = "value",
                          sortCols = c("time"),
                          filterExpr = NULL) {
  # Number of values above the active threshold
  #
  # Args:
  #   df: fitbit data.frame (variable schema)
  #     ... | "valCol" <num> | "sortCols"|...
  #   consecNb: int, default 5
  #             minimum number of consecutive minute to start counting
  #   threshold: integer, default 120
  #             threshold for a minute to be considered active
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   sortCols: string vector, default "time" column
  #             columns to use to sort the dataframe
  #   filterExpr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #   active minutes <num>

  valCol <- rlang::ensym(valCol)

  df %>%
    CustomFilter(filterExpr) %>%
    arrange_at(sortCols) %>%
    mutate(isHigh = if_else(!!valCol >= threshold, 1, 0, missing = 0)) %>%
    ungroup() %>%
    do(data.frame(activeMins = CountMinutes(.$isHigh, consecNb))) %>%
    mutate(activeMins = replace_na(activeMins, 0)) %>%
    pull(activeMins)
}

CoefficientOfVariation <- function(df,
                valCol = "value",
                removeZero = TRUE,
                filterExpr = NULL){
  # Coefficient of variation
  #
  # Args:
  #   df: fitbit data.frame (variable schema)
  #     ... | "valCol" <num> | ...
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   removeZero: boolean, default TRUE
  #           if TRUE, filter out all zero values before CoefficientOfVariation calculation
  #   filterExpr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #  Coefficient of variation <num>

  if (!any(is.finite(unlist(df[valCol], use.names = FALSE))) | nrow(df) == 0){
    return(0)
  }

  valCol <- rlang::ensym(valCol)

  if (removeZero){
    df %>%
      filter(!!valCol > 0) -> df
  }

  df %>%
    CustomFilter(filterExpr) %>%
    summarise(sd = sd(!!valCol, na.rm = TRUE),
              mean = mean(!!valCol, na.rm = TRUE)) %>%
    mutate(CoefficientOfVariation = if_else(mean > 0, (sd / mean) * 100, 0, missing = 0)) %>%
    pull(CoefficientOfVariation)
}

###################################
# Combined Utils
###################################

CombineFootstepsHR <- function(steps, heartrate){
  # Combine the footsteps and heartrate together
  #
  # Args:
  #   steps: data frame with minute steps values
  #         userid <chr> | time <POSIXct> | value <num>
  #   heartrate: data frame with heartrate values
  #         userid <chr> | time <POSIXct> | value <num>
  #
  # Returns:
  #  Combined data frame with steps and aggregated hr value per minute
  #       userid <chr> | time <POSIXct> | value_hr <num> | value_steps <num>

  if (nrow(steps) != 0 & nrow(heartrate) != 0){

    steps <- distinct(steps)
    heartrate <- distinct(heartrate)

    heartrate <- heartrate %>%
      group_by(userid,
               date = lubridate::date(time),
               time_hour = lubridate::hour(time),
               time_minute = lubridate::minute(time)) %>%
      summarise(value_hr = mean(value, na.rm = TRUE)) %>%
      ungroup()

    # change the name of the footsteps value field for consistency
    colnames(steps)[which(names(steps) == "value")] <- "value_steps"

    steps %>%
      mutate(date = lubridate::date(time),
             time_hour = lubridate::hour(time),
             time_minute = lubridate::minute(time)) %>%
      full_join(heartrate, by = c("userid", "date", "time_hour", "time_minute")) %>%
      mutate(time = replace_na(as.POSIXct(paste(date, " ", time_hour,
                                                ":", time_minute, ":00", sep = "")))) %>%
      select(-date, -time_hour, -time_minute)
  } else if (nrow(steps) == 0) {

    heartrate <- distinct(heartrate)

    heartrate <- heartrate %>%
      group_by(userid,
               date = lubridate::date(time),
               time_hour = lubridate::hour(time),
               time_minute = lubridate::minute(time)) %>%
      summarise(value_hr = mean(value, na.rm = TRUE)) %>%
      mutate(time = as.POSIXct(paste(date, " ", time_hour,
                                     ":", time_minute, ":00", sep = ""))) %>%
      ungroup() %>%
      select(-date, -time_hour, -time_minute)

    heartrate["value_steps"] <- 0

    return(heartrate)
  } else if (nrow(heartrate) == 0) {

    steps <- distinct(steps)
    colnames(steps)[which(names(steps) == "value")] <- "value_steps"
    steps["value_hr"] <- 0

    return(steps)
  } else {
    stop("two empty data frames were passed to function")
  }
}

StepNormActivityPeriods <- function(df,
                                    valCol = "value_steps",
                                    filterExpr = NULL){
  # Normalised step count for an activity period
  #
  # Args:
  #   df: fitbit data.frame (variable schema)
  #     ... | "valCol" <num> | ...
  #   valCol: string, default "value" column
  #           name of column to perform aggregations on
  #   filterExpr: string, default NULL (no filtering)
  #               filter expression to define the activity period
  #
  # Returns:
  #  Normalised steps count for the period <num>

  valCol <- rlang::ensym(valCol)

  df %>%
    CustomFilter(filterExpr) %>%
    summarise(minutes = n(),
              stepCount = sum(!!valCol, na.rm = TRUE)) %>%
    mutate(stepNorm = if_else(minutes > 0, stepCount / minutes, 0, missing = 0)) %>%
    pull(stepNorm)
}

ActiveMinutesHeartrateSteps <- function(df,
                                        consecNb = 5,
                                        thresholdHr = 120,
                                        thresholdSteps = 100,
                                        colHr = "value_hr",
                                        colSteps = "value_steps",
                                        sortCols = c("time"),
                                        filterExpr = NULL) {
  # Number of values above the active threshold
  #
  # Args:
  #   df: heartrate & steps data.frame (variable schema)
  #   ... | "colHR" <num> | "colSteps" <num>| "sortCols" |...
  #   consecNb: int, default 5
  #             minimum number of consec minute above threshold to start counting
  #   thresholdHr: integer, default 120
  #             HR threshold for a minute to be considered active
  #   thresholdSteps: integer, default 100
  #             Steps threshold for a minute to be considered active
  #   colHr: string, default "value_hr"
  #           name of HR column to perform aggregations on
  #   colSteps: string, default "value_steps
  #           name of HR column to perform aggregations on
  #   sortCols: string vector, default "time" column
  #             columns to use to sort the dataframe
  #
  # Returns:
  #   active minutes column <num>

  colHr <- rlang::ensym(colHr)
  colSteps <- rlang::ensym(colSteps)

  df %>%
    CustomFilter(filterExpr) %>%
    arrange_at(sortCols) %>%
    mutate(isHigh = if_else(!!colHr > thresholdHr & !!colSteps > thresholdSteps,
                            1, 0, missing = 0)) %>%
    ungroup() %>%
    do(data.frame(activeMins = CountMinutes(.$isHigh, consecNb))) %>%
    mutate(activeMins = replace_na(activeMins, 0)) %>%
    pull(activeMins)
}

###################################
# Over-arching featurise functions
###################################

FeaturiseFitbitSteps <- function(df){
  # Feature fitbit footsteps
  #
  # Args:
  #   df: footsteps data.frame
  #       time <POSIXct> | value <num> | userid <chr>
  #
  # Returns:
  #  data.frame containing the featurised data
  #         userid <chr> | date <POSIXct> | stepCount <num> | meanHourlyStepsDay <num> |
  #         activeHours <num> | lowActiveHours <num> | startWindow <POSIXct> |
  #         endWindow <POSIXct>

  ValidateFitbitDataframe(df)

  df %>%
    group_by(userid, date = lubridate::date(time)) %>%
    nest() %>%
    mutate(stepCount = map_dbl(data, StepCount),
           meanHourlyStepsDay = map_dbl(data, MeanHourlySteps),
           activeHours = map_dbl(data, ActiveHoursCount),
           lowActiveHours = map_dbl(data, LowActiveHoursCount),
           activeWindow = map(data, GetActiveWindow)
    ) %>%
    select(-data) %>%
    unnest()
}

FeaturiseFitbitHeartRate <- function(df) {
  # Featurise fitbit heartrate data
  #
  # Args:
  #   df: heartrate data.frame
  #       time <POSIXct> | value <num> | userid <chr>
  #
  # Returns:
  #  data.frame containing the featurised data
  #         userid <chr> | date <POSIXct> | maxHourly <num> | minHourly <num> |
  #         medianHourly <num> | restHrProxy <num> | activeMinutes <num> |
  #         minsWearTotal <num> | minsWearQ1 <num> | minsWearQ2 <num> |
  #         minsWearQ3 <num> | minsWearQ4 <num> | wearPercent <num> |
  #         wearQ1Percent <num> | wearQ2Percent <num> | wearQ3Percent <num> |
  #         wearQ4Percent <num> | gapPercent <num> | gapPercentQ1 <num> |
  #         gapPercentQ2 <num> | gapPercentQ3 <num> | gapPercentQ4 <num> |
  #         wearDuringSleep <boolean> | switch100 <num> | switch120 <num> |
  #         switch140 <num>

  ValidateFitbitDataframe(df)

  df %>%
    group_by(userid,
             date = lubridate::date(time),
             time_hour = lubridate::hour(time),
             time_minute = lubridate::minute(time)) %>%
    summarise(minuteAvg = mean(value, na.rm = TRUE)) %>%
    group_by(userid, date) %>%
    nest() %>%
    mutate(activeMins = map_dbl(data,
                                ActiveMinutes,
                                valCol = "minuteAvg",
                                sortCols = c("time_hour", "time_minute")),
           restingHrProxy = map_dbl(data,
                                    RestingHrProxy,
                                    valCol = "minuteAvg"),
           summaryVals = map(data,
                             HourlyInterval,
                             valCol = "minuteAvg",
                             grpExpr = "time_hour")) %>%
    # wearing time features
    mutate(minsWearTotal = map_dbl(data,
                                   MinuteWearCount,
                                   hrCol = "minuteAvg"),
           minsWearQ1 = map_dbl(data,
                                MinuteWearCount,
                                hrCol = "minuteAvg",
                                filterExpr = filterHrQ1),
           minsWearQ2 = map_dbl(data,
                                MinuteWearCount,
                                hrCol = "minuteAvg",
                                filterExpr = filterHrQ2),
           minsWearQ3 = map_dbl(data,
                                MinuteWearCount,
                                hrCol = "minuteAvg",
                                filterExpr = filterHrQ3),
           minsWearQ4 = map_dbl(data,
                                MinuteWearCount,
                                hrCol = "minuteAvg",
                                filterExpr = filterHrQ4)) %>%
    mutate(wearPercent = (minsWearTotal / kMinsDay) * 100,
           wearQ1Percent = (minsWearQ1 / kMinsQuadrant) * 100,
           wearQ2Percent = (minsWearQ2 / kMinsQuadrant) * 100,
           wearQ3Percent = (minsWearQ3 / kMinsQuadrant) * 100,
           wearQ4Percent = (minsWearQ4 / kMinsQuadrant) * 100) %>%
    mutate(gapPercent = 100 - wearPercent,
           gapPercentQ1 = 100 - wearQ1Percent,
           gapPercentQ2 = 100 - wearQ2Percent,
           gapPercentQ3 = 100 - wearQ3Percent,
           gapPercentQ4 = 100 - wearQ4Percent) %>%
    mutate(wearDuringSleep = if_else(gapPercentQ1 > 25, 0, 1)) %>%
    # threshold switches
    mutate(switch100 = map_dbl(data,
                               CountSwitches,
                               valCol = "minuteAvg",
                               threshold = 100),
           switch120 = map_dbl(data,
                               CountSwitches,
                               valCol = "minuteAvg",
                               threshold = 120),
           switch140 = map_dbl(data,
                               CountSwitches,
                               valCol = "minuteAvg",
                               threshold = 140)) %>%
    select(-data) %>%
    unnest()
}

FeaturiseFitbitCombined <- function(df,thresh,RegressionMVPAPrev15) {
  # Featurise fitbit combine data (steps and HR)
  #
  # Args:
  #   df: combined fitbit data.frame
  #       time <POSIXct> | value_hr <num> | value_steps <num> | userid <chr>
  #
  # Returns:
  #  data.frame containing the featurised data
  #         userid <chr> | date <POSIXct> | stepCount <num> | stepCountQ2 <num> |
  #         stepCountQ3 <num> | stepCountQ4 <num> | stepCountQ234 <num> | meanHourlyStepsDay <num> |
  #         activeHours <num> | lowActiveHours <num> | activeMinsSteps0 <dbl> |
  #         activeMinsSteps2 <num> | activeMinsSteps5 <num> | activeMinsSteps10 <num> |
  #         activeMinsSteps20 <num> | activeMinsSteps0AP <num> | CoefficientOfVariationStep <num> |
  #         CoefficientOfVariationStepQ1 <num> | CoefficientOfVariationStepQ2 <num>|
  #         CoefficientOfVariationStepQ3 <num>| CoefficientOfVariationStepQ4 <num>| 
  #         CoefficientOfVariationStepQ234 <num>
  #         restingHrProxy <num>| activeMinsHr0 <num>|
  #         activeMinsHr2 <num>| activeMinsHr5 <num>| activeMinsHr10 <num>|
  #         activeMinsHr20 <num>| activeMinsHr0AP <num> | CoefficientOfVariationHr <num>| 
  #         CoefficientOfVariationHrQ1 <num>|
  #         CoefficientOfVariationHrQ2 <num>| CoefficientOfVariationHrQ3 <num>|
  #         CoefficientOfVariationHrQ4 <num> | CoefficientOfVariationHrQ234 <num> | minsWearTotal <num> |
  #         minsWearQ1 <num> | minsWearQ2 <num> | minsWearQ3 <num> |
  #         minsWearQ4 <num> | wearPercent <num> | wearQ1Percent <num> |
  #         wearQ2Percent <num> | wearQ3Percent <num> | wearQ4Percent <num> |
  #         gapPercent <num> | gapPercentQ1 <num> | gapPercentQ2 <num> | gapPercentQ3 <num> |
  #         gapPercentQ4 <num> | wearDuringSleep <num> | switch100 <num> | switch120 <num> |
  #         switch140 <num> | activeMinsHrSteps0 <num> | activeMinsHrSteps2 <num> |
  #         activeMinsHrSteps5 <num> | activeMinsHrSteps10 <num> | activeMinsHrSteps20 <num> |
  #         stepCountNorm <num> | stepCountNormQ2 <num> | stepCountNormQ3 <num> |
  #         stepCountNormQ4 <num> | stepNormModerate <num> | stepNormVigorous <num> |
  #         startWindow <POSIXct> | endWindow <POSIXct> | maxHourly <num> |
  #         minHourly <num> | medianHourly <num>

  df %>%
    group_by(userid, date = lubridate::date(time)) %>%
    nest() %>%
    # steps in total and per quadrants
    mutate(stepCount = map_dbl(data,
                               StepCount,
                               valCol = "value_steps"),
           stepCountQ2 = map_dbl(data,
                                 StepCount,
                                 valCol = "value_steps",
                                 filterExpr = filterCombQ2),
           stepCountQ3 = map_dbl(data,
                                 StepCount,
                                 valCol = "value_steps",
                                 filterExpr = filterCombQ3),
           stepCountQ4 = map_dbl(data,
                                 StepCount,
                                 valCol = "value_steps",
                                 filterExpr = filterCombQ4),
           stepCountQ234 = map_dbl(data,
                                 StepCount,
                                 valCol = "value_steps",
                                 filterExpr = filterCombQ234)) %>%
    mutate(meanHourlyStepsDay = map_dbl(data,
                                        MeanHourlySteps,
                                        valCol = "value_steps"),
           activeHours = map_dbl(data,
                                 ActiveHoursCount,
                                 valCol = "value_steps"),
           lowActiveHours = map_dbl(data,
                                    LowActiveHoursCount,
                                    valCol = "value_steps"),
           activeWindow = map(data,
                              GetActiveWindow,
                              valCol = "value_steps")) %>%
    # active minutes steps
    mutate(activeMinsSteps0 = map_dbl(data,
                                  ActiveMinutes,
                                  consecNb = 0,
                                  threshold = 100,
                                  valCol = "value_steps",
                                  sortCols = c("time")),
           activeMinsSteps2 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 2,
                                      threshold = 100,
                                      valCol = "value_steps",
                                      sortCols = c("time")),
           activeMinsSteps5 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 5,
                                      threshold = 100,
                                      valCol = "value_steps",
                                      sortCols = c("time")),
           activeMinsSteps10 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 10,
                                      threshold = 100,
                                      valCol = "value_steps",
                                      sortCols = c("time")),
           activeMinsSteps20 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 20,
                                      threshold = 100,
                                      valCol = "value_steps",
                                      sortCols = c("time")),
           activeMinsSteps0AP = map_dbl(data,
                                  ActiveMinutes,
                                  consecNb = 0,
                                  threshold = 100,
                                  valCol = "value_steps",
                                  sortCols = c("time"),
                                  filterExpr = filterCombQ234)) %>%
    # coefficient of variation
    mutate(CoefficientOfVariationStep = map_dbl(data,
                             CoefficientOfVariation,
                             valCol = "value_steps"),
           CoefficientOfVariationStepQ1 = map_dbl(data,
                               CoefficientOfVariation,
                               valCol = "value_steps",
                               filterExpr = filterCombQ1),
           CoefficientOfVariationStepQ2 = map_dbl(data,
                               CoefficientOfVariation,
                               valCol = "value_steps",
                               filterExpr = filterCombQ2),
           CoefficientOfVariationStepQ3 = map_dbl(data,
                               CoefficientOfVariation,
                               valCol = "value_steps",
                               filterExpr = filterCombQ3),
           CoefficientOfVariationStepQ4 = map_dbl(data,
                               CoefficientOfVariation,
                               valCol = "value_steps",
                               filterExpr = filterCombQ4),
           CoefficientOfVariationStepQ234 = map_dbl(data,
                               CoefficientOfVariation,
                               valCol = "value_steps",
                               filterExpr = filterCombQ234)) %>%
    # heartrate only feature
    mutate(restingHrProxy = map_dbl(data,
                                    RestingHrProxy,
                                    valCol = "value_hr"),
           restingHr722 = map_dbl(data,
                                    RestingHrProxy,
                                    valCol = "value_hr",
                                    timeFilter = TRUE),
           summaryVals = map(data,
                             HourlyInterval,
                             valCol = "value_hr",
                             grpExpr = "lubridate::hour(time)")) %>%
    # active minutes hr
    
    mutate(RegressionMVPAThreshold = thresh)%>%
    mutate(RegressionMVPAPrev15 = RegressionMVPAPrev15)%>%
    
    
    mutate(activeMinsHrRegressionThresh = map_dbl(data,
                                      ActiveMinutes,
                                      threshold = RegressionMVPAThreshold,
                                      consecNb = 0,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHrRegressionThresh1 = map_dbl(data,
                                      ActiveMinutes,
                                      threshold = RegressionMVPAThreshold,
                                      consecNb = 1,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHrRegressionThresh2 = map_dbl(data,
                                      ActiveMinutes,
                                      threshold = RegressionMVPAThreshold,
                                      consecNb = 2,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHrRegressionThresh5 = map_dbl(data,
                                      ActiveMinutes,
                                      threshold = RegressionMVPAThreshold,
                                      consecNb = 5,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHrRegressionThresh10 = map_dbl(data,
                                      ActiveMinutes,
                                      threshold = RegressionMVPAThreshold,
                                      consecNb = 10,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHrRegressionThresh20 = map_dbl(data,
                                      ActiveMinutes,
                                      threshold = RegressionMVPAThreshold,
                                      consecNb = 20,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHr0 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 0,
                                      valCol = "value_hr",
                                      sortCols = c("time")),    
           activeMinsHr1 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 1,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHr2 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 2,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHr5 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 5,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHr10 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 10,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHr20 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 20,
                                      valCol = "value_hr",
                                      sortCols = c("time")),
           activeMinsHr0AP = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 0,
                                      valCol = "value_hr",
                                      sortCols = c("time"),
                                      filterExpr = filterCombQ234),
           activeMinsHrThreshAP = map_dbl(data,
                                      ActiveMinutes,
                                      threshold = RegressionMVPAThreshold,
                                      consecNb = 0,
                                      valCol = "value_hr",
                                      sortCols = c("time"),
                                      filterExpr = filterCombQ234)) %>%
    # coefficient of variation
    mutate(CoefficientOfVariationHr = map_dbl(data,
                           CoefficientOfVariation,
                           valCol = "value_hr",
                           removeZero = FALSE),
           CoefficientOfVariationHrQ1 = map_dbl(data,
                             CoefficientOfVariation,
                             removeZero = FALSE,
                             valCol = "value_hr",
                             filterExpr = filterCombQ1),
           CoefficientOfVariationHrQ2 = map_dbl(data,
                             CoefficientOfVariation,
                             removeZero = FALSE,
                             valCol = "value_hr",
                             filterExpr = filterCombQ2),
           CoefficientOfVariationHrQ3 = map_dbl(data,
                             CoefficientOfVariation,
                             removeZero = FALSE,
                             valCol = "value_hr",
                             filterExpr = filterCombQ3),
           CoefficientOfVariationHrQ4 = map_dbl(data,
                             CoefficientOfVariation,
                             removeZero = FALSE,
                             valCol = "value_hr",
                             filterExpr = filterCombQ4),
           CoefficientOfVariationHrQ234 = map_dbl(data,
                             CoefficientOfVariation,
                             removeZero = FALSE,
                             valCol = "value_hr",
                             filterExpr = filterCombQ234)) %>%
    # wearing time features
    mutate(minsWearTotal = map_dbl(data,
                                   MinuteWearCount),
           minsWear623 = map_dbl(data,
                                MinuteWearCount,
                                filterExpr = filterCombQ5),
           minsWear722 = map_dbl(data,
                                MinuteWearCount,
                                filterExpr = filterCombQ6),
           minsWearQ1 = map_dbl(data,
                                MinuteWearCount,
                                filterExpr = filterCombQ1),
           minsWearQ2 = map_dbl(data,
                                MinuteWearCount,
                                filterExpr = filterCombQ2),
           minsWearQ3 = map_dbl(data,
                                MinuteWearCount,
                                filterExpr = filterCombQ3),
           minsWearQ4 = map_dbl(data,
                                MinuteWearCount,
                                filterExpr = filterCombQ4)) %>%
    mutate(minsWearQ234 = map_dbl(data,
                                MinuteWearCount,
                                filterExpr = filterCombQ234)) %>%
    mutate(wearPercent = (minsWearTotal / kMinsDay) * 100,
           wearQ1Percent = (minsWearQ1 / kMinsQuadrant) * 100,
           wearQ2Percent = (minsWearQ2 / kMinsQuadrant) * 100,
           wearQ3Percent = (minsWearQ3 / kMinsQuadrant) * 100,
           wearQ4Percent = (minsWearQ4 / kMinsQuadrant) * 100,
           wearQ234Percent = (minsWearQ234 / kMinsActiveQs) * 100) %>%
    mutate(gapPercent = 100 - wearPercent,
           gapPercentQ1 = 100 - wearQ1Percent,
           gapPercentQ2 = 100 - wearQ2Percent,
           gapPercentQ3 = 100 - wearQ3Percent,
           gapPercentQ4 = 100 - wearQ4Percent) %>%
    mutate(wearDuringSleep = if_else(gapPercentQ1 > 25, 0, 1)) %>%
    # threshold switches
    mutate(switchThresh = map_dbl(data,
                               CountSwitches,
                               valCol = "value_hr",
                               sortCols = "time",
                               threshold = thresh),
           switch100 = map_dbl(data,
                               CountSwitches,
                               valCol = "value_hr",
                               sortCols = "time",
                               threshold = 100),
           switch120 = map_dbl(data,
                               CountSwitches,
                               valCol = "value_hr",
                               sortCols = "time",
                               threshold = 120),
           switch140 = map_dbl(data,
                               CountSwitches,
                               valCol = "value_hr",
                               sortCols = "time",
                               threshold = 140)) %>%
    # combine hr and steps active minutes
    mutate(activeMinsHrStepsThresh = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        thresholdHr = thresh,
                                        consecNb = 0),
           activeMinsHrStepsThresh2 = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        thresholdHr = thresh,
                                        consecNb = 2),
           activeMinsHrStepsThresh5 = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        thresholdHr = thresh,
                                        consecNb = 5),
           activeMinsHrStepsThresh10 = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        thresholdHr = thresh,
                                        consecNb = 10),
           activeMinsHrStepsThresh20 = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        thresholdHr = thresh,
                                        consecNb = 20),
           activeMinsHrSteps0 = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        consecNb = 0),
           activeMinsHrSteps2 = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        consecNb = 2),
           activeMinsHrSteps5 = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        consecNb = 5),
           activeMinsHrSteps10 = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        consecNb = 10),
           activeMinsHrSteps20 = map_dbl(data,
                                        ActiveMinutesHeartrateSteps,
                                        consecNb = 20)) %>%
    # normalised step count
    mutate(stepCountNorm = if_else(minsWearTotal == 0, 0, stepCount / minsWearTotal, missing = 0),
           stepCountNormQ2 = if_else(minsWearQ2 == 0, 0, stepCountQ2 / minsWearQ2, missing = 0),
           stepCountNormQ3 = if_else(minsWearQ3 == 0, 0, stepCountQ3 / minsWearQ3, missing = 0),
           stepCountNormQ4 = if_else(minsWearQ4 == 0, 0, stepCountQ4 / minsWearQ4, missing = 0)) %>%
    # step count during active periods
    
    mutate(stepNormModerateThresh = map_dbl(data,
                                      StepNormActivityPeriods,
                                      filterExpr = paste(paste("value_hr >= ", thresh, sep = '')," & value_hr < 140",sep  ='')),
           stepNormModVigThresh = map_dbl(data,
                                      StepNormActivityPeriods,
                                      filterExpr = paste("value_hr >= ", thresh, sep = '')),
           stepNormSedentaryThresh = map_dbl(data,
                                      StepNormActivityPeriods,
                                      filterExpr = paste("value_hr < ", thresh, sep = '')),
           stepNormModerate = map_dbl(data,
                                      StepNormActivityPeriods,
                                      filterExpr = "value_hr >= 120 & value_hr < 140"),
           stepNormVigorous = map_dbl(data,
                                      StepNormActivityPeriods,
                                      filterExpr = "value_hr >= 140"),
           stepNormModVig = map_dbl(data,
                                      StepNormActivityPeriods,
                                      filterExpr = "value_hr >= 120"),
           stepNormSedentary = map_dbl(data,
                                      StepNormActivityPeriods,
                                      filterExpr = "value_hr < 120")) %>%
    mutate(GovRec = ifelse(activeMinsHr1 > 60, 1, 0)) %>%
    select(-data) %>%
    unnest()
}