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
## - CustomFilter: Applies a filter on top of data if a filter expression is given
## - CountMinutes: Count number of consecutive active minutes
## - ActiveMinutes: Number of minutes with HR > threshold
##                  after n consecutive minutes with HR > threshold
## - CoefficientOfVariation: Calculates Coefficient of variation
## - CombineFootstepsHR: Combine the footsteps and heartrate together
## - activeMinutesRegThresh: get threshold data per person per day
## - switchTh: count number of times hr data goes above and below threshold value
## - consecutive: count number of timestamps in a vector that have at least n consecutive minutes
## - stepNorm: average steps per minute within specific hr range
## - mvpaCountNeighbourTimesPrev: count mvpa where mvpa requires steps in previous 15 minutes
## - FeaturiseFitbitCombined: Featurise fitbit combine data (steps and HR)
################################################################################

# define constants for time intervals
kMinsDay <- 24 * 60
kMinsQuadrant <- kMinsDay / 4
kMinsActiveQs <- kMinsQuadrant * 3
stepActive <- 70
fs_column <- "value_steps"
hr_column <- "value_hr"
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
filterCombQ6 <- "lubridate::hour(time) >= 11 & lubridate::hour(time) < 21"
filterCombQ234 <- "lubridate::hour(time) >= 6 & lubridate::hour(time) < 24"

apst <- 8
aped <- 21

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
        filter(lubridate::hour(time) >= 11 & lubridate::hour(time) < 21) %>%
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


###################################
# Common Utils
###################################

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
  minTotal <- sum(consecMin[consecMin >= consecNb])
  return(minTotal)
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


activeMinutesRegThresh <- function(patientId, d, threshTable, conn){

    # get threshold data for individual day
    #
    # Args:
    #   patientId: string for userid
    #   d : date of day of data
    #   threshTable: DB table name with threshold data
    #
    # Returns:
    #  tibble of closest thresh data based on day and person
    #     thresh <dbl> | threshLow <dbl> | threshHigh <dbl>
        
    patientData <- tbl(conn,threshTable) %>%
        filter(userid == patientId) %>%
        mutate(dateDiff = as.integer(abs(date - d))) %>%
        filter(dateDiff == min(dateDiff,na.rm = TRUE)) %>%
        collect()
        
    return (patientData)
}


switchTh <- function (vals,thresh) # function that takes in minute average dataframe for a day for hr data
{
    # count switches for hr data and a specific threshold value
    #
    # Args:
    #   vals: vector of hr data aggregated per minute
    #   thresh: numeric threshold value 
    #
    # Returns:
    #  integer of number of times hr goes above and below thresh
    
    if(length(vals) > 1){

        vals[is.na(vals)] <- 0

        switchThresh = 0
        
        for(i in 1:(length(vals)-1)){
        
            if((vals[i] < thresh) & (vals[i + 1] > thresh))
            {
                switchThresh = switchThresh + 1
            }
            
        }
        return (switchThresh)
    }else{
        return(0)
    }
}

consecutive <- function(time,consec){

    # count switches for hr data and a specific threshold value
    #
    # Args:
    #   time: vector of per minute timestamps
    #   consec: numeric for number of consecutive values needed to begin counting
    #
    # Returns:
    #  numeric of number of minutes in mvpa with consecutive value threshold met
    
    if(length(time) == 0){
        return(0)
    }
    
    if(length(time) == 1){
        return(0)
    }
    time = sort(time)
    count = 0
    tc = 0
    for ( i in 1:(length(time)-1)){
        curr = time[i]
        nex = time[i + 1]
        dif = abs(as.integer(difftime(nex,curr,units = 'mins')))
        if(dif == 1)
        {
            if(tc == 0)
            {
                tc  = tc + 2
            }else{
                tc = tc + 1
            }
            if(i == (length(time) - 1) & tc >= consec){
                count = count + tc
                tc = 0 
            }
        }
        if(dif != 1 & tc < consec){
            tc = 0
        }
        if(dif != 1 & tc >= consec){
            count = count + tc
            tc = 0
        }
    }
    return(count)
}

stepNorm <- function(mvpaTimes,fs,fs_column = 'value_steps') 
{
    # average steps in specfic period of time
    #
    # Args:
    #   mvpaTimes: vector of per minute timestamps
    #   fs: data frame with minute steps values
    #         userid <chr> | time <POSIXct> | value <num>
    #
    # Returns:
    #  numeric of average steps for minutes in mvpa
    
    matchActiveHRSteps = intersect(mvpaTimes,fs$time)
    matchActiveHRSteps = as_datetime(matchActiveHRSteps)
    matchSteps = fs[fs$time %in% matchActiveHRSteps,]
    return(mean(matchSteps %>% pull(!!as.name(fs_column)),na.rm = TRUE))
    
}

mvpaCountNeighbourTimesPrev <- function(activeHr,
                                        fs,
                                        fs_column = 'value_steps',
                                        windowSize = 15){

    # count minutes in mvpa
    # for each minute in activeHr
    # check if any steps in previous 15 minutes
    # if yes, then classify as mvpa
    # else, not a mvpa
    
    # Args:
    #   mvpaTimes: vector of per minute timestamps
    #   fs: data frame with minute steps values
    #         userid <chr> | time <POSIXct> | value <num>
    #   activeHr: data frame with heartrate values > thresh
    #         userid <chr> | time <POSIXct> | value <num>
    #    windowSize: intger informing how many minutes 
    #                   to look back for steps
    #
    # Returns:
    #  vector of timestamps for minutes in mvpa
    
    timesIn = c()

    count = 0 
    for (t in unique(activeHr$time)){
        timeVal = as_datetime(t)

        prv = timeVal - (60*windowSize)
        fsT = fs[(fs$time >= prv) & (fs$time <= timeVal),]
        maxS = max(fsT[fs_column], na.rm = TRUE)
        if(maxS > 0){
            count = count + 1
            timesIn = c(timesIn,timeVal)
        }
        
    }
    
    timesIn <- if (length(timesIn)>0) as_datetime(timesIn, tz = 'UTC') else c()

    return(timesIn)
}

FeaturiseFitbitCombined <- function(fitbitData, conn, threshTable) {
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
  #         ...
  featurisedFitbitData <- fitbitData %>%
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
                                  threshold = stepActive,
                                  valCol = "value_steps",
                                  sortCols = c("time")),
           activeMinsSteps2 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 2,
                                      threshold = stepActive,
                                      valCol = "value_steps",
                                      sortCols = c("time")),
           activeMinsSteps5 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 5,
                                      threshold = stepActive,
                                      valCol = "value_steps",
                                      sortCols = c("time")),
           activeMinsSteps10 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 10,
                                      threshold = stepActive,
                                      valCol = "value_steps",
                                      sortCols = c("time")),
           activeMinsSteps20 = map_dbl(data,
                                      ActiveMinutes,
                                      consecNb = 20,
                                      threshold = stepActive,
                                      valCol = "value_steps",
                                      sortCols = c("time")),
           activeMinsSteps0AP = map_dbl(data,
                                  ActiveMinutes,
                                  consecNb = 0,
                                  threshold = stepActive,
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
           restingHr1121 = map_dbl(data,
                                    RestingHrProxy,
                                    valCol = "value_hr",
                                    timeFilter = TRUE),
           summaryVals = map(data,
                             HourlyInterval,
                             valCol = "value_hr",
                             grpExpr = "lubridate::hour(time)")) %>%

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
           minsWear1121 = map_dbl(data,
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
    
    mutate(stepCountNorm = if_else(minsWearTotal == 0, 0, stepCount / minsWearTotal, missing = 0),
           stepCountNormQ2 = if_else(minsWearQ2 == 0, 0, stepCountQ2 / minsWearQ2, missing = 0),
           stepCountNormQ3 = if_else(minsWearQ3 == 0, 0, stepCountQ3 / minsWearQ3, missing = 0),
           stepCountNormQ4 = if_else(minsWearQ4 == 0, 0, stepCountQ4 / minsWearQ4, missing = 0)) %>%

    select(-data) %>%
    unnest()

    threshData  = activeMinutesRegThresh(patient,d,threshTable, conn)
    thresh = (threshData %>% pull(thresh14))[1]
    threshLow = (threshData %>% pull(thresh14_low))[1]
    threshHigh = (threshData %>% pull(thresh14_high))[1]

    featurisedFitbitData$thresh = thresh
    featurisedFitbitData$threshLow = threshLow
    featurisedFitbitData$threshHigh = threshHigh
    
    activeHr = fitbitData %>% filter(!!as.name(hr_column) > thresh)
    fsData = fitbitData %>% select(!!as.name(fs_column),time)
    timesIn <- mvpaCountNeighbourTimesPrev(activeHr,fsData,fs_column = fs_column, windowSize = 15)
    NonActiveHr = fitbitData %>% filter(!!as.name(hr_column) <= thresh)
    featurisedFitbitData$mvpa_15PrevMethod = length(timesIn)
    featurisedFitbitData$mvpa_15PrevMethod2 = consecutive(timesIn,2)
    featurisedFitbitData$mvpa_15PrevMethod3 = consecutive(timesIn,3)
    featurisedFitbitData$mvpa_15PrevMethod5 = consecutive(timesIn,5)
    featurisedFitbitData$mvpa_15PrevMethod10 = consecutive(timesIn,10)
    featurisedFitbitData$mvpa_15PrevMethod20 = consecutive(timesIn,20)
    featurisedFitbitData$mvpa_15PrevMethodAP_8_21 = length(timesIn[hour(timesIn) >= apst & hour(timesIn) <= aped ])
    featurisedFitbitData$switchThresh = switchTh(fitbitData %>% pull(!!as.name(hr_column)),thresh)
    featurisedFitbitData$switchThreshLow = switchTh(fitbitData %>% pull(!!as.name(hr_column)),threshLow)
    featurisedFitbitData$switchThreshHigh = switchTh(fitbitData %>% pull(!!as.name(hr_column)),threshHigh)
    
    activeSteps = fitbitData %>% filter(!!as.name(fs_column) > stepActive)
    activeMinsHrSteps = as_datetime(intersect(timesIn,activeSteps$time), tz = 'UTC')
    featurisedFitbitData$activeMinsHrStepsThresh = length(activeMinsHrSteps)
    featurisedFitbitData$activeMinsHrStepsThresh2 = consecutive(activeMinsHrSteps,2)
    featurisedFitbitData$activeMinsHrStepsThresh3 = consecutive(activeMinsHrSteps,3)
    featurisedFitbitData$activeMinsHrStepsThresh5 = consecutive(activeMinsHrSteps,5)
    featurisedFitbitData$activeMinsHrStepsThresh10 = consecutive(activeMinsHrSteps,10)
    featurisedFitbitData$activeMinsHrStepsThresh20 = consecutive(activeMinsHrSteps,20)

    featurisedFitbitData$stepNormThreshMVPA = stepNorm(timesIn,fsData)
    featurisedFitbitData$stepSedentaryThreshMVPA = stepNorm(NonActiveHr$time,fsData)    
    return(featurisedFitbitData)
}