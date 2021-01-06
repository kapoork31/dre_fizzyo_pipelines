################################################################################
## This file contains the following fitbit feature extraction functions:
##
## count_steps : Total number of steps in the dataframe
## calc_mean_hourly_steps: Average of hourly number of steps
## calc_active_hours_count: Calculate number of active hours
## calc_low_active_hours_count: Calculate number of low activity hours
## get_active_aindow: Return the timestamp of the start and end of activity
## calc_resting_hr_proxy: Calculate resting heartrate features
## calc_hourly_interval: Extract hourly min, max, and median heartrate metrics
## calc_minute_wear_count: Number of minutes the fitbit was worn based on
##                    the filter condition
## custom_filter: Applies a filter on top of data if a filter expression is given
## count_minutes: Count number of consecutive active minutes
## calc_active_minutes: Number of minutes with HR grt threshold
##                  after n consecutive minutes with HR grt threshold
## calc_coefficient_of_variation: Calculates Coefficient of variation
## combine_foot_steps_hr: Combine the footsteps and heartrate together
## get_active_minutes_reg_thresh: get threshold data per person per day
## calc_switch_th: count number of times hr data goes above and below threshold value
## calc_consecutive: count number of timestamps in a vector that have at least n consecutive minutes
## calc_step_norm: average steps per minute within specific hr range
## calc_mvpa_count_neighbour_times_prev: count mvpa where mvpa requires steps in previous 15 minutes
## featurise_fitbit_combined: Featurise fitbit combine data (steps and HR)
################################################################################

# define constants for time intervals
k_mins_day <- 24 * 60
k_mins_quadrant <- k_mins_day / 4
k_mins_active_qs <- k_mins_quadrant * 3
step_active <- 70
fs_column <- "value_steps"
hr_column <- "value_hr"
# quadrants filter expressions for heartrate data
filter_hr_q1 <- "time_hour < 6"
filter_hr_q2 <- "time_hour >= 6 & time_hour < 12"
filter_hr_q3 <- "time_hour >= 12 & time_hour < 18"
filter_hr_q4 <- "time_hour >= 18"

thresh14 <- "thresh14"
thresh14_low <- "thresh14_low"
thresh14_high <- "thresh14_high"

# quadrants filter expressions for combined hr and steps
filter_comb_q1 <- "lubridate::hour(time) < 6"
filter_comb_q2 <- "lubridate::hour(time) >= 6 & lubridate::hour(time) < 12"
filter_comb_q3 <- "lubridate::hour(time) >= 12 & lubridate::hour(time) < 18"
filter_comb_q4 <- "lubridate::hour(time) >= 18"
filter_comb_5 <- "lubridate::hour(time) >= 6 & lubridate::hour(time) < 23"
filter_comb_6 <- "lubridate::hour(time) >= 11 & lubridate::hour(time) < 21"
filter_comb_q234 <- "lubridate::hour(time) >= 6 & lubridate::hour(time) < 24"

apst <- 8
aped <- 21

###################################
# FootSteps Specific Utils
###################################

count_steps <- function(df,
                       val_col = "value",
                       filter_expr = NULL){
  # Total number of steps in the data.frame
  #
  # Args:
  #   df: data.frame containing a column "value" to sum
  #     ... | "val_col" <num> | ... -> variable schema
  #   val_col: string, default "value" column
  #           name of column to perform aggregations on
  #   filter_expr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #   step_count <num>

  if (val_col %in% names(df)){

    val_col <- rlang::ensym(val_col)

    df %>%
      custom_filter(filter_expr) %>%
      summarise(step_count = sum(!!val_col, na.rm = TRUE)) %>%
      pull(step_count)
  } else {
    stop("data.frame does not contain the column specified")
  }
}


calc_mean_hourly_steps_day <- function(df,
                            val_col = "value",
                            time_col = "time"){
  # Average of hourly number of steps
  #
  # Args:
  #   df: data.frame with time, value columns (variable schema)
  #   ... | "val_col" <num> | "time_col" <POSIXCt>|...
  #   val_col: string, default "value" column
  #           name of column to perform aggregations on
  #   time_col: string, default "time" column
  #           name of the column which contains the timestamps for values
  #
  # Returns:
  #   mean_hourly_steps_day <num>

  if (val_col %in% names(df) & time_col %in% names(df)){

    val_col <- rlang::ensym(val_col)
    time_col <- rlang::ensym(time_col)

    df %>%
      group_by(hour = lubridate::hour(!!time_col)) %>%
      summarise(step_hour = sum(!!val_col, na.rm = TRUE)) %>%
      summarise(mean_hourly_steps_day = mean(step_hour, na.rm = TRUE)) %>%
      ungroup() %>%
      pull(mean_hourly_steps_day)
  } else {
    stop("data.frame does not contain the columns specified")
  }
}

calc_active_hours_count <- function(df,
                             val_col = "value",
                             time_col = "time",
                             active_threshold = 500){
  # Calculate number of active hours
  #
  # Args:
  #   df: data.frame with time, value columns (variable schema)
  #     ... | "val_col" <num> | "time_col" <POSIXCt>|...
  #   val_col: string, default "value" column
  #           name of column to perform aggregations on
  #   time_col: string, default "time" column
  #           name of the column which contains the timestamps for values
  #   active_threshold: numeric, minimum number of steps for a hour to be active
  #
  # Returns:
  #   number of active hours <num>

  if (val_col %in% names(df) & time_col %in% names(df)){

    val_col <- rlang::ensym(val_col)
    time_col <- rlang::ensym(time_col)

    df %>%
      group_by(hour = lubridate::hour(!!time_col)) %>%
      summarise(step_hour = sum(!!val_col, na.rm = TRUE)) %>%
      filter(step_hour > active_threshold) %>%
      summarise(active_hours = n()) %>%
      ungroup() %>%
      pull(active_hours)
  } else {
    stop("data.frame does not contain the columns specified")
  }
}

calc_low_active_hours_count <- function(df,
                                val_col = "value",
                                time_col = "time",
                                low_activity_threshold = 500){
  # Calculate number of low activity hours
  #
  # Args:
  #   df: data.frame with time, value columns (variable schema)
  #     ... | "val_col" <num> | "time_col" <POSIXCt>|...
  #   val_col: string, default "value" column
  #           name of column to perform aggregations on
  #   time_col: string, default "time" column
  #           name of the column which contains the timestamps for values
  #   low_activity_threshold: numeric, max number of steps
  #               for a hour to have low activity
  #
  # Returns:
  #   number of low activity hours <num>

  if (val_col %in% names(df) & time_col %in% names(df)){

    val_col <- rlang::ensym(val_col)
    time_col <- rlang::ensym(time_col)

    df %>%
      group_by(hour = lubridate::hour(!!time_col)) %>%
      summarise(step_hour = sum(!!val_col, na.rm = TRUE)) %>%
      filter(step_hour <= low_activity_threshold & step_hour > 0) %>%
      summarise(low_active_hours = n()) %>%
      ungroup() %>%
      pull(low_active_hours)
  } else {
    stop("data.frame does not contain the columns specified")
  }
}

get_active_window <- function(df,
                            val_col = "value",
                            time_col = "time",
                            sleep_Thres = 25,
                            awake_thres = 5){
  # Get the activity window for the day
  #
  # Args:
  #   df: data frame with footsteps values (variable schema)
  #     ... | "val_col" <num> | "time_col" <POSIXCt>|...
  #   val_col: string, default "value" column
  #           name of column to perform aggregations on
  #   time_col: string, default "time" column
  #           name of the column which contains the timestamps for values
  #   sleep_Thres: integer, default 25
  #               min number of steps to count when in sleep period
  #   awake_thres: integer, default 5
  #             min number of steps to count when in awake period
  #
  # Returns:
  #   data frame with start and end timestamps of activity
  #     start_window <POSIXct> | end_window <POSIXct>

  time_col <- rlang::ensym(time_col)
  val_col <- rlang::ensym(val_col)

  df %>%
    filter(lubridate::hour(!!time_col) >= 6
           | (lubridate::hour(!!time_col) <= 6 &
                !!val_col > sleep_Thres)) %>%
    filter(!!val_col > awake_thres) %>%
    arrange(!!time_col) %>%
    summarise(start_window = first(!!time_col),
              end_window = last(!!time_col)) %>%
    select(start_window, end_window)

}

###################################
# Heartrate Specific Utils
###################################

calc_resting_hr_proxy <- function(df,
                           val_col = "value",
                           by_group_sort_flag = FALSE,
                           time_filter = FALSE) {
  # Calculate resting heartrate proxy
  #
  # Args:
  #   df: heartrate data.frame (variable schema)
  #     ... | "val_col" <num> | ...
  #   val_col: string, default "value" column
  #           name of column to perform aggregations on
  #   by_group_sort_flag: boolean, default FALSE
  #            if TRUE, sort based on group_by groups
  #   time_filter: boolean, default False
  #            if TRUE, only look for rhr between 11-21
  # Returns:
  #   resting heartrate column <num>

  if (!any(is.finite(unlist(df[val_col], use.names = FALSE))) | nrow(df) == 0){
    return(0)
  }

  val_col_sym <- rlang::ensym(val_col)
  if (time_filter == FALSE){

      df %>%
        arrange_at(val_col, .by_group = by_group_sort_flag) %>%
        head(., 5) %>%
        ungroup() %>%
        summarise(rest_hr_proxy = mean(!!val_col_sym, na.rm = TRUE)) %>%
        pull(rest_hr_proxy)
  }else{
      df %>%
        filter(lubridate::hour(time) >= 11 & lubridate::hour(time) < 21) %>%
        arrange_at(val_col, .by_group = by_group_sort_flag) %>%
        head(., 5) %>%
        ungroup() %>%
        summarise(rest_hr_proxy = mean(!!val_col_sym, na.rm = TRUE)) %>%
        pull(rest_hr_proxy)
  }
}

calc_hourly_interval <- function(df,
                           val_col = "value",
                           grp_expr = NULL) {
  # Extract hourly min, max, and median heartrate metrics
  #
  # Args:
  #   df: heartrate data.frame (variable schema)
  #     ... | "val_col" <num> | ...
  #   val_col: string, default "value" column
  #           name of column to perform aggregations on
  #   grp_expr: string, default NULL
  #           column name for grouping or grouping expression
  #
  # Returns:
  #   a data frame with heartrate metrics

  if (!any(is.finite(unlist(df[val_col], use.names = FALSE))) | nrow(df) == 0){
    return(tibble(max_hourly = 0, min_hourly = 0, median_hourly = 0))
  }

  val_col <- rlang::ensym(val_col)

  if (!is.null(grp_expr)){

    df %>%
      group_by(!!rlang::parse_expr(grp_expr)) -> df

  }

  df %>%
    summarise(hour_avg = mean(!!val_col, na.rm = TRUE)) %>%
    summarise(max_hourly = max(hour_avg, na.rm = TRUE),
              min_hourly = min(hour_avg, na.rm = TRUE),
              median_hourly = median(hour_avg, na.rm = TRUE)) %>%
    ungroup() %>%
    select(max_hourly, min_hourly, median_hourly)
}

calc_minute_wear_count <- function(df,
                            hr_col = "value_hr",
                            filter_expr = NULL){
  # Number of minutes the fitbit was worn based on the filter condition
  #   Note: the minutes are counted when there are heartrate value
  #
  # Args:
  #   df: fitbit data frame (variable schema)
  #       ... | "hr_col" <num> | ...
  #   hr_col: string, default "value_hr column
  #         name of column containing hr values to check for NAs
  #   filter_expr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #   MinutesWearCount <num>

  hr_col <- rlang::ensym(hr_col)

  df %>%
    custom_filter(filter_expr) %>%
    filter(!is.na(!!hr_col)) %>%
    count() %>%
    pull(n)
}


###################################
# Common Utils
###################################

custom_filter <- function(df,
                        filter_expr = NULL){
  # Applies a filter on top of data if a filter expression is passed in
  #
  # Args:
  #   df: data.frame (variable schema)
  #   filter_expr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #   Filtered data frame if filter_expr exists else original df

  if (!is.null(filter_expr)) {
    filter_expr <- rlang::parse_expr(filter_expr)
    df <- filter(df, !!filter_expr)
  }
  return(df)
}

count_minutes <- function(is_high,
                          consec_nb = 5,
                          val_col = "values") {
    # Counts number of consecutive instances of active minutes
    #
    # Args:
    #   is_high: a vector of 0's and 1's representing active minutes
    #   consec_nb: int, default 5
    #             minimum number of consec minute to start counting
    #
    # Returns:
    #   an int representing the number of consecutive active minutes

    consec_min <- with(rle(is_high), lengths[get(val_col) > 0])
    min_total <- sum(consec_min[consec_min >= consec_nb])
    return(min_total)
}

calc_active_minutes <- function(df,
                          consec_nb = 5,
                          threshold = 70,
                          val_col = "value",
                          sort_cols = c("time"),
                          filter_expr = NULL) {
  # Number of values above the active threshold
  #
  # Args:
  #   df: fitbit data.frame (variable schema)
  #     ... | "val_col" <num> | "sort_cols"|...
  #   consec_nb: int, default 5
  #             minimum number of consecutive minute to start counting
  #   threshold: integer, default 120
  #             threshold for a minute to be considered active
  #   val_col: string, default "value" column
  #           name of column to perform aggregations on
  #   sort_cols: string vector, default "time" column
  #             columns to use to sort the dataframe
  #   filter_expr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #   active minutes <num>

  val_col <- rlang::ensym(val_col)

  df %>%
    custom_filter(filter_expr) %>%
    arrange_at(sort_cols) %>%
    mutate(is_high = if_else(!!val_col >= threshold, 1, 0, missing = 0)) %>%
    ungroup() %>%
    do(data.frame(active_mins = count_minutes(.$is_high, consec_nb))) %>%
    mutate(active_mins = replace_na(active_mins, 0)) %>%
    pull(active_mins)
}

calc_coefficient_of_variation <- function(df,
                val_col = "value",
                remove_zero = TRUE,
                filter_expr = NULL){
  # Coefficient of variation
  #
  # Args:
  #   df: fitbit data.frame (variable schema)
  #     ... | "val_col" <num> | ...
  #   val_col: string, default "value" column
  #           name of column to perform aggregations on
  #   remove_zero: boolean, default TRUE
  #           if TRUE, filter out all zero values before cov calculation
  #   filter_expr: string, default NULL (no filtering)
  #               expression to filter data frame on
  #
  # Returns:
  #  Coefficient of variation <num>

  if (!any(is.finite(unlist(df[val_col], use.names = FALSE))) | nrow(df) == 0){
    return(0)
  }

  val_col <- rlang::ensym(val_col)

  if (remove_zero){
    df %>%
      filter(!!val_col > 0) -> df
  }

  df %>%
    custom_filter(filter_expr) %>%
    summarise(sd = sd(!!val_col, na.rm = TRUE),
              mean = mean(!!val_col, na.rm = TRUE)) %>%
    mutate(
        coefficient_of_variation =
            if_else(mean > 0, (sd / mean) * 100, 0, missing = 0)) %>%
    pull(coefficient_of_variation)
}

###################################
# Combined Utils
###################################

combine_foot_steps_hr <- function(steps, heartrate){
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
      full_join(
        heartrate,
        by = c("userid", "date", "time_hour", "time_minute")
        ) %>%
      mutate(time = replace_na(
        as.POSIXct(
            paste(
                date, " ",
                time_hour,
                ":",
                time_minute,
                ":00",
                sep = "")
                )
            )
        ) %>%
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


get_active_minutes_reg_thresh <- function(patient_id, d, thresh_table, conn){

    # get threshold data for individual day
    #
    # Args:
    #   patient_id: string for userid
    #   d : date of day of data
    #   thresh_table: DB table name with threshold data
    #   conn: db connection

    # Returns:
    #  tibble of closest thresh data based on day and person
    #     thresh <dbl> | thresh_low <dbl> | thresh_high <dbl>

    patient_data <- tbl(conn, thresh_table) %>%
        filter(userid == patient_id) %>%
        mutate(date_diff = as.integer(abs(date - d))) %>%
        filter(date_diff == min(date_diff, na.rm = TRUE)) %>%
        collect()

    return (patient_data)
}


calc_switch_th <- function (vals, thresh){

    # count switches for hr data and a specific threshold value
    #
    # Args:
    #   vals: vector of hr data aggregated per minute
    #   thresh: numeric threshold value
    #
    # Returns:
    #  integer of number of times hr goes above and below thresh

    if (length(vals) > 1){

        vals[is.na(vals)] <- 0

        switch_thresh <- 0

        for (i in 1: (length(vals) - 1)){

            if ( (vals[i] < thresh) & (vals[i + 1] > thresh)){

                switch_thresh <- switch_thresh + 1
            }

        }
        return(switch_thresh)
    }else{
        return(0)
    }
}

consecutive <- function(time, consec){

    # count switches for hr data and a specific threshold value
    #
    # Args:
    #   time: vector of per minute timestamps
    #   consec: numeric for number of consecutive values
    #       needed to begin counting
    #
    # Returns:
    #  numeric of number of minutes in mvpa with consecutive
    #       value threshold met

    if (length(time) == 0){

        return(0)
    }

    if (length(time) == 1){

        return(0)
    }
    time <- sort(time)
    count <- 0
    tc <- 0
    for (i in 1:(length(time) - 1)){

        curr <- time[i]
        nex <- time[i + 1]
        dif <- abs(as.integer(difftime(nex, curr, units = "mins")))
        if (dif == 1){

            if (tc == 0){

                tc  <- tc + 2
            }else{
                tc <- tc + 1
            }
            if (i == (length(time) - 1) & tc >= consec){
                count <- count + tc
                tc <- 0
            }
        }
        if (dif != 1 & tc < consec){

            tc <- 0
        }
        if (dif != 1 & tc >= consec){

            count <- count + tc
            tc <- 0
        }
    }
    return(count)
}

calc_step_norm <- function(mvpa_times, fs, fs_column = "value_steps"){

    # average steps in specfic period of time
    #
    # Args:
    #   mvpa_times: vector of per minute timestamps
    #   fs: data frame with minute steps values
    #         userid <chr> | time <POSIXct> | value <num>
    #
    # Returns:
    #  numeric of average steps for minutes in mvpa

    match_active_hr_steps <- intersect(mvpa_times, fs$time)
    match_active_hr_steps <- as_datetime(match_active_hr_steps)
    match_steps <- fs[fs$time %in% match_active_hr_steps, ]
    return(mean(match_steps %>% pull(!!as.name(fs_column)), na.rm = TRUE))

}

calc_mvpa_count_neighbour_times_prev <- function(active_hr,
                                        fs,
                                        fs_column = "value_steps",
                                        window_size = 15){

    # count minutes in mvpa
    # for each minute in active_hr
    # check if any steps in previous 15 minutes
    # if yes, then classify as mvpa
    # else, not a mvpa

    # Args:
    #   fs: data frame with minute steps values
    #         userid <chr> | time <POSIXct> | value <num>
    #   active_hr: data frame with heartrate values > thresh
    #         userid <chr> | time <POSIXct> | value <num>
    #   window_size: intger informing how many minutes
    #                   to look back for steps
    #
    # Returns:
    #  vector of timestamps for minutes in mvpa

    times_in <- c()

    count <- 0
    for (t in unique(active_hr$time)){

        time_val <- as_datetime(t)
        prv <- time_val - (60 * window_size)
        fs_temp <- fs[ (fs$time >= prv) & (fs$time <= time_val), ]
        max_step <- max(fs_temp[fs_column], na.rm = TRUE)
        if (max_step > 0){

            count <- count + 1
            times_in <- c(times_in, time_val)
        }

    }

    times_in <- if (length(times_in) > 0)
                    as_datetime(times_in, tz = "UTC")
                else
                    c()

    return(times_in)
}

featurise_fitbit_combined <- function(
    fitbit_data,
    conn,
    thresh_table,
    d,
    patient) {

  # Featurise fitbit combine data (steps and HR)
  #
  # Args:
  #   df: combined fitbit data.frame
  #       time <POSIXct> | value_hr <num> | value_steps <num> | userid <chr>
  #
  # Returns:
  #  data.frame

  featurised_fitbit_data <- fitbit_data %>%
    group_by(userid, date = lubridate::date(time)) %>%
    nest() %>%
    # steps in total and per quadrants
    mutate(step_count = map_dbl(data,
                               count_steps,
                               val_col = "value_steps"),
           step_count_q2 = map_dbl(data,
                                 count_steps,
                                 val_col = "value_steps",
                                 filter_expr = filter_comb_q2),
           step_count_q3 = map_dbl(data,
                                 count_steps,
                                 val_col = "value_steps",
                                 filter_expr = filter_comb_q3),
           step_count_q4 = map_dbl(data,
                                 count_steps,
                                 val_col = "value_steps",
                                 filter_expr = filter_comb_q4),
           step_count_q234 = map_dbl(data,
                                 count_steps,
                                 val_col = "value_steps",
                                 filter_expr = filter_comb_q234)) %>%
    mutate(mean_hourly_steps_day = map_dbl(data,
                                        calc_mean_hourly_steps_day,
                                        val_col = "value_steps"),
           active_hours = map_dbl(data,
                                 calc_active_hours_count,
                                 val_col = "value_steps"),
           low_active_hours = map_dbl(data,
                                    calc_low_active_hours_count,
                                    val_col = "value_steps"),
           active_window = map(data,
                              get_active_window,
                              val_col = "value_steps")) %>%
    # active minutes steps
    mutate(active_mins_steps_0 = map_dbl(data,
                                  calc_active_minutes,
                                  consec_nb = 0,
                                  threshold = step_active,
                                  val_col = "value_steps",
                                  sort_cols = c("time")),
           active_mins_steps_2 = map_dbl(data,
                                      calc_active_minutes,
                                      consec_nb = 2,
                                      threshold = step_active,
                                      val_col = "value_steps",
                                      sort_cols = c("time")),
           active_mins_steps_5 = map_dbl(data,
                                      calc_active_minutes,
                                      consec_nb = 5,
                                      threshold = step_active,
                                      val_col = "value_steps",
                                      sort_cols = c("time")),
           active_mins_steps_10 = map_dbl(data,
                                      calc_active_minutes,
                                      consec_nb = 10,
                                      threshold = step_active,
                                      val_col = "value_steps",
                                      sort_cols = c("time")),
           active_mins_steps_20 = map_dbl(data,
                                      calc_active_minutes,
                                      consec_nb = 20,
                                      threshold = step_active,
                                      val_col = "value_steps",
                                      sort_cols = c("time")),
           active_mins_steps_0_ap = map_dbl(data,
                                  calc_active_minutes,
                                  consec_nb = 0,
                                  threshold = step_active,
                                  val_col = "value_steps",
                                  sort_cols = c("time"),
                                  filter_expr = filter_comb_q234)) %>%
    # coefficient of variation
    mutate(cov_step = map_dbl(data,
                             calc_coefficient_of_variation,
                             val_col = "value_steps"),
           cov_step_q1 = map_dbl(data,
                               calc_coefficient_of_variation,
                               val_col = "value_steps",
                               filter_expr = filter_comb_q1),
           cov_step_q2 = map_dbl(data,
                               calc_coefficient_of_variation,
                               val_col = "value_steps",
                               filter_expr = filter_comb_q2),
           cov_step_q3 = map_dbl(data,
                               calc_coefficient_of_variation,
                               val_col = "value_steps",
                               filter_expr = filter_comb_q3),
           cov_step_q4 = map_dbl(data,
                               calc_coefficient_of_variation,
                               val_col = "value_steps",
                               filter_expr = filter_comb_q4),
           cov_step_q234 = map_dbl(data,
                               calc_coefficient_of_variation,
                               val_col = "value_steps",
                               filter_expr = filter_comb_q234)) %>%
    # heartrate only feature
    mutate(resting_hr_proxy = map_dbl(data,
                                    calc_resting_hr_proxy,
                                    val_col = "value_hr"),
           resting_hr_1121 = map_dbl(data,
                                    calc_resting_hr_proxy,
                                    val_col = "value_hr",
                                    time_filter = TRUE),
           summary_vals = map(data,
                             calc_hourly_interval,
                             val_col = "value_hr",
                             grp_expr = "lubridate::hour(time)")) %>%

    mutate(cov_hr = map_dbl(data,
                           calc_coefficient_of_variation,
                           val_col = "value_hr",
                           remove_zero = FALSE),
           cov_hr_q1 = map_dbl(data,
                             calc_coefficient_of_variation,
                             remove_zero = FALSE,
                             val_col = "value_hr",
                             filter_expr = filter_comb_q1),
           cov_hr_q2 = map_dbl(data,
                             calc_coefficient_of_variation,
                             remove_zero = FALSE,
                             val_col = "value_hr",
                             filter_expr = filter_comb_q2),
           cov_hr_q3 = map_dbl(data,
                             calc_coefficient_of_variation,
                             remove_zero = FALSE,
                             val_col = "value_hr",
                             filter_expr = filter_comb_q3),
           cov_hr_q4 = map_dbl(data,
                             calc_coefficient_of_variation,
                             remove_zero = FALSE,
                             val_col = "value_hr",
                             filter_expr = filter_comb_q4),
           cov_hr_q5 = map_dbl(data,
                             calc_coefficient_of_variation,
                             remove_zero = FALSE,
                             val_col = "value_hr",
                             filter_expr = filter_comb_q234)) %>%
    # wearing time features
    mutate(mins_wear_total = map_dbl(data,
                                   calc_minute_wear_count),
           mins_wear_623 = map_dbl(data,
                                calc_minute_wear_count,
                                filter_expr = filter_comb_5),
           mins_wear_1121 = map_dbl(data,
                                calc_minute_wear_count,
                                filter_expr = filter_comb_6),
           mins_wear_q1 = map_dbl(data,
                                calc_minute_wear_count,
                                filter_expr = filter_comb_q1),
           mins_wear_q2 = map_dbl(data,
                                calc_minute_wear_count,
                                filter_expr = filter_comb_q2),
           mins_wear_q3 = map_dbl(data,
                                calc_minute_wear_count,
                                filter_expr = filter_comb_q3),
           mins_wear_q4 = map_dbl(data,
                                calc_minute_wear_count,
                                filter_expr = filter_comb_q4)) %>%
    mutate(mins_wear_q234 = map_dbl(data,
                                calc_minute_wear_count,
                                filter_expr = filter_comb_q234)) %>%
    mutate(wear_percent = (mins_wear_total / k_mins_day) * 100,
           wear_percent_q1 = (mins_wear_q1 / k_mins_quadrant) * 100,
           wear_percent_q2 = (mins_wear_q2 / k_mins_quadrant) * 100,
           wear_percent_q3 = (mins_wear_q3 / k_mins_quadrant) * 100,
           wear_percent_q4 = (mins_wear_q4 / k_mins_quadrant) * 100,
           wear_percent_q234 = (mins_wear_q234 / k_mins_active_qs) * 100) %>%

    mutate(gap_percent = 100 - wear_percent,
           gap_percent_q1 = 100 - wear_percent_q1,
           gap_percent_q2 = 100 - wear_percent_q2,
           gap_percent_q3 = 100 - wear_percent_q3,
           gap_percent_q4 = 100 - wear_percent_q4) %>%
    mutate(wear_during_sleep = if_else(gap_percent_q1 > 25, 0, 1)) %>%

    mutate(step_count_norm =
        if_else(mins_wear_total == 0, 0,
            step_count / mins_wear_total, missing = 0),

           step_count_norm_q2 =
            if_else(wear_percent_q2 == 0, 0,
            step_count_q2 / wear_percent_q2, missing = 0),

           step_count_norm_q3 =
            if_else(wear_percent_q3 == 0, 0,
            step_count_q3 / wear_percent_q3, missing = 0),

           step_count_norm_q4 =
            if_else(wear_percent_q4 == 0, 0,
            step_count_q4 / wear_percent_q4, missing = 0)) %>%

    select(-data) %>%
    unnest()

    # get mvpa threshold
    thresh_data  <- get_active_minutes_reg_thresh(
                        patient,
                        d,
                        thresh_table,
                        conn
                    )

    get # all 3 mvpa thresholds and add to returned df
    thresh <- (thresh_data %>% pull(!!as.name(thresh14)))[1]
    thresh_low <- (thresh_data %>% pull(!!as.name(thresh14_low)))[1]
    thresh_high <- (thresh_data %>% pull(!!as.name(thresh14_high)))[1]

    featurised_fitbit_data$thresh <- thresh
    featurised_fitbit_data$thresh_low <- thresh_low
    featurised_fitbit_data$thresh_high <- thresh_high

    # get hr > thresh and fs data
    active_hr <- fitbit_data %>% filter(!!as.name(hr_column) > thresh)
    fs_data <- fitbit_data %>% select(!!as.name(fs_column), time)

    # get times for hr > thresh and with steps in prev 15 minutes
    times_in <- calc_mvpa_count_neighbour_times_prev(
                    active_hr,
                    fs_data,
                    fs_column = fs_column,
                    window_size = 15
                )

    # get data where hr <= thresh and add mvpa values to returned df
    non_active_hr <- fitbit_data %>% filter(!!as.name(hr_column) <= thresh)
    featurised_fitbit_data$mvpa_15_prev_method <- length(times_in)
    featurised_fitbit_data$mvpa_15_prev_method2 <- consecutive(times_in, 2)
    featurised_fitbit_data$mvpa_15_prev_method3 <- consecutive(times_in, 3)
    featurised_fitbit_data$mvpa_15_prev_method5 <- consecutive(times_in, 5)
    featurised_fitbit_data$mvpa_15_prev_method10 <- consecutive(times_in, 10)
    featurised_fitbit_data$mvpa_15_prev_method20 <- consecutive(times_in, 20)

    # mvpa between 8-21 added to returned df
    featurised_fitbit_data$mvpa_15_prev_method_ap_8_21 <-
        ifelse ( (length(times_in) > 0),
            length(times_in[hour(times_in) >= apst & hour(times_in) <= aped ]),
            0
        )

    # add switch features to returned df
    featurised_fitbit_data$switch_thresh <- calc_switch_th(
                fitbit_data %>% pull(!!as.name(hr_column)), thresh)

    featurised_fitbit_data$switch_thresh_low <- calc_switch_th(
                fitbit_data %>% pull(!!as.name(hr_column)), thresh_low)

    featurised_fitbit_data$switch_thresh_high <- calc_switch_th(
                fitbit_data %>% pull(!!as.name(hr_column)), thresh_high)

    # get fs data when fs > 70 per minute
    # add various active step features to returned df
    active_steps <- fitbit_data %>%
                filter(!!as.name(fs_column) > step_active)

    active_mins_hr_steps <- as_datetime(
                intersect(times_in, active_steps$time), tz = "UTC")

    featurised_fitbit_data$active_mins_hr_steps_thresh <-
                length(active_mins_hr_steps)

    featurised_fitbit_data$active_mins_hr_steps_thresh2 <-
                consecutive(active_mins_hr_steps, 2)

    featurised_fitbit_data$active_mins_hr_steps_thresh3 <-
                consecutive(active_mins_hr_steps, 3)

    featurised_fitbit_data$active_mins_hr_steps_thresh5 <-
                consecutive(active_mins_hr_steps, 5)

    featurised_fitbit_data$active_mins_hr_steps_thresh10 <-
                consecutive(active_mins_hr_steps, 10)

    featurised_fitbit_data$active_mins_hr_steps_thresh20 <-
                consecutive(active_mins_hr_steps, 20)

    # add combo step and mvpa features to returned df
    featurised_fitbit_data$step_norm_thresh_mvpa <-
                calc_step_norm(times_in, fs_data)

    featurised_fitbit_data$step_sedentary_thresh_mvpa <-
                calc_step_norm(non_active_hr$time, fs_data)

    return(featurised_fitbit_data)
}
