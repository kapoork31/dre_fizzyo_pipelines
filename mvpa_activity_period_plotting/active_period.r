# load in libraries
xap.require('dplyr','chron','lubridate','ggplot2','DBI','tidyr','grid')

# source util files
source(file.path("~",
                 "scripts",
                 "fitbit_pipeline_cleaning_kunal",
                 "src",
                 "utils",
                 "fitbit_utils_5_fix_lint.R"
    )
)


input_table_name_hr = 'heart_rate_vals_2020_12_clean' # name of cleaned hr data
conn = xap.conn # # db connection
input_table_name_fs = 'fs_new' # fs data
year_table = 'activity_period_mvpa_data' # name of table to save data in 

data = xap.read_table('fitbit_featurise_table_2020_12') # featurised data
data = data[!duplicated(data[,1:2]),] # check no date for a parti
data = data[!is.na(data$mins_wear_total),] # dont include any day with na wear time

data$step_count_q1 = data$step_count - data$step_count_q234 # create step count for 00:01 - 06:00

dataFilter = data[data$mins_wear_total >= 1400,] # only days with >= 1400 minutes of wear time
dataFilter = dataFilter[dataFilter$step_count_q1 <= dataFilter$step_count_q3, ] # make sure steps in q1 <= steps in q3


link = xap.read_table('patient_key_id_list') # read in link file
ids = unique(dataFilter$userid) # all ids from fitbit data

get_active_minutes_reg_thresh <- function(patient_id, d,thresh_table =  'mvpa_thresh_11_9_4_5',conn){

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

# loop through each person
for(id in ids){

    print(id) # print persons id
    dates <- unique(dataFilter[dataFilter$userid == id,'date']) # get all days with minimum wear time for this person
    
    heartT <- tbl(conn, input_table_name_hr) %>%
        filter(userid == id) %>%
        mutate(date = to_date(time, "YYYY-MM-DD")) %>%
        filter(date %in% dates) %>%
        collect() # get data

    fsteps <- tbl(conn, input_table_name_fs) %>%
        filter(userid == id) %>%
        mutate(date = to_date(time, "YYYY-MM-DD")) %>%
        filter(date %in% dates) %>%
        collect() # get data

    heartT$min = format(heartT$time, format="%Y-%m-%d %H:%M") # create a time column with no seconds
    heartT$m = format(heartT$time, format="%H:%M") # create an hour and minute column
    heartT$timeData = format( heartT$time, format="%H:%M:%OS") # create time column
    heartTF = heartT # save as heartTF
    
    fsteps$timeData = format( fsteps$time, format="%H:%M:%OS") # create time column
    fstepsF = fsteps # save as fstepsF

    for(d in as.list(dates)){ # for each day

        thresh_data = get_active_minutes_reg_thresh(id,d, 'mvpa_thresh_11_9_4_5', conn)
        thresh <- (thresh_data %>% pull(!!as.name(thresh14)))[1] # get threshold data

        heartTD = heartTF[heartTF$date == d,] # heart filter by day
        fsD = fstepsF[fstepsF$date == d,] # foot steps filter by day
        month_column = month(d) # get month of day
        year_column = year(d) # get year
        day_n =  wday(d) # get day of week, 1 is sunday
        m_day = mday(d)
        raw_fitbit_data <- combine_foot_steps_hr(fsD, heartTD) # combine hr and footstep data
        active_hr <- raw_fitbit_data %>% filter(!!as.name(hr_column) > thresh) # get hr data > thresh
        fs_data <- raw_fitbit_data %>% select(!!as.name(fs_column), time) # get fs_data
        
        times_in <- calc_mvpa_count_neighbour_times_prev(
                    active_hr,
                    fs_data,
                    fs_column = fs_column,
                    window_size = 15
        ) # get time in mvpa

        agg = aggregate(heartTD[c('value')], list(heartTD$min), mean) # aggregate by minute
        colnames(agg) = c('time','value')	# change agg column names
        agg$time = as.POSIXct(agg$time, format = "%Y-%m-%d %H:%M") # convert time column to time variable
        agg$m = format(agg$time, format="%H:%M") # create a min column
        
        if(length(times_in)>0){ # if time in mvpa > 0
            activeHr = agg[agg$time %in% times_in,] # get actual mvpa data
            agghr = aggregate(activeHr[c('value')], list(activeHr$m), length) # aggregate by minute
            colnames(agghr) = c('hour','value') # change names, should be min but set as hour
            agghr$month = month_column # add month
            agghr$year = year_column # add year
            agghr$day = day_n # get day of week
            agghr$m_day = m_day # day of month
            agghr$userid = id # add id
            # could very well just add the date as a column as well
            dbWriteTable(conn, year_table, agghr, append = TRUE, row.names = FALSE) # save into table

        }
    }
}