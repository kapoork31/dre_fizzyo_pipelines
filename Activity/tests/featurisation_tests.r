#############################################################
##  This file runs tests on featurisaton utils
##
#############################################################

###################################
# Load libraries and source files
########################## #########

source(file.path("~",
                "scripts",
                "fitbit_pipeline_cleaning_kunal",
                "src",
                "utils",
                "fitbit_utils_5_fix_lint.R"
                )
            )

library(testthat)
library(readr)

conn <- xap.conn

file_extension <- file.path(
                            "fitbit_pipeline_cleaning_kunal",
                            "data",
                            "clean_fitbit_data.csv")
                            

dir <- sub("devices", "scripts", getwd())
dir <- file.path(dir, file_extension)

clean_fitbit_data  <- readr::read_csv(dir)

thresh_table <- "mvpa_thresh_11_9_4_5" # thresh table name

test_that("count total steps in day", {

    step_count_val <- count_steps(clean_fitbit_data, val_col = "value_steps")
    expect_type(step_count_val, "integer")
    expect_equal(step_count_val,
                    sum(clean_fitbit_data$value_steps,
                    na.rm = TRUE)
                )

})


test_that("mean steps per hour", {

    avg_step_hours <- calc_mean_hourly_steps_day(clean_fitbit_data,
                                            val_col = "value_steps"
                                            )
    expect_type(avg_step_hours, "double")

})

test_that("Active_hours_count, number of hours wiht > 500 steps", {

    active_step_hours <- calc_active_hours_count(clean_fitbit_data,
                                          val_col = "value_steps")
    expect_type(active_step_hours, "integer")
    expect_equal(active_step_hours, 7)

})

test_that("low_active_hours_count, number of hours witt <= 500 steps", {

    low_active_step_hours <- calc_low_active_hours_count(
                                                clean_fitbit_data,
                                                val_col = "value_steps"
                                                )

    expect_type(low_active_step_hours, "integer")
    expect_equal(low_active_step_hours, 13)

})


test_that("get_active_window, start and end of active window", {

    active_window <- get_active_window(clean_fitbit_data,
                                        val_col = "value_steps")
    expect_length(active_window, 2)
    expect_setequal(colnames(active_window), c("start_window", "end_window"))

})


test_that("resting_hr_proxy, calculate rhr", {

    rhr <- calc_resting_hr_proxy(clean_fitbit_data, val_col = "value_hr")
    expect_type(rhr, "double")

})


test_that("hourly_interval, calculate rhr", {

    intervals <- calc_hourly_interval(clean_fitbit_data,
                                    val_col = "value_hr",
                                    grp_expr = "lubridate::hour(time)")
    expect_length(intervals, 3)
    expect_setequal(colnames(intervals),
                            c("max_hourly",
                            "min_hourly",
                            "median_hourly")
                    )

})


test_that("minute_wear_count, calculate rhr", {

    wear_time <- calc_minute_wear_count(clean_fitbit_data)
    expect_lte(wear_time, 1440)

})

test_that("count_minutes, count consecutive minutes", {

    is_high <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    consec <- count_minutes(is_high, 5)
    expect_equal(consec, length(is_high))

})

test_that("active_minutes, count consecutive minutes", {

    active_minute_steps <- calc_active_minutes(consec_nb = 0,
                                        clean_fitbit_data,
                                        val_col = "value_steps",
                                        threshold = 70
                                        )
    expect_equal(active_minute_steps,
        length(which(clean_fitbit_data$value_steps >= 70)))

    expect_type(active_minute_steps, "double")

})

test_that("coefficient_of_variation, calc cov of numerical vector", {

    steps <- c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1)
    numerical_values <- data.frame(value_steps = steps)
    cov <- calc_coefficient_of_variation(
      numerical_values,
      val_col = "value_steps"
    )
    expect_equal(cov, 0)

})

test_that("active_minutes_reg_thresh, thresh data per person per date", {

    thresh_data <- get_active_minutes_reg_thresh(
                            clean_fitbit_data$userid[1],
                            as.Date(clean_fitbit_data$time[1]),
                            thresh_table,
                            conn)

    expect_type(thresh_data, "list")
})

test_that("switch_th, count number of times hr values goes above a threshold", {

    numerical_vector <- c(1, 121, 1, 121, 1, 121)
    switch_count <- calc_switch_th(numerical_vector, 120)
    expect_equal(switch_count, 3)
})


test_that("mvpa_count_neighbour_times_prev, count mvpa minutes", {

    active_hr <- clean_fitbit_data %>% filter(!!as.name(hr_column) > 120)
    fs_data <- clean_fitbit_data %>% select(!!as.name(fs_column), time)
    times_in <- calc_mvpa_count_neighbour_times_prev(active_hr,
                                            fs_data,
                                            fs_column = fs_column,
                                            window_size = 15
                                            )

    expect_type(times_in[1], "double")
})


test_that("consecutive, count mvpa minutes with consecutive minimum", {

    active_hr <- clean_fitbit_data %>% filter(!!as.name(hr_column) > 120)
    fs_data <- clean_fitbit_data %>% select(!!as.name(fs_column), time)
    times_in <- calc_mvpa_count_neighbour_times_prev(active_hr,
                                           fs_data,
                                           fs_column = fs_column,
                                           window_size = 15
                                           )

    mvpa_5_consec <- consecutive(times_in, 5)
    expect_type(mvpa_5_consec, "double")

})

test_that("step_norm,
    count avg steps in period where hr is in a certain group", {

    active_hr <- clean_fitbit_data %>% filter(!!as.name(hr_column) > 120)
    fs_data <- clean_fitbit_data %>% select(!!as.name(fs_column), time)
    times_in <- calc_mvpa_count_neighbour_times_prev(active_hr,
                                            fs_data,
                                            fs_column = fs_column,
                                            window_size = 15
                                            )
    step_norm_mvpa <- calc_step_norm(times_in, fs_data)
    expect_type(step_norm_mvpa, "double")
})
