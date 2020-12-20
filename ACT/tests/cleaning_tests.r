source(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_cleaning_utils_kunal.R"
    )
)

library(testthat)
library(readr)
options(digits.secs = 3)

dir <- file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "data",
        "raw_act_data.csv"
    )
raw_act_data <- readr::read_csv(dir)

test_that("detrend_signal functions returns vector of same length", {

    new_signal <- detrend_signal(raw_act_data$pressurevalues)
    expect_equal(nrow(raw_act_data), length(new_signal))
})


test_that("test get_duplicate_level", {

    pressure_data_dup <- rep (10, 18)
    pressure_data_no_dup <- c(1, 2, 3, 4, 5, 6, 7, 8,
        9, 9, 8, 7, 6, 5, 4, 3, 2, 1)

    dup_level_dup <- get_duplicate_level(
        pressure_data_dup,
        duplicate_size = 9
    )
    dup_level_dup_no_dup <- get_duplicate_level(
        pressure_data_no_dup,
        duplicate_size = 9
    )
    expect_equal(dup_level_dup, 1)
    expect_equal(dup_level_dup_no_dup, 0)

})

test_that("test tag_duplicate_points", {

    pressure_data_dup <- rep(10, 18)
    pressure_data_no_dup <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 9,
        8, 7, 6, 5, 4, 3, 2, 1)

    dup_level_dup <- tag_duplicate_points(
        pressure_data_dup,
        duplicate_size = 9
    )

    dup_level_dup_no_dup <- tag_duplicate_points(
        pressure_data_no_dup,
        duplicate_size = 9
    )

    duplicates_true <- rep(TRUE, 9)
    duplicates_false <- rep(FALSE, 9)

    expect_equal(dup_level_dup[10:18], duplicates_true)
    expect_equal(dup_level_dup_no_dup[10:18], duplicates_false)

})

test_that("test fix_deduplicated_time", {

    time_data <- raw_act_data$time
    fixed_time <- fix_deduplicated_time(
        time_data, session_time_step = 0.1)

    expect_equal(length(time_data), length(fixed_time))
})
