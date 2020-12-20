#############################################################
##  This file runs tests on cleaning utils
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
                "fitbit_cleaning_utils.R"
                )
            )

library(testthat)
library(readr)


file_extension <- file.path(
                            "fitbit_pipeline_cleaning_kunal",
                            "data",
                            "raw_fitbit_data.csv")
                            

dir <- sub("devices", "scripts", getwd())
dir <- file.path(dir, file_extension)
raw_fitbit_data  <- readr::read_csv(dir)

test_that("drop_indexes functions returns vector with given indexes removed", {

    fitbit_data  <- data.frame(
                    value = runif(
                        100, min = 0, max = 20),
                    value = runif(
                        100, min = 0, max = 50)
                        )
    remove_indexes <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    data_indexes_removed <- drop_indexes(fitbit_data, remove_indexes)
    expect_equal(nrow(fitbit_data) - length(remove_indexes),
                    nrow(data_indexes_removed)
                )
})


test_that("returns proportional difference between 2 values", {

    prop_difference <- prop_diff(6, 4)
    expect_equal(prop_difference, 50)
})


test_that("remove_rep_points_data removes repeating values", {

    cleaned_data <- remove_rep_points_data(raw_fitbit_data)
    expect_equal(colnames(raw_fitbit_data), colnames(cleaned_data))
    expect_lte(nrow(cleaned_data), nrow(raw_fitbit_data))
})


test_that("remove_random_points removes random values based on time", {

    cleaned_data <- remove_random_points(raw_fitbit_data)
    expect_equal(colnames(raw_fitbit_data), colnames(cleaned_data))
    expect_lte(nrow(cleaned_data), nrow(raw_fitbit_data))
})


test_that("remove70_leniant_1 removes false 70's based on time and value", {

    cleaned_data <- remove70_leniant_1(raw_fitbit_data)
    expect_equal(colnames(raw_fitbit_data), colnames(cleaned_data))
    expect_lte(nrow(cleaned_data), nrow(raw_fitbit_data))
})


test_that("remove_outlier_points removes outlier values", {

    cleaned_data <- remove_outlier_points(raw_fitbit_data)
    expect_equal(colnames(raw_fitbit_data), colnames(cleaned_data))
    expect_lte(nrow(cleaned_data), nrow(raw_fitbit_data))
})
