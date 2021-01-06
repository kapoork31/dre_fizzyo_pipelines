#############################################################
##  This file runs tests on labelling utils
##
#############################################################

###################################
# Load libraries and source files
########################## #########

source(
  file.path(
    "~",
    "scripts",
    "ACT_v3",
    "act_pipeline",
    "src",
    "utils",
    "act_labelling_utils_kunal.R"
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
  "cleaned_act_data.csv"
)

cleaned_act_data <- readr::read_csv(dir)
pressure_data <- cleaned_act_data$pressure_detrend

test_that("indexes functions returns vector
    of same length and boolean vector", {

  peaks <- indexes(pressure_data)
  expect_equal(length(pressure_data), length(peaks))
  expect_type(peaks, "logical")
})

test_that("get_breath_endpoints functions
            returns startd and end of breath", {

  first_peak <- which(indexes(pressure_data))[1]
  endpoints <- get_breath_endpoints(pressure_data, first_peak, 2)
  expect_lt(endpoints[1], first_peak)
  expect_gt(endpoints[2], first_peak)
})

test_that("tag_breath_ids functions counts breaths well", {

  peaks <- indexes(pressure_data)
  breath_ids <- tag_breath_ids(pressure_data, peaks)
  expect_equal(max(breath_ids), 84)
  expect_type(breath_ids, "double")

})

test_that("tag_break_ids functions counts breaths well", {

  peaks <- indexes(pressure_data)
  break_ids <- tag_break_ids(pressure_data, peaks)
  expect_equal(max(break_ids), 85)
  expect_type(break_ids, "double")

})

test_that("determine_sets functions determines sets well", {

  peaks <- indexes(pressure_data)
  breath_ids <- tag_breath_ids(pressure_data, peaks)
  sets <- determine_sets(breath_ids, pressure_data)
  expect_equal(sets, 0)

})

test_that("tag_sets functions to tag breaths with sets", {

  peaks <- indexes(pressure_data)
  breath_ids <- tag_breath_ids(pressure_data, peaks)
  sets <- tag_sets(breath_ids, pressure_data)
  expect_equal(max(sets), 0)

})

test_that("find_peak_index functions to find peaks of each breath", {

  peaks <- indexes(pressure_data)
  breath_ids <- tag_breath_ids(pressure_data, peaks)
  peak_indexes <- find_peak_index(breath_ids, pressure_data)
  expect_equal(length(peak_indexes), 81)

})

test_that("test count_ids", {

  numerical_vector <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  expect_equal(count_ids(numerical_vector), length(numerical_vector))
})
