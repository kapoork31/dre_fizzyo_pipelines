#############################################################
##  This file runs tests on featurisation utils
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
    "act_featurisation_utils_kunal.R"
  )
)

source(
  file.path(
    "~",
    "scripts",
    "ACT_v3",
    "act_pipeline",
    "src",
    "utils",
    "pipeline_utils_kunal_2.r"
  )
)

library(testthat)
library(readr)
library(lubridate)

options(digits.secs = 3)

conn <- xap.conn

dir_lab <- file.path(
  "~",
  "scripts",
  "ACT_v3",
  "act_pipeline",
  "data",
  "labelled_act_data.csv"
)
labelled_act_data <- readr::read_csv(dir_lab) %>%
  mutate_if(is.integer, as.numeric)

dir_feat <- file.path(
  "~",
  "scripts",
  "ACT_v3",
  "act_pipeline",
  "data",
  "featurised_act_data.csv"
)

featurised_data <- readr::read_csv(dir_feat)

link_more_secure_table_name <- "linkMoresecure"
devices_clean_table_name <- "devices_clean_test"
devices <- fetch_all_devices(
  conn,
  devices_clean_table_name,
  link_more_secure_table_name
)

gen_data <- function(labelled_act_data){

  # ==== ENTRYPOINT FUNCTION ====
  # Calculate breath durations and amplitudes, and break durations.
  #     Features are calculated from these measurements and returned
  #     as a data.frame. Features are identified by patient_id,
  #     treatment_id, session_id, and act_date
  #
  # Args:
  #   labelled_act_data: Data.frame of pressure data (see LabelledACTDataSchema)
  #
  # Returns:
  #   ACTFeatures: Data.frame of features (see ACTFeaturesSchema)

  # Validate schema of labelled ACT data

  data_date <- labelled_act_data %>%
    group_by(patient_id, treatment_id) %>%
    mutate(act_date = lubridate::as_date(time),
           treatment_start = min(time),
           treatment_end = max(time),
           seconds_above10 =
             sum(pressure_detrend[which(breath_id != 0)] >= 10) / 10,
           percentage_above10 =
             (seconds_above10 * 10 / length(pressure_detrend)) * 100,
           treatment_length =
             as.integer(difftime(max(time), min(time), unit = "secs"))) %>%
    # Some treatments occur over two days,
    # since they may start close to midnight and end early on the next day.
    # Slicing takes the first act_date of each treatment, so only one feature
    # row will be generated per treatment.
    slice(1:1) %>%
    ungroup() %>%
    select(patient_id, treatment_id, act_date,
           treatment_start, treatment_end, treatment_length,
           seconds_above10, percentage_above10)

  labelled_act_data <- left_join(labelled_act_data, data_date,
    by = c("patient_id", "treatment_id"), all.x = TRUE)

  # Calculate daily amount of ACT data (number of pressures) capture per day
  labelled_act_data <- labelled_act_data %>%
    group_by(act_date)  %>%
    mutate(pressures_daily_count = n()) %>%
    ungroup()

  labelled_act_data

}

labelled_data <- gen_data(labelled_act_data)

test_that("get_breath_measures", {

  breath_measures <- get_breath_measures(labelled_data)
  unique_breaths <- unique(
    labelled_data$breath_id[labelled_data$breath_id != 0])

  expect_equal(length(unique_breaths), nrow(breath_measures))
  expect_equal(colnames(breath_measures[1]), "amplitude")
  expect_equal(colnames(breath_measures[2]), "duration")

})

test_that("get_break_durations", {

  break_measures <- get_break_durations(labelled_data)
  unique_breaths <- unique(
    labelled_data$break_id[labelled_data$break_id != 0])

  expect_equal(length(unique_breaths), length(break_measures))
})

test_that("extract_breath_features", {

  measured_act_data <- labelled_data %>%
    group_by(patient_id, treatment_id, act_date,
        breath_count, break_count, set_count) %>%
    nest() %>%
    mutate(
      breath_measures = purrr::map(data, get_breath_measures)
    )

  breath_features <- extract_breath_features(measured_act_data)
  breath_features %>%
    select_if(function(x) any(!is.na(x))) -> finite_features

  expect_equal(length(breath_features), length(finite_features))

})

test_that("extract_break_features", {

  measured_act_data <- labelled_data %>%
    group_by(patient_id, treatment_id, act_date,
        breath_count, break_count, set_count) %>%
    nest() %>%
    mutate(
      break_durations = purrr::map(data, get_break_durations),
    )

  break_features <- extract_break_features(measured_act_data)
  break_features %>%
    select_if(function(x) any(!is.na(x))) -> finite_features

  expect_equal(length(break_features), length(finite_features))

})

test_that("fill_missing_prescription_data", {

  cleaned_devices <- fill_missing_prescription_data(devices)
  expect_equal(nrow(cleaned_devices), nrow(devices))
})


test_that("agg_treatments_by_day_and_week", {

  treatment_features <- featurised_data %>%
    select(
      patient_id,
      treatment_id,
      act_date,
      breath_count,
      pressures_completeness_score
    )

  agg_data <- agg_treatments_by_day_and_week(treatment_features)
  expect_equal(nrow(agg_data), nrow(treatment_features))

})

test_that("featurise_treatment_adherence", {

  # Time Featurise Treatment Adherence Score
  treatment_features <- featurised_data %>%
    select(
      patient_id,
      treatment_id,
      act_date,
      breath_count,
      pressures_completeness_score
    )

  adherence_features <- featurise_treatment_adherence(
    treatment_features, devices)

  expect_equal(nrow(adherence_features), nrow(treatment_features))

})


test_that("featurise_act_data", {

  # Time Featurise Treatment Adherence Score
  featurised_labelled_data <- featurise_act_data(labelled_act_data)
  unique_treatments <- unique(labelled_act_data$treatment_id)
  expect_equal(nrow(featurised_labelled_data), length(unique_treatments))

})
