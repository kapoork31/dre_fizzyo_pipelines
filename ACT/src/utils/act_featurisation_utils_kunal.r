####################################################################################################
# act_featurisation_utils.R
#
# This script contains utilites for featurising cleaned ACT data.
#
# Entrypoint Function: FeaturiseACTData()
#
# ------------------------------------------------------------------------------------------------
#
# Functions in this file:
#   featurise_act_data: Calls the functions that featurise ACT breath features' amplitudes and
#                     durations, and ACT break features duration (see below).
#   extract_breath_features: Using measured ACT data, calls the functions that calculate breath count,
#                          percent time breathing, and statistical functions of amplitude and
#                          duration.
#   extract_break_features: Using measured ACT data, calls the functions that calculate break count,
#                         and functions on durations of breaks between expired breaths.
#   get_breath_measures: Calculates the breath counts and statistical functions of amplitudes
#                      and durations of individual expired breaths.
#   get_break_durations: Calculates the break counts and statistical functions of durations of breaks
#                      between individual expired breaths.
#   agg_treatments_by_day_and_week: Aggregates the number of treatments by day and week and associates
#                              the treatment level data together with the featurised ACT data.
#   fill_missing_prescription_data: Replaces NAs in ACT data treatment grouping with default values.
#   featurise_treatment_adherence: Calculates features for treatment adherence from ACT data.
####################################################################################################

# The relative path below should be changed whn used on the DRE
options(digits.secs = 3)

k_min_complete_pressures <- 150

featurise_act_data <- function(labelled_act_data) {
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
  validate_data_schema(labelled_act_data, labelled_act_data_schema)

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

  # Flag with 0 treatments (sessions) that do not have enough data for analysis
  pressure_data_complete_score <- labelled_act_data %>%
    select(treatment_id, pressures_daily_count) %>%
    mutate(
      pressures_completeness_score =
        as.integer(pressures_daily_count > k_min_complete_pressures)
    ) %>%
    group_by(treatment_id) %>%
    slice(1:1) # take 1st elem from each group

  measured_act_data <- labelled_act_data %>%
    group_by(patient_id, treatment_id, act_date,
        breath_count, break_count, set_count) %>%
    nest() %>%
    mutate(
      break_durations = purrr::map(data, get_break_durations),
      breath_measures = purrr::map(data, get_breath_measures)
    )

  breath_features <- extract_breath_features(measured_act_data)
  break_features <- extract_break_features(measured_act_data)

  act_features <- breath_features %>%
    inner_join(break_features, by = "treatment_id") %>%
    inner_join(select(measured_act_data,
        -break_durations, -breath_measures, -data),
               by = "treatment_id") %>%
    inner_join(pressure_data_complete_score, by = "treatment_id") %>%
    inner_join(select(data_date, -patient_id, -act_date),
        by = "treatment_id") %>%
    select(patient_id, treatment_id, act_date, everything())

  act_features
}


extract_breath_features <- function(measured_act_data) {
  # Get features about breaths in ACT data
  #     from already-measured ACT data. Includes count,
  #     percent_time_breathing, mean/min/max/sd of durations
  #     and amplitudes.
  #
  # Args:
  #   measured_act_data: Data.frame of pressure data, including measurements.
  #                    See MeasuredACTDataSchema.
  #
  # Returns:
  #   Tibble of feature data.

  # for readability later statistical
  measures <- measured_act_data$breath_measures

  features <- tibble(
    treatment_id = measured_act_data$treatment_id,
    percent_time_breathing = purrr::map_dbl(
      measured_act_data$data,
      function(data) {
        length(data$breath_id[data$breath_id > 0]) / length(data$breath_id)
      }
    ),

    # breath_measures is a tibble of breath amplitudes/durations
    mean_breath_amplitude = purrr::map_dbl(
        measures, function(data) mean(data$amplitude)),

    min_breath_amplitude = purrr::map_dbl(
        measures, function(data) min(data$amplitude)),

    max_breath_amplitude = purrr::map_dbl(
        measures, function(data) max(data$amplitude)),

    sd_breath_amplitude = purrr::map_dbl(
        measures, function(data) sd(data$amplitude)),

    mean_breath_duration = purrr::map_dbl(
        measures, function(data) mean(data$duration)),

    min_breath_duration = purrr::map_dbl(
        measures, function(data) min(data$duration)),

    max_breath_duration = purrr::map_dbl(
        measures, function(data) max(data$duration)),

    sd_breath_duration = purrr::map_dbl(
        measures, function(data) sd(data$duration))
  )

  features
}




extract_break_features <- function(measured_act_data) {
  # Get features about breaks in ACT data from
  #     already-measured ACT data. Includes break count
  #     and mean/min/max/sd of break durations.
  #
  # Args:
  #   measured_act_data: Tibble of measured ACT data
  #     (see MeasuredACTDataSchema)
  #
  # Returns:
  #   tibble of feature data about breaks in ACT data

  features <- tibble(
    treatment_id = measured_act_data$treatment_id,

    mean_break_duration = purrr::map_dbl(
        measured_act_data$break_durations, mean),

    min_break_duration = purrr::map_dbl(
        measured_act_data$break_durations, min),

    max_break_duration = purrr::map_dbl(
        measured_act_data$break_durations, max),

    sd_break_duration = purrr::map_dbl(
        measured_act_data$break_durations, sd)

  )

  features
}

get_breath_measures <- function(df_tibble) {
  # Get breath measurements. Returns a tibble
  #     of two columns, amplitudes and breath durations.
  #
  # Args:
  #   labelled_act_data: A tibble of labelled ACT data
  #     (see LabelledACTDataSchema)
  #
  # Returns:
  #   Tibble of amplitudes and durations

  # need to check what will happen if there is 0 breaths
  #     in the treatment could throw an error?

  if (max(df_tibble$breath_id) == 0){

      return(tibble(amplitude = NA, duration = NA))
  }

  t <- df_tibble %>%
        filter(breath_id > 0) %>%
        group_by(breath_id) %>%
            summarize(
                amplitude = max(pressure_detrend),
                duration = as.numeric(max(time) - min(time))) %>%
            select(amplitude, duration)

  t

}

get_break_durations <- function(df_tibble) {

    durations <- df_tibble %>%
        filter(break_id > 0) %>%
        group_by(break_id) %>%
            summarize(k_breath_dur = as.numeric(max(time) - min(time))) %>%
            pull(k_breath_dur)

    durations
}

agg_treatments_by_day_and_week <- function(pressure_data) {
  # Calculate treatment/breaths counts for day and week.
  #
  # Args:
  #   pressure_data: preprocessed ACT data
  #        treatment_id <chr> | patient_id <chr> | act_date <POSIXct> |
  #        data <list - tibble of breath  data> | breath_count <num> |
  #        break_count <num> | pressures_daily_count <num> |
  #        pressures_completeness_score <num>
  #
  # Returns:
  #   pressure_data with aggregation columns:
  #      breaths_count_day,  treatment_count_day,
  #      breaths_count_week, treatment_count_week

  pressure_day <- pressure_data %>%
    group_by(patient_id, act_date) %>%
    mutate(
      breaths_count_day =
        sum(breath_count) * pressures_completeness_score,
        treatment_count_day = n() * pressures_completeness_score,
        act_week = week(act_date),
        year = year(act_date)
    ) %>%
    select(patient_id, treatment_id, act_date, year,
        act_week, breaths_count_day, treatment_count_day
    )

  pressure_day_top1 <-  pressure_day %>%
    group_by(patient_id, act_date) %>%
    arrange(act_date) %>%
    filter(row_number() == 1)

  pressure_weekly <- pressure_day_top1 %>%
    group_by(patient_id, year, act_week) %>%
    mutate(
      breaths_count_week = sum(breaths_count_day),
      treatment_count_week = sum(treatment_count_day)
    ) %>%
    arrange(patient_id, act_date) %>%
    filter(row_number() == 1) %>%
    select(patient_id, act_week, year, breaths_count_week, treatment_count_week)

  features <- left_join(pressure_day, pressure_weekly,
        by = c("patient_id", "act_week", "year"), all.x = TRUE)

  pressure_data <- inner_join(pressure_data, features,
        by = c("treatment_id", "patient_id", "act_date"), all.x = TRUE)

  pressure_data
}


fill_missing_prescription_data <- function(presc_data) {
  # Replace missing values in the prescription with defaults.
  #
  # Args:
  #   presc_data: prescribed ACT routine data.frame
  #
  # Returns:
  #    data.frame where NAs for prescription data have been
  #     filled with the default values
  k_default_n_sets <- 10
  k_default_n_treatments <- 2
  k_default_n_breaths_per_set <- 10

  # Rename columns in devices_data to highlight the prescription
  colnames(presc_data)[colnames(presc_data) == "treatments_per_day"] <-
    "prescr_treatments_day"

  colnames(presc_data)[colnames(presc_data) == "number_of_sets"] <-
    "prescr_n_sets"

  colnames(presc_data)[colnames(presc_data) == "breaths_per_set"] <-
    "prescr_n_breaths"

  # Make sure the columns with the prescription info have numeric types
  prescription_cols <- c("prescr_treatments_day", "prescr_n_sets",
    "prescr_n_breaths")

  presc_data[, prescription_cols] <-
    sapply(presc_data[, prescription_cols], as.numeric)

  presc_data %>%
    replace_na(
        list(
            prescr_n_sets = k_default_n_sets,
            prescr_n_breaths =  k_default_n_breaths_per_set,
            prescr_treatments_day = k_default_n_treatments
        )
    )
}


featurise_treatment_adherence <- function(pressure_data, devices_data) {
  # Calculate ACT adherence features.
  #
  # Args:
  #   pressure_data: preprocessed ACT data
  #        treatment_id <chr> | patient_id <chr> | act_date <POSIXct> |
  #        data <list - tibble of breath data> | breath_count <num> |
  #        break_count <num> | pressures_daily_count <num> |
  #         pressures_completeness_score <num>
  #
  # Returns:
  #    data.frame with new features
  #        treatment_id <chr> | patient_id <chr> | act_date <POSIXct> |
  #         pressures_completeness_score <num> |
  #         treatment_day_adherence_score <num> |
  #         breaths_day_adherence_score <num> |
  #         treatment_week_adherence_score <num> |
  #         breaths_week_adherence_score <num>

  pressure_data <- agg_treatments_by_day_and_week(pressure_data)
  devices_data <- fill_missing_prescription_data(devices_data)

  pressure_data <- merge(pressure_data, devices_data, by = "patient_id")
  pressure_data <- pressure_data %>%
    mutate(
        treatment_day_adherence_score =
                treatment_count_day / prescr_treatments_day,

        breaths_day_adherence_score =
            breaths_count_day / prescr_treatments_day /
            prescr_n_sets / prescr_n_breaths,

        treatment_week_adherence_score =
            treatment_count_week / prescr_treatments_day / 7,

        breaths_week_adherence_score =
            breaths_count_week / prescr_treatments_day /
            prescr_n_sets / prescr_n_breaths / 7) %>%

    select(patient_id, treatment_id, act_date, pressures_completeness_score,
           treatment_day_adherence_score, breaths_day_adherence_score,
           treatment_week_adherence_score, breaths_week_adherence_score)

  pressure_data
}
