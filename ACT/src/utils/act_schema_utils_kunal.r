####################################################################################################
# act_schemas_utils.R
#
# A central location to keep the various schemas.
#
# ------------------------------------------------------------------------------------------------
# Schemas for:
#   * Raw ACT data
#   * Cleaned ACT Data
#   * Labelled ACT Data
#   * Featurised ACT Data
#   * ACT Treatment Metadata
#   * ACT Devices Data
#   * ACT Link Data
#
####################################################################################################


raw_act_schema <- data.frame(
  id = character(),
  pressurevalues = numeric(),
  time = lubridate::ymd_hms(),
  patient_record_id = character(),
  stringsAsFactors = FALSE
)

clean_act_schema <- data.frame(
  patient_id = character(),
  session_id = character(),
  time = lubridate::ymd_hms(),
  pressure_detrend = numeric(),
  stringsAsFactors = FALSE
)

act_metadata_schema <- data.frame(
  patient_id = character(),
  session_id = character(),
  treatment_id = character(),
  session_start = lubridate::ymd_hms(),
  session_end = lubridate::ymd_hms(),
  stringsAsFactors = FALSE
)

act_metadata_schema2 <- data.frame(
  patient_id = character(),
  session_id = character(),
  treatment_id = character(),
  session_start = lubridate::ymd_hms(),
  session_end = lubridate::ymd_hms(),
  date = lubridate::ymd(),
  stringsAsFactors = FALSE
)

labelled_act_data_schema <- data.frame(
  patient_id = character(),
  session_id = character(),
  time = lubridate::ymd_hms(),
  pressure_detrend = numeric(),
  treatment_id = character(),
  session_start = lubridate::ymd_hms(),
  prominent_peak = logical(),
  breath_id = numeric(),
  break_id = numeric(),
  set_id = numeric(),
  breath_count = numeric(),
  break_count = numeric(),
  set_count = numeric(),
  stringsAsFactors = FALSE
)

act_features_schema <- data.frame(
  patient_id = character(),
  treatment_id = character(),
  act_date = lubridate::ymd(),
  pressures_completeness_score = numeric(),
  treatment_day_adherence_score = numeric(),
  breaths_day_adherence_score = numeric(),
  treatment_week_adherence_score = numeric(),
  breaths_week_adherence_score = numeric(),
  percent_time_breathing = numeric(),
  mean_breath_amplitude = numeric(),
  min_breath_amplitude = numeric(),
  max_breath_amplitude = numeric(),
  sd_breath_amplitude = numeric(),
  mean_breath_duration = numeric(),
  min_breath_duration = numeric(),
  max_breath_duration = numeric(),
  sd_breath_duration = numeric(),
  mean_break_duration = numeric(),
  min_break_duration = numeric(),
  max_break_duration = numeric(),
  sd_break_duration = numeric(),
  breath_count = numeric(),
  break_count = numeric(),
  set_count = numeric(),
  pressures_daily_count = numeric(),
  treatment_start = lubridate::ymd_hms(),
  treatment_end = lubridate::ymd_hms(),
  treatment_length = numeric(),
  seconds_above10 = numeric(),
  percentage_above10 = numeric(),
  stringsAsFactors = FALSE
)

act_post_featurisation_schema <- data.frame(
  patient_id = character(),
  treatment_id = character(),
  act_date = lubridate::ymd(),
  pressures_completeness_score = numeric(),
  treatment_day_adherence_score = numeric(),
  breaths_day_adherence_score = numeric(),
  treatment_week_adherence_score = numeric(),
  breaths_week_adherence_score = numeric(),
  percent_time_breathing = numeric(),
  mean_breath_amplitude = numeric(),
  min_breath_amplitude = numeric(),
  max_breath_amplitude = numeric(),
  sd_breath_amplitude = numeric(),
  mean_breath_duration = numeric(),
  min_breath_duration = numeric(),
  max_breath_duration = numeric(),
  sd_breath_duration = numeric(),
  mean_break_duration = numeric(),
  min_break_duration = numeric(),
  max_break_duration = numeric(),
  sd_break_duration = numeric(),
  breath_count = numeric(),
  break_count = numeric(),
  set_count = numeric(),
  pressures_daily_count = numeric(),
  treatment_start = lubridate::ymd_hms(),
  treatment_end = lubridate::ymd_hms(),
  treatment_length = numeric(),
  seconds_above10 = numeric(),
  percentage_above10 = numeric(),
  games = numeric(),
  games_played = character(),
  phase = character(),
  study_email = numeric(),
  age_recruited = numeric(),
  date_recruited = lubridate::ymd(),
  gender = character(),
  decimal_age = numeric(),
  day_in_study = numeric(),
  day_of_week = character(),
  month = numeric(),
  day_treatment = numeric(),
  tr_start_time = lubridate::ymd_hms(),
  season = numeric(),
  breath_count_is_out = logical(),
  stringsAsFactors = FALSE
)

measured_act_data_schema <- data.frame(
  patientId = character(),
  treatmentId = character(),
  sessionId = character(),
  breath_count = integer(),
  break_count = integer(),
  # data: tibble of pressure data
  #     | time | pressureDetrend |
  #     |prominentPeak | breathId |
  #     |breakId | actDate|
  data = list(),
  # break_durations: vector of difftime
  break_durations = list(),
  # breathMeasures: tibble of measurements per breath
  #                 | amplitude | duration
  breathMeasures = list(),
  stringsAsFactors = FALSE
)

act_devices_schema <- data.frame(
  study_email = numeric(),
  acapella = integer(),
  aerobika = integer(),
  pep_mask = integer(),
  pari_pep = integer(),
  other = integer(),
  if_other_please_state = character(),
  number_of_sets = integer(),
  breaths_per_set = integer(),
  if_other_please_state__1 = character(),
  treatments_per_day = integer(),
  if_other_please_state__2 = character(),
  tags = character(),
  id = integer(),
  id1 = integer(),
  stringsAsFactors = FALSE
)


act_devices_schema <- data.frame(
  study_email = numeric(),
  acapella = integer(),
  aerobika = integer(),
  pep_mask = integer(),
  pari_pep = integer(),
  other = integer(),
  if_other_please_state = character(),
  number_of_sets = integer(),
  breaths_per_set = integer(),
  if_other_please_state__1 = character(),
  treatments_per_day = integer(),
  if_other_please_state__2 = character(),
  tags = character(),
  id = integer(),
  id1 = integer(),
  patientId = character(),
  stringsAsFactors = FALSE
)

link_more_secure_schema <- data.frame(
  gos_id = character(),
  study_email = numeric(),
  study_uid = numeric(),
  date_recruited = lubridate::ymd(),
  age_recruited = integer(),
  fizzyo_hub_id = character(),
  patient_record_id = character(),
  date_feedback_start = lubridate::ymd(),
  date_gaming_start   = lubridate::ymd(),
  date_feedback_end   = lubridate::ymd(),
  date_gaming_ended   = lubridate::ymd(),
  date_withdrawn      = lubridate::ymd(),
  date_midpoint_test  = lubridate::ymd(),
  stringsAsFactors = FALSE
)
