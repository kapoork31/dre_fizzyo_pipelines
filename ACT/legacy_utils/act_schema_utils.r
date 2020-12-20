####################################################################################################
# act_schemas_uitls.R
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
#
####################################################################################################


RawACTSchema <- data.frame(
  id = character(),
  pressurevalues = numeric(),
  time = lubridate::ymd_hms(),
  patient_record_id = character(),
  stringsAsFactors = FALSE
)

CleanACTSchema <- data.frame(
  patientId = character(),
  sessionId = character(),
  time = lubridate::ymd_hms(),
  pressureDetrend = numeric(),
  stringsAsFactors = FALSE
)

ACTMetadataSchema <- data.frame(
  patientId = character(),
  sessionId = character(),
  treatmentId = character(),
  sessionStart = lubridate::ymd_hms(),
  sessionEnd = lubridate::ymd_hms(),
  stringsAsFactors = FALSE
)

LabelledACTDataSchema <- data.frame(
  patientId = character(),
  sessionId = character(),
  time = lubridate::ymd_hms(),
  pressureDetrend = numeric(),
  treatmentId = character(),
  prominentPeak = logical(),
  breathId = numeric(),
  breakId = numeric(),
  setId = numeric(),
  breathCount = integer(),
  breakCount = integer(),
  setCount = integer(),
  stringsAsFactors = FALSE
)

ACTFeaturesSchema <- data.frame(
  patientId = character(),
  treatmentdId = character(),
  actDate = lubridate::ymd(),
  breathCount = integer(),
  percentTimeBreathing = numeric(),
  meanBreathAmplitude = numeric(),
  minBreathAmplitude = numeric(),
  maxBreathAmplitude = numeric(),
  sdBreathAmplitude = numeric(),
  meanBreathDuration = numeric(),
  minBreathDuration = numeric(),
  maxBreathDuration = numeric(),
  sdBreathDuration = numeric(),
  breakCount = integer(),
  meanBreakDuration = numeric(),
  minBreakDuration = numeric(),
  maxBreakDuration = numeric(),
  sdBreakDuration = numeric(),
  stringsAsFactors = FALSE
)

MeasuredACTDataSchema <- data.frame(
  patientId = character(),
  treatmentId = character(),
  sessionId = character(),
  breathCount = integer(),
  breakCount = integer(),
  # data: tibble of pressure data
  #       | time | pressureDetrend | prominentPeak | breathId | breakId | actDate
  data = list(),
  # breakDurations: vector of difftime
  breakDurations = list(),
  # breathMeasures: tibble of measurements per breath
  #                 | amplitude | duration
  breathMeasures = list(),
  stringsAsFactors = FALSE
)
