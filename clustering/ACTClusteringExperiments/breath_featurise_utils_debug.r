################################################################################
# breath_featurise_utils.R
#
# This script is used to get a Tibble of features from a Tibble of pressure
# data.
#
# Entrypoint: FeaturiseBreaths
#
################################################################################


library(magrittr)
library(purrr)
library(tidyverse)
library(lubridate)


ActFeaturesSchema <- data.frame(
  sessionId = character(),
  patientId = character(),
  actDate = lubridate::ymd(),
  breathCount = numeric(),
  percentTimeBreathing = numeric(),
  meanBreathAmplitude = numeric(),
  minBreathAmplitude = numeric(),
  maxBreathAmplitude = numeric(),
  sdBreathAmplitude = numeric(),
  meanBreathDuration = numeric(),
  minBreathDuration = numeric(),
  maxBreathDuration = numeric(),
  sdBreathDuration = numeric(),
  breakCount = numeric(),
  meanBreakDuration = numeric(),
  minBreakDuration = numeric(),
  maxBreakDuration = numeric(),
  sdBreakDuration = numeric(),
  stringsAsFactors = FALSE
)


FeaturiseBreaths <- function(pressureData, groupCol) {
  # Get breath and break features.
  #
  # Args:
  #   pressureData: Tibble of pressure data
  #   id <chr> | patient_record_id <chr> | breathCount <dbl> | breakCount <dbl> | data <list> |
  #      breathMeasures <list> | breakDurations <dbl>
  #   groupCol: string column name used for grouping ("sessionId" or "treatmentId")
  # Returns:
  #   Nested "data" column tibble format:
  #   pressurevalues <dbl> | time <dttm> | pressureDetrend <dbl> |
  #      prominentPeak <lgl> | breathId <dbl>

  if (!groupCol %in% c("sessionId", "treatmentId")){
    stop(sprintf("The desired grouping column %s is not supported", groupCol))
  }

  if (!groupCol %in% colnames(pressureData)) {
    stop(sprintf("The grouping column %s was not found in the input data", groupCol))
  }

  pressureData %<>%
    # Calculate breath amplitudes and durations
    mutate(breathMeasures = map(data, GetBreathMeasures)) %>%
    mutate(breakDurations = map(data, GetBreakDurations))

  breathFeatures <- ExtractBreathFeatures(pressureData, groupCol)
  breakFeatures <- ExtractBreakFeatures(pressureData, groupCol)

  actDateDf <- pressureData %>%
    select(!!rlang::ensym(groupCol), patient_record_id, actDate) %>%
    rename(patientId = patient_record_id)

  groupColumns <-  c("patientId", groupCol)

  features <- merge(breathFeatures, breakFeatures, by = groupColumns)
  f <- breathFeatures %>%
    inner_join(breakFeatures,  by = groupColumns) %>%
    inner_join(actDateDf, by = groupColumns) %>% #reorder columns (bring ids cols to the left)
    select(patientId, !!rlang::ensym(groupCol), actDate, everything())
}


ExtractBreathFeatures <- function(pt, groupCol) {
  # Get a Tibble of breath features from a tibble of pressure data.
  #
  # Args:
  #   pt: Tibble of pressure data
  #   id <chr> | patient_record_id <chr> | breathCount <dbl> | breakCount <dbl> | data <list> |
  #       breathMeasures <list> | breakDurations <dbl>
  #   Nested "data" column tibble format:
  #   pressurevalues <dbl> | time <dttm> | pressureDetrend <dbl> |
  #       prominentPeak <lgl> | breathId <dbl>
  #
  # Returns:
  #   Tibble of feature data.

  features <- tibble(
    patientId = pt$patient_record_id,

    breathCount = pt$breathCount,
    percentTimeBreathing = map_dbl(pt$data, SessionPercentTimeBreathing),

    # breathMeasures is a tibble of breath amplitudes/durations
    meanBreathAmplitude = map_dbl(pt$breathMeasures, function(x) mean(x$amplitude)),
    minBreathAmplitude  = map_dbl(pt$breathMeasures, function(x) min(x$amplitude)),
    maxBreathAmplitude  = map_dbl(pt$breathMeasures, function(x) max(x$amplitude)),
    sdBreathAmplitude   = map_dbl(pt$breathMeasures, function(x) sd(x$amplitude)),

    meanBreathDuration = map_dbl(pt$breathMeasures, function(x) mean(x$duration)),
    minBreathDuration  = map_dbl(pt$breathMeasures, function(x) min(x$duration)),
    maxBreathDuration  = map_dbl(pt$breathMeasures, function(x) max(x$duration)),
    sdBreathDuration   = map_dbl(pt$breathMeasures, function(x) sd(x$duration))
  )

  features <- add_column(features, !!(groupCol) := pt[[groupCol]])

  features
}


ExtractBreakFeatures <- function(pt, groupCol) {
  # Get a Tibble of break features from a Tibble of pressure data.
  #
  # Args:
  #   pressure_tibble: Tibble of pressure data
  #   id <chr> | patient_record_id <chr> | breathCount <dbl> | breakCount <dbl> | data <list> |
  #       breathMeasures <list> | breakDurations <dbl
  #   Nested "data" column tibble format:
  #   pressurevalues <dbl> | time <dttm> | pressureDetrend <dbl> |
  #       prominentPeak <lgl> | breathId <dbl>
  #
  # Returns:
  #   Tibble of feature data.
  features <- tibble(
    patientId = pt$patient_record_id,
    breakCount        = map_dbl(pt$breakDurations, length),
    meanBreakDuration = map_dbl(pt$breakDurations, mean),
    minBreakDuration  = map_dbl(pt$breakDurations, min),
    maxBreakDuration  = map_dbl(pt$breakDurations, max),
    sdBreakDuration   = map_dbl(pt$breakDurations, sd)
  )

  features <- add_column(features, !!(groupCol) := pt[[groupCol]])
  features
}


SessionPercentTimeBreathing <- function(data) {
  # Extract percent of breathing time.
  #
  # Args:
  #   data: a tibble
  #
  # Returns:
  #   Percentage of time breathing

  breathIds <- data$breathId
  length(breathIds[breathIds > 0]) / length(breathIds)
}


GetBreathMeasures <- function(data) {
  # Get breath measurements. Returns a list containing
  # two vectors; a vector of breath amplitudes and breath
  # durations respectively.
  #
  # Args:
  #   data: A Tibble of breath session data
  #   pressurevalues <dbl> | time <dttm> | pressureDetrend <dbl> |
  #       prominentPeak <lgl> | breathId <dbl>
  #
  # Returns:
  #   Tibble of (amplitudes, durations)

  breathIds <- unique(data$breathId)

  if (identical(breathIds, 0)) {
    return(tibble(amplitude = NA, duration = NA))
  }

  amplitudes <- c()
  durations  <- c()

  for (id in breathIds[breathIds != 0]) {
    kBreath <- filter(data, data$breathId == id)

    kBreathPressures <- kBreath$pressureDetrend
    amplitudes <- c(amplitudes, max(kBreathPressures) - min(kBreathPressures))

    kBreathTimes <- kBreath$time
    kStartTime   <- lubridate::ymd_hms(min(kBreathTimes))
    kEndTime     <- lubridate::ymd_hms(max(kBreathTimes))
    durations    <- c(durations, kEndTime - kStartTime)
  }

  tibble(amplitude = amplitudes, duration = durations)
}


GetBreakDurations <- function(data) {
  # Get break durations as a vector.
  #
  # Args:
  #   data: A tibble of breath session data.
  #   pressurevalues <dbl> | time <dttm> | pressureDetrend <dbl> |
  #       prominentPeak <lgl> | breathId <dbl>
  #
  # Returns:
  #   Vector of break durations

  breakIds <- unique(data$breakId)
  breakIds <- breakIds[breakIds != 0]

  durations <- c()

  for (id in breakIds) {
    kBreak <- filter(data, data$breakId == id)

    kBreakTimes <- kBreak$time
    kStartTime  <- lubridate::ymd_hms(min(kBreakTimes))
    kEndTime    <- lubridate::ymd_hms(max(kBreakTimes))
    kBreathDur  <- kEndTime - kStartTime

    durations <- c(durations, kBreathDur)
  }

  durations
}

AggTreatmentsByDayAndWeek <- function(pressureData) {
  # Calculate treatment/breaths counts for day and week.
  #
  # Args:
  #   pressureData: preprocessed ACT data
  #        treatmentId <chr> | patient_record_id <chr> | actDate <POSIXct> |
  #        data <list - tibble of breath  data> | breathCount <num> |
  #        breakCount <num> | pressuresDailyCount <num> | pressuresDayCompletenessScore <num>
  #
  # Returns:
  #   pressureData with aggregation columns:
  #      breathsCountDay,  treatmentCountDay, breathsCountWeek, treatmentCountWeek

  pressureDay <- pressureData %>%
    group_by(patient_record_id, actDate) %>%
    mutate(
      breathsCountDay = sum(breathCount) * pressuresDayCompletenessScore,
      treatmentCountDay = n() *  pressuresDayCompletenessScore,
      actWeek = week(actDate)
    ) %>%
    select(patient_record_id, treatmentId, actDate, actWeek, breathsCountDay, treatmentCountDay)

  pressureDayTop1 <-  pressureDay %>%
    group_by(patient_record_id, actDate) %>%
    arrange(actDate) %>%
    filter(row_number() == 1)

  pressureWeekly <- pressureDayTop1  %>%
    group_by(patient_record_id, actWeek) %>%
    mutate(
      breathsCountWeek = sum(breathsCountDay),
      treatmentCountWeek = sum(treatmentCountDay)
    ) %>%
    arrange(patient_record_id, actDate) %>%
    filter(row_number() == 1) %>%
    select(patient_record_id, actWeek, breathsCountWeek, treatmentCountWeek)

  features <- left_join(pressureDay, pressureWeekly,
                        by = c("patient_record_id", "actWeek"), all.x = TRUE)
  pressureData <- inner_join(pressureData, features,
                             by = c("treatmentId", "patient_record_id", "actDate"), all.x = TRUE)
  pressureData
}

FillMissingPrescriptionData <- function(prescData)
  # Replace missing values in the prescription with defaults.
  #
  # Args:
  #   (prescData: prescribed ACT routine dataframe
  #
  # Returns:
  #    dataFrame with NAs for prescription data being filled with the default values
{
  kDefaultNSets <- 10
  kDefaultNTreatments <- 2
  kDefaultNBreathsPerSet <- 10
  # Rename columns in devicesData to highlight the prescription
  colnames(prescData)[colnames(prescData) == "treatments_per_day"] <- "prescrTreatmentsDay"
  colnames(prescData)[colnames(prescData) == "number_of_sets"] <- "prescrNSets"
  colnames(prescData)[colnames(prescData) == "breaths_per_set"] <- "prescrNBreaths"

  # Make sure the columns with the prescription info have numeric types
  prescriptionCols <- c("prescrTreatmentsDay", "prescrNSets", "prescrNBreaths")
  prescData[, prescriptionCols] <- sapply(prescData[, prescriptionCols], as.numeric)

  prescData %>%
    replace_na(list(prescrNSets = kDefaultNSets, prescrNBreaths =  kDefaultNBreathsPerSet,
                    prescrTreatmentsDay = kDefaultNTreatments))
}


FeaturiseTreatmentAdherence <- function(pressureData, devicesData) {
  # Calculate  ACT adherence features.
  #
  # Args:
  #   pressureData: preprocessed ACT data
  #        treatmentId <chr> | patient_record_id <chr> | actDate <POSIXct> |
  #        data <list - tibble of breath data> | breathCount <num> |
  #        breakCount <num> | pressuresDailyCount <num> | pressuresDayCompletenessScore <num>
  #
  # Returns:
  #    dataFrame with new features
  #        treatmentId <chr> | patient_record_id <chr> | actDate <POSIXct> |
  #        pressuresDayCompletenessScore <num> | treatmentDayAdherenceScore <num> |
  #        breathsDayAdherenceScore <num> | treatmentWeekAdherenceScore <num> |
  #        breathsWeekAdherenceScore <num>

  pressureData <- AggTreatmentsByDayAndWeek(pressureData)
  print("AggTreatmentsByDayAndWeek")
  print(str(pressureData))
  devicesData <- FillMissingPrescriptionData(devicesData)

  pressureData <- merge(pressureData, devicesData, by = "patient_record_id")
  pressureData <- pressureData %>%
    mutate(treatmentDayAdherenceScore = treatmentCountDay / prescrTreatmentsDay,
           breathsDayAdherenceScore = breathsCountDay / prescrTreatmentsDay /
                                      prescrNSets / prescrNBreaths,
           treatmentWeekAdherenceScore = treatmentCountWeek / prescrTreatmentsDay / 7,
           breathsWeekAdherenceScore = breathsCountWeek / prescrTreatmentsDay /
                                       prescrNSets / prescrNBreaths / 7)
                                       #%>%
    #select(patient_record_id, treatmentId, actDate, pressuresDayCompletenessScore,
   #        treatmentDayAdherenceScore, breathsDayAdherenceScore,
    #       treatmentWeekAdherenceScore, breathsWeekAdherenceScore)

  pressureData
}
