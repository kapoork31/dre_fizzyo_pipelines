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
#   FeaturiseBreaths: Calls the functions that featurise ACT breath features' amplitudes and
#                     durations, and ACT break features duration (see below).
#   ExtractBreathFeatures: Using measured ACT data, calls the functions that calculate breath count,
#                          percent time breathing, and statistical functions of amplitude and
#                          duration.
#   ExtractBreakFeatures: Using measured ACT data, calls the functions that calculate break count,
#                         and functions on durations of breaks between expired breaths.
#   GetBreathMeasures: Calculates the breath counts and statistical functions of amplitudes
#                      and durations of individual expired breaths.
#   GetBreakDurations: Calculates the break counts and statistical functions of durations of breaks
#                      between individual expired breaths.
#   AggTreatmentsByDayAndWeek: Aggregates the number of treatments by day and week and associates
#                              the treatment level data together with the featurised ACT data.
#   FillMissingPrescriptionData: Replaces NAs in ACT data treatment grouping with default values.
#   FeaturiseTreatmentAdherence: Calculates features for treatment adherence from ACT data.
####################################################################################################

# The relative path below should be changed whn used on the DRE
source("~/scripts/ACT_v3/act_pipeline/src/utils/act_validation_utils.R")
source("~/scripts/ACT_v3/act_pipeline/src/utils/act_schema_utils_kunal.R")
options(digits.secs = 3)

kMinCompletPressures <- 150

FeaturiseACTData <- function(labelledACTData) {
  # ==== ENTRYPOINT FUNCTION ====
  # Calculate breath durations and amplitudes, and break durations. Features are calculated from
  #   these measurements and returned as a data.frame. Features are identified by patientId,
  #   treatmentId, sessionId, and actDate.
  #
  # Args:
  #   labelledACTData: Data.frame of pressure data (see LabelledACTDataSchema)
  #
  # Returns:
  #   ACTFeatures: Data.frame of features (see ACTFeaturesSchema)

  # Validate schema of labelled ACT data
  #ValidateDataSchema(labelledACTData, LabelledACTDataSchema)

  #dataDate <- labelledACTData %>%
    #group_by(patientId, treatmentId) %>%
    #mutate(actDate = lubridate::as_date(time)) %>%
    # Some treatments occur over two days,
    # since they may start close to midnight and end early on the next day.
    # Slicing takes the first actDate of each treatment, so only one feature
    # row will be generated per treatment.
    #slice(1:1) %>%
    #ungroup() %>%
    #select(patientId, treatmentId, actDate)

  dataDate <- labelledACTData %>%
    group_by(patientId, treatmentId) %>%
    mutate(actDate = lubridate::as_date(time),
    treatmentStart = min(time),
    treatmentEnd = max(time),
    secondsAbove10 = sum(pressureDetrend[which(breathId!= 0)] >= 10)/10,
    percentageAbove10 = (secondsAbove10*10/length(pressureDetrend)) * 100,
    treatmentLength = as.integer(difftime(max(time),min(time),unit = 'secs'))) %>%
    # Some treatments occur over two days,
    # since they may start close to midnight and end early on the next day.
    # Slicing takes the first actDate of each treatment, so only one feature
    # row will be generated per treatment.
    slice(1:1) %>%
    ungroup() %>%
    select(patientId, treatmentId, actDate,treatmentStart,treatmentEnd,treatmentLength,secondsAbove10,percentageAbove10)

  labelledACTData <- left_join(labelledACTData, dataDate,
                        by = c("patientId", "treatmentId"), all.x = TRUE)

  # Calculate daily amount of ACT data (number of pressures) capture per day
  labelledACTData <- labelledACTData %>%
    group_by(actDate)  %>%
    mutate(pressuresDailyCount = n()) %>%
    ungroup()

  # Flag with 0 treatments (sessions) that do not have enough data for analysis
  pressureDataCompletScore <- labelledACTData %>%
    select(treatmentId, pressuresDailyCount) %>%
    mutate(
      pressuresDayCompletenessScore = as.integer(pressuresDailyCount > kMinCompletPressures)
    ) %>%
    group_by(treatmentId) %>%
    slice(1:1) # take 1st elem from each group

  measuredACTData <- labelledACTData %>%
    group_by(patientId, treatmentId, actDate, breathCount, breakCount, setCount) %>%
    nest() %>%
    mutate(
      breakDurations = purrr::map(data, GetBreakDurations),
      breathMeasures = purrr::map(data, GetBreathMeasures)
    )

  breathFeatures <- ExtractBreathFeatures(measuredACTData)
  breakFeatures <- ExtractBreakFeatures(measuredACTData)

  ACTFeatures <- breathFeatures %>%
    inner_join(breakFeatures, by = "treatmentId") %>%
    inner_join(select(measuredACTData, -breakDurations, -breathMeasures, -data),
               by = "treatmentId") %>%
    inner_join(pressureDataCompletScore, by = "treatmentId") %>%
    inner_join(select(dataDate, -patientId, -actDate),by = "treatmentId") %>%
    select(patientId, treatmentId, actDate, everything())

  ACTFeatures
}


ExtractBreathFeatures <- function(measuredACTData) {
  # Get features about breaths in ACT data from already-measured ACT data. Includes count,
  #   percentTimeBreathing, mean/min/max/sd of durations and amplitudes.
  #
  # Args:
  #   measuredACTData: Data.frame of pressure data, including measurements.
  #                    See MeasuredACTDataSchema.
  #
  # Returns:
  #   Tibble of feature data.

  # for readability later statistical
  measures <- measuredACTData$breathMeasures

  features <- tibble(
    treatmentId = measuredACTData$treatmentId,
    percentTimeBreathing = purrr::map_dbl(
      measuredACTData$data,
      function(data) {
        length(data$breathId[data$breathId > 0]) / length(data$breathId)
      }
    ),

    # breathMeasures is a tibble of breath amplitudes/durations
    meanBreathAmplitude = purrr::map_dbl(measures, function(data) mean(data$amplitude)),
    minBreathAmplitude = purrr::map_dbl(measures, function(data) min(data$amplitude)),
    maxBreathAmplitude = purrr::map_dbl(measures, function(data) max(data$amplitude)),
    sdBreathAmplitude = purrr::map_dbl(measures, function(data) sd(data$amplitude)),
    meanBreathDuration = purrr::map_dbl(measures, function(data) mean(data$duration)),
    minBreathDuration = purrr::map_dbl(measures, function(data) min(data$duration)),
    maxBreathDuration = purrr::map_dbl(measures, function(data) max(data$duration)),
    sdBreathDuration = purrr::map_dbl(measures, function(data) sd(data$duration))
  )

  features
}




ExtractBreakFeatures <- function(measuredACTData) {
  # Get features about breaks in ACT data from already-measured ACT data. Includes break count
  #   and mean/min/max/sd of break durations.
  #
  # Args:
  #   measuredACTData: Tibble of measured ACT data (see MeasuredACTDataSchema)
  #
  # Returns:
  #   tibble of feature data about breaks in ACT data

  features <- tibble(
    treatmentId = measuredACTData$treatmentId,
    meanBreakDuration = purrr::map_dbl(measuredACTData$breakDurations, mean),
    minBreakDuration = purrr::map_dbl(measuredACTData$breakDurations, min),
    maxBreakDuration = purrr::map_dbl(measuredACTData$breakDurations, max),
    sdBreakDuration = purrr::map_dbl(measuredACTData$breakDurations, sd)
  )

  features
}


GetBreathMeasures <- function(df_tibble) {
  # Get breath measurements. Returns a tibble of two columns, amplitudes and breath durations.
  #
  # Args:
  #   labelledACTData: A tibble of labelled ACT data (see LabelledACTDataSchema)
  #
  # Returns:
  #   Tibble of amplitudes and durations
  
  # need to check what will happen if there is 0 breaths in the treatment could throw an error?
  
  if(max(df_tibble$breathId) == 0){
      return(tibble(amplitude = NA, duration = NA))
  }
  
  t <- df_tibble %>%
        filter(breathId >0) %>%
        group_by(breathId) %>%
            summarize(
                amplitude = max(pressureDetrend),
                duration = as.numeric(max(time) - min(time))) %>%
            select(amplitude,duration)
    
  t

}

GetBreakDurations <- function(df_tibble) {

    durations <- df_tibble %>%
        filter(breakId >0) %>%
        group_by(breakId) %>%
            summarize(kBreathDur = as.numeric(max(time) - min(time))) %>%
            pull(kBreathDur)
    
    durations
}

AggTreatmentsByDayAndWeek <- function(pressureData) {
  # Calculate treatment/breaths counts for day and week.
  #
  # Args:
  #   pressureData: preprocessed ACT data
  #        treatmentId <chr> | patientId <chr> | actDate <POSIXct> |
  #        data <list - tibble of breath  data> | breathCount <num> |
  #        breakCount <num> | pressuresDailyCount <num> | pressuresDayCompletenessScore <num>
  #
  # Returns:
  #   pressureData with aggregation columns:
  #      breathsCountDay,  treatmentCountDay, breathsCountWeek, treatmentCountWeek

  pressureDay <- pressureData %>%
    group_by(patientId, actDate) %>%
    mutate(
      breathsCountDay = sum(breathCount) * pressuresDayCompletenessScore,
      treatmentCountDay = n() * pressuresDayCompletenessScore,
      actWeek = week(actDate),
      year = year(actDate)
    ) %>%
    select(patientId, treatmentId, actDate, year, actWeek, breathsCountDay, treatmentCountDay)

  pressureDayTop1 <-  pressureDay %>%
    group_by(patientId, actDate) %>%
    arrange(actDate) %>%
    filter(row_number() == 1)

  pressureWeekly <- pressureDayTop1 %>%
    group_by(patientId, year, actWeek) %>%
    mutate(
      breathsCountWeek = sum(breathsCountDay),
      treatmentCountWeek = sum(treatmentCountDay)
    ) %>%
    arrange(patientId, actDate) %>%
    filter(row_number() == 1) %>%
    select(patientId, actWeek, year,breathsCountWeek, treatmentCountWeek)

  features <- left_join(pressureDay, pressureWeekly,
                        by = c("patientId", "actWeek","year"), all.x = TRUE)
  pressureData <- inner_join(pressureData, features,
                             by = c("treatmentId", "patientId", "actDate"), all.x = TRUE)
  pressureData
}


FillMissingPrescriptionData <- function(prescData) {
  # Replace missing values in the prescription with defaults.
  #
  # Args:
  #   prescData: prescribed ACT routine data.frame
  #
  # Returns:
  #    data.frame where NAs for prescription data have been filled with the default values
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
  # Calculate ACT adherence features.
  #
  # Args:
  #   pressureData: preprocessed ACT data
  #        treatmentId <chr> | patientId <chr> | actDate <POSIXct> |
  #        data <list - tibble of breath data> | breathCount <num> |
  #        breakCount <num> | pressuresDailyCount <num> | pressuresDayCompletenessScore <num>
  #
  # Returns:
  #    data.frame with new features
  #        treatmentId <chr> | patientId <chr> | actDate <POSIXct> |
  #        pressuresDayCompletenessScore <num> | treatmentDayAdherenceScore <num> |
  #        breathsDayAdherenceScore <num> | treatmentWeekAdherenceScore <num> |
  #        breathsWeekAdherenceScore <num>

  pressureData <- AggTreatmentsByDayAndWeek(pressureData)
  devicesData <- FillMissingPrescriptionData(devicesData)

  pressureData <- merge(pressureData, devicesData, by = "patientId")
  pressureData <- pressureData %>%
    mutate(treatmentDayAdherenceScore = treatmentCountDay / prescrTreatmentsDay,
           breathsDayAdherenceScore = breathsCountDay / prescrTreatmentsDay /
                                      prescrNSets / prescrNBreaths,
           treatmentWeekAdherenceScore = treatmentCountWeek / prescrTreatmentsDay / 7,
           breathsWeekAdherenceScore = breathsCountWeek / prescrTreatmentsDay /
                                       prescrNSets / prescrNBreaths / 7) %>%
    select(patientId, treatmentId, actDate, pressuresDayCompletenessScore,
           treatmentDayAdherenceScore, breathsDayAdherenceScore,
           treatmentWeekAdherenceScore, breathsWeekAdherenceScore)

  pressureData
}
