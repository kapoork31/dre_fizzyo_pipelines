####################################################################################################
# act_labelling_utils.R
#
# This script contains utilites for labelling cleaned ACT data.
#
# Entrypoint Function: LabelACTData()
#
# ------------------------------------------------------------------------------------------------
#
# Functions in this file:
#
# LabelACTData: Identifies and labels breaths in clean ACT data.
# DetectPeaks: Detects peaks in a vector of detrended ACT pressure values.
# GetBreathEndpoints: Finds endpoints for a peak in a vector of detrended ACT pressure values.
# TagBreathIds: Creates a vector of ids from a vector of peaks and a vector of detrended ACT
#               pressure values.
# TagBreakIds: Creates a vector of ids from a vector of peaks and a vector of detrended ACT
#              pressure values.
#
####################################################################################################

# The relative path below should be changed whn used on the DRE
source("~/scripts/ACT_v3/1_featurise/utils/act_validation_utils.R")
source("~/scripts/ACT_v3/1_featurise/utils/act_schema_utils.R")


LabelACTData <- function(cleanACTData, metadata) {
  # ==== ENTRYPOINT FUNCTION ====
  # Identifies and labels breaths and breaks in a session, and adds a column for their counts.
  #
  # Args:
  #   cleanACTData: Data.frame of cleaned ACT data - see schema
  #   metadata: Data.frame of ACT treatment metadata - see schema
  #
  # Returns:
  #   labelledACTData: Data.frame of labelled ACT data - see schema

  # validate raw data and metadata schema
  ValidateDataSchema(cleanACTData, CleanACTSchema)
  ValidateDataSchema(metadata, ACTMetadataSchema)

  # check that the metadata contains all the sessions in pressure data
  if (!all(unique(cleanACTData$sessionId) %in% unique(metadata$sessionId))) {
    stop("Metadata dataframe does not contain all sessions from input dataframe")
  }

  cleanACTData %>%
    inner_join(metadata[c("sessionId", "treatmentId")], by = "sessionId") %>%
    # identify breaths in a session, set, or treatment
    group_by(patientId, treatmentId) %>%
    arrange(time) %>%
    mutate(
      prominentPeak = DetectPeaks(pressureDetrend),
      breathId = TagBreathIds(pressureDetrend, prominentPeak),
      breakId = TagBreakIds(pressureDetrend, prominentPeak),
      setId = TagSets(breathId),
      breathCount = CountIds(breathId),
      breakCount = CountIds(breakId),
      setCount = CountIds(setId)
    ) %>%
    ungroup()
}


DetectPeaks <- function(points, minAmplitude = 10, minDistance = 5) {
  # Detects peaks in a session.
  #
  # Args:
  #   points: Vector of values representing a signal
  #   minAmplitude: A peak must have at least a pressure value of minAmplitude
  #   minDistance: Peaks must be at least minDistance measurements apart
  #
  # Returns:
  #   A boolean vector of peaks

  slopeLeft <- points - dplyr::lag(points, default = 0)
  slopeRight <- dplyr::lead(slopeLeft, default = 0)
  peaks <- slopeLeft > 0 & slopeRight < 0 & (points > minAmplitude)

  # Scan for multiple peaks in a neighborhood (minDistance)
  # and keep the most prominent peaks
  peakIndices <- which(peaks)

  # For each peak, get neighborhood of peaks and
  # find the max peak in that neighborhood
  prominentPeakIndices <- c()
  peakIndex <- peakIndices[1]
  while (length(peakIndices) > 0) {
    peakIndicesInNeighborhood <- peakIndices[
      (peakIndices > (peakIndex - minDistance)) &
       peakIndices < (peakIndex + minDistance)
    ]

    # Find max peak index in neighborhood and add it to prominentPeakIndices
    for (currentIndex in peakIndicesInNeighborhood) {
      if (points[currentIndex] == max(points[peakIndicesInNeighborhood])) {
        prominentPeakIndices <- c(prominentPeakIndices, currentIndex)
      }
    }

    # Remove peaks in neighborhood from peakIndices,
    # now that we found the max peak in that neighborhood
    peakIndices <- setdiff(peakIndices, peakIndicesInNeighborhood)
    peakIndex <- peakIndices[1]
  }

  # Create boolean array of size points with TRUE values
  # at all of the prominentPeakIndices values
  peaks <- sapply(seq(1, length(points)), function(index) {
    ifelse ((index %in% prominentPeakIndices), TRUE, FALSE)
  })

  peaks
}


GetBreathEndpoints <- function(points, peak, threshold) {
    # Identify the endpoints of a breath by crawling left and right from a peak until the value
    #   is less than a threshold.
    #
    # Args:
    #   peak: Index of peak in signal
    #   points: Vector of values representing signal
    #   threshold: Lower boundary for signal
    #
    # Returns:
    #   vector containing (LeftEndpoint, RightEndpoint)

    leftEndpoint  <- peak
    rightEndpoint <- peak

    leftEndpointFound  <- FALSE
    rightEndpointFound <- FALSE

    while (!leftEndpointFound || !rightEndpointFound) {
        if (!leftEndpointFound) {
            leftEndpoint <- max(leftEndpoint - 1, 1)
            leftEndpointFound <- (points[leftEndpoint] <= threshold || leftEndpoint == 1)
        }
        if (!rightEndpointFound) {
            rightEndpoint <- min(rightEndpoint + 1, length(points))
            rightEndpointFound <- (points[rightEndpoint] <= threshold ||
            rightEndpoint == length(points))
        }
    }

    c(leftEndpoint, rightEndpoint)
}


TagBreathIds <- function(points, peaks, threshold = 2) {
  # Creates a vector of breath ids to be applied to a session. Breaths are id'd sequentially from
  #    1, and padded with zeros to indicate that the points are not a breath. Threshold parameter
  #    determines lower boundary for start and end of a breath.
  #
  # Args:
  #   points: A vector of points representing a signal
  #   peaks: A boolean vector of length(points) where  TRUE indicates that a point is a peak
  #   threshold: Threshold to cutoff breath endpoints (default: 2)
  #
  # Returns:
  #   A vector of breath ids for a session

  peakIndices <- which(peaks)

  ids <- rep(0, length(points))
  id  <- 1

  for (peak in peakIndices) {
    endpoints <- GetBreathEndpoints(points, peak, threshold)
    ids[endpoints[1]:endpoints[2]] <- id
    id <- id + 1
  }

  ids
}


TagBreakIds <- function(points, peaks, threshold = 2) {
  # Creates a vector of break ids to be applied to a session. Somewhat opposite its counterpart
  #   TagBreathIds, the vector is prepopulated with 1s, since we assume whatever is not a breath
  #   is a break. Any further breaks are sequential from 2 and start counting the row after a
  #   completed breath. Periods of a breath are padded with 0s.
  #
  # Args:
  #   points: A vector of points representing a signal
  #   peaks: A boolean vector of length(points) where TRUE indicates that a point is a peak
  #   threshold: Threshold to cutoff breath endpoints (default: 2)
  #
  # Returns:
  #   A vector of breath ids for a session

  peakIndices <- which(peaks)

  ids <- rep(1, length(points)) # defaulted to 1 since the absence of a breath is 1 break
  id  <- 2

  for (peak in peakIndices) {
    endpoints <- GetBreathEndpoints(points, peak, threshold)
    endBreath <- min(endpoints[2] + 1, length(points))
    ids[endpoints[1]:endpoints[2]] <- 0
    ids[endBreath:length(points)]  <- id
    id <- id + 1
  }

  ids
}


TagSets <- function(breathIds){
  # Function which tags breath peaks as belonging to each set.
  #   Used when peaks have already been identified (prominentPeaks),
  #   and you want a new column showing their membership of each set
  #
  # Args:
  #   breathIds: a vector of unique breathIds identifying each breath
  #
  # Returns:
  #   A vector (setId) containing the integer value of the set ID of each peak
  #     If there are no discernible sets (decided by determineSets() function),
  #     then all setIds will remain 0

  # Heuristic cut-off used to find the boundaries of sets
  kpropCutOff <- 3

  # Create vector of zeros to overwrite with set IDs
  setId <- rep(0, length(breathIds))

  # Find where the peaks are
  peakIndices <- FindBreathCentres(breathIds)

  # Find first order of difference in peaks
  gaps <- diff(peakIndices)

  # Get proportional difference of gaps,
  # each value is a proportion of the previous gap (leave out the last gap)
  prop <- diff(gaps) / gaps[- length(gaps)]

  # which gaps are 3 times bigger than the previous gap?
  # these gaps demarcate the beginning and end of sets
  setBoundaries <- which(prop >= kpropCutOff)

  # Check whether there are sensible, discernible sets to tag
  # If there aren't any, then all setIds will remain zero

  if (DetermineSets(breathIds) == 1){ # if sets identifiable, proceed to tagging setIds

    # append first and last gap, to all the gaps between sets:
    setBoundaries <- c(0, setBoundaries, length(peakIndices))

    # use to iterate through each set in the treatment
    setCounter <- 1:(length(setBoundaries) - 1)

    for (i in setCounter){
      # note on convention:
      # peak n, gap n-1, peak n+1, gap n
      firstPeakOfSet <- setBoundaries[i] + 2
      lastPeakOfSet <- setBoundaries[i + 1] + 1

      peaksInSet <- peakIndices[firstPeakOfSet : lastPeakOfSet]

      setId[c(peaksInSet)] <- i
    }
  }

  setId
}


FindBreathCentres <- function(breathIds){
  # Given a vector of unique breathIds, this function finds the index of the
  # midpoint of each breath.
  # Returns the indices of breath midpoints.

  breathIdVals <- unique(breathIds)

  # Remove zero ID
  breathIdVals <- breathIdVals[!breathIdVals == 0]
  k <- 1 # iterator
  # Initialise vector to store peak indices
  peakIndices <- 0

  for (i in breathIdVals){
    # Breath width is all the points which have that breath ID
    breathWidth <- which(breathIds == i)
    midpoint <- round(mean(breathWidth))

    # Mark the peak at its midpoint
    peakIndices[k] <- midpoint
    k <- k + 1
  }

  peakIndices
}


DetermineSets <- function(breathIds) {
  # determine if treatment has sets or not.Based on breathIds column.
  # attempts to find gaps in breaths that represent gaps in sets.
  # once sets identified, then determines these are good sets.
  # sets found analysed by number of breaths inside them.

  # Args:
  #   breathIds: contains ids of breaths at respective indexes.

  # Returns
  #   1 if sets identifiable, 0 if not identifiable.

  kPropCutOff <- 3 # Heuristic cut - off used to find the boundaries of sets
  # absolute difference between breaths in a set to 10 must be <= 4 to be considered a set.
  kabsDifCutOff <- 4
  minimumBreaths <- 8 # minimum breaths in treatment
  assumedBreathsPerSet <- 8 # number of breaths per set, will be updated to be personal
  peaks <- FindBreathCentres(breathIds)
  if (length(peaks) <= minimumBreaths) {
  # if no breaths return 0
    return (0)
  }
  gaps <- diff(peaks) # find first order of difference in peaks
  prop <- diff(gaps) / gaps[-length(gaps)]# get proportional difference of gaps,
  # each value is a proportional of the previous gap
  outliers <- which(prop >= kPropCutOff) # which gaps are 3 times bigger than the previous gap
  if (length(outliers) > 15 & length(peaks) < 100) {
    return (0)
  } else if (length(outliers) >= 1) {
    if (length(outliers) + 1 >= ((length(peaks) / assumedBreathsPerSet) - 2)) {
      # if big gaps exist
      BreathsinsideGaps <- c(outliers[1], diff(outliers))
      BreathsinsideGaps <- c(BreathsinsideGaps, length(peaks) - outliers[length(outliers)])
      # determine number of breaths between big gaps
      absDif <- mean(abs(BreathsinsideGaps - assumedBreathsPerSet))
      # find absolute difference to 10 of BreathsinsideGaps
      if (absDif <= kabsDifCutOff) {
      # if absDif <= 4, then sets identifiable
        return (1)
      } else {
        return (0)
      }
    } else {
      return (0)
    }
  }
  return (0)
}

CountIds <- function(vector) {
  # Counts the total number of unique non zeros in a vector
  #
  # Args:
  #   vector: Of which to count unique non zeros
  #
  #
  # Returns:
  #   Number of unique non zeros in the vector

  length(unique(vector[vector != 0]))
}
