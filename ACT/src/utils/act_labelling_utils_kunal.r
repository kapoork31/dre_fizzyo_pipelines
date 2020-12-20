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

source(
    file.path(
        "~",
        "scripts",
        "ACT_v3",
        "act_pipeline",
        "src",
        "utils",
        "act_validation_utils_kunal.R"
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
        "act_schema_utils_kunal.R"
    )
)
options(digits.secs = 3)

label_act_data <- function(clean_act_data, metadata) {
  # ==== ENTRYPOINT FUNCTION ====
  # Identifies and labels breaths and breaks in a session,
  # and adds a column for their counts.
  #
  # Args:
  #   clean_act_data: Data.frame of cleaned ACT data - see schema
  #   metadata: Data.frame of ACT treatment metadata - see schema
  #
  # Returns:
  #   labelled_act_data: Data.frame of labelled ACT data - see schema

  # validate raw data and metadata schema
  validate_data_schema(clean_act_data, clean_act_schema)
  validate_data_schema(metadata, act_metadata_schema2)

  # check that the metadata contains all the sessions in pressure data
  if (!all(unique(clean_act_data$session_id) %in%
    unique(metadata$session_id))) {

    stop("Metadata dataframe doesn't contain all sessions in input dataframe")
  }

  labelled_act_data <- clean_act_data %>%
    inner_join(metadata[c("session_id",
        "treatment_id",
        "session_start")],
        by = "session_id") %>%
    # identify breaths in a session, set, or treatment
    group_by(patient_id, treatment_id) %>%
    arrange(session_start) %>%
    mutate(
      prominent_peak = indexes(pressure_detrend),
      breath_id = tag_breath_ids(pressure_detrend, prominent_peak),
      break_id = tag_break_ids(pressure_detrend, prominent_peak),
      set_id = tag_sets(breath_id, pressure_detrend),
      breath_count = count_ids(breath_id),
      break_count = count_ids(break_id),
      set_count = count_ids(set_id)
    ) %>%
    ungroup()
}


indexes <- function( y, thres = 8, min_dist = 5, thresh_abs = TRUE){

  min_dist <- as.integer(min_dist)
  if (thresh_abs == FALSE){

    thres <- thres * (max(y) - min(y)) + min(y);
  }

  dy <- diff(y);

  zeroes <- which(dy == 0)

  while (length(zeroes) > 0){

    zeroesr <- c()
    zeroesl <- c()
    zeroesr <- dy[2:length(dy)] # everytthing but first
    zeroesr[length(zeroesr) + 1] <- 0

    zeroesl <- dy[1:(length(dy) - 1)] # everything but last
    zeroesl <- c(c(0), zeroesl)

    dy <- replace(dy, zeroes, zeroesr[zeroes])
    zeroes <- which(dy == 0)

    dy <- replace(dy, zeroes, zeroesl[zeroes])
    zeroes <- which(dy == 0)

  }

  dy1 <- c(dy, c(0))
  dy2 <- c(c(0), dy)
  peaks <- which(dy1 < 0 & dy2 > 0 & y > thres)

  rem <- rep(FALSE, length(y))

  if (length(peaks) > 1 & length(min_dist > 1)){

    highest <- rev(peaks[order(y[peaks])])

    rem[peaks] <- TRUE

    for (peak in highest){

      if (rem[peak] == TRUE){

        floor <- max(0, peak - min_dist)
        ceiling <- min(peak + min_dist, length(rem))
        rem[floor:ceiling] <- FALSE
        rem[peak] <- TRUE
      }

    }

  }
  return (rem)

}

get_breath_endpoints <- function(points, peak, threshold) {
    # Identify the endpoints of a breath by moving left
    #   and right from a peak until the value
    #   is less than a threshold.
    #
    # Args:
    #   peak: Index of peak in signal
    #   points: Vector of values representing signal
    #   threshold: Lower boundary for signal
    #
    # Returns:
    #   vector containing (left_endpoint, right_endpoint)

    left_endpoint  <- peak
    right_endpoint <- peak

    left_endpoint_found  <- FALSE
    right_endpoint_found <- FALSE

    while (!left_endpoint_found || !right_endpoint_found) {

        if (!left_endpoint_found) {

            left_endpoint <- max(left_endpoint - 1, 1)
            left_endpoint_found <- (points[left_endpoint] <= threshold ||
                left_endpoint == 1)
        }
        if (!right_endpoint_found) {

            right_endpoint <- min(right_endpoint + 1, length(points))
            right_endpoint_found <- (points[right_endpoint] <= threshold ||
                right_endpoint == length(points))
        }
    }

    c(left_endpoint, right_endpoint)
}


tag_breath_ids <- function(points, peaks, threshold = 2) {
  # Creates a vector of breath ids to be applied to a session.
  #     Breaths are id'd sequentially from
  #     1, and padded with zeros to indicate
  #     that the points are not a breath.

  #     Threshold parameter determines lower
  #         boundary for start and end of a breath.
  #
  # Args:
  #   points: A vector of points representing a signal
  #   peaks: A boolean vector of length(points)
  #     where  TRUE indicates that a point is a peak
  #   threshold: Threshold to cutoff breath endpoints (default: 2)
  #
  # Returns:
  #   A vector of breath ids for a session

  peak_indices <- which(peaks)

  ids <- rep(0, length(points))
  id  <- 1

  for (peak in peak_indices) {

    end_points <- get_breath_endpoints(points, peak, threshold)
    ids[end_points[1]:end_points[2]] <- id
    id <- id + 1
  }

  ids
}


tag_break_ids <- function(points, peaks, threshold = 2) {
  # Creates a vector of break ids to be applied to a session.
  #     Somewhat opposite its counterpart
  # TagBreathIds, the vector is prepopulated with 1s,
  #     since we assume whatever is not a breath
  #     is a break. Any further breaks are sequential from
  #     2 and start counting the row after a
  #     completed breath. Periods of a breath are padded with 0s.
  #
  # Args:
  #   points: A vector of points representing a signal
  #   peaks: A boolean vector of length(points) where
  #     TRUE indicates that a point is a peak
  #   threshold: Threshold to cutoff breath endpoints (default: 2)
  #
  # Returns:
  #   A vector of breath ids for a session

  peak_indices <- which(peaks)

  ids <- rep(1, length(points))
  # defaulted to 1 since the absence of a breath is 1 break
  id  <- 2

  for (peak in peak_indices) {

    end_points <- get_breath_endpoints(points, peak, threshold)
    end_breath <- min(end_points[2] + 1, length(points))
    ids[end_points[1]:end_points[2]] <- 0
    ids[end_breath:length(points)]  <- id
    id <- id + 1
  }

  ids
}



determine_sets <- function(breath_ids, pressure_detrend) {
  # determine if treatment has sets or not.Based on breathIds column.
  # attempts to find gaps in breaths that represent gaps in sets.
  # once sets identified, then determines these are good sets.
  # sets found analysed by number of breaths inside them.

  # Args:
  #   breathIds: contains ids of breaths at respective indexes.

  # Returns
  #   1 if sets identifiable, 0 if not identifiable.

  real_breath_set <- 2 # number of breaths in set for it to be real
  outlier_floor <- 1 # minimum number of sets
  outlier_ceiling <- 30 # max number of sets
  avg_breath_floor <- 4 # minimum average breaths per set
  avg_breath_ceiling <- 25 # max average breaths per set
  sd_baseline <- 3.5 # max sd
  sd_leniant_baseline <- 5.5 # max sd with high average breaths per set
  k_prop_cut_off <- 2 # Heuristic cut - off used to find the boundaries of sets,
    # sets identified when a gap is 3 times bigger then previous gap
    # reason its 2 is because prop diff is 2 for something 3x bigger,
    # so prop diff of 20 - 60 is 2 not 3

  minimum_breaths <- 8
    # minimum breaths in treatment for sets to be even counted,
    # else no sets exist

  big_set <- 8
  peaks <- find_peak_index(breath_ids, pressure_detrend)
  if (length(peaks) <= minimum_breaths) {

    return (0)
  }
  gaps <- diff(peaks) # find first order of difference in peaks

  prop <- diff(gaps) / gaps[-length(gaps)]
    # get proportional difference of gaps,
    # each value is a proportional of the previous gap

  outliers <- which(prop >= k_prop_cut_off)
    # which gaps are 3 times bigger than the previous gap

  avg_breath_between_outliers <- mean(diff(outliers))

  if (length(outliers) > outlier_floor & length(outliers)
    <= outlier_ceiling & avg_breath_between_outliers >= avg_breath_floor){
    # if average of 4 breaths per set

    actual_outliers <- c()
    for ( i in 1:(length(outliers) - 1)){

      outlier <- outliers[i]
      nex <- outliers[i + 1]
      if ( (nex - outlier) > real_breath_set){
        actual_outliers <- c(actual_outliers, outlier)
      }
      if (i == (length(outliers) - 1)){

        actual_outliers <- c(actual_outliers, nex)
      }

    }
    outliers <- actual_outliers
        # have removed all small sets and merged them into bigger sets

    if (length(outliers) > outlier_floor &
        length(outliers) <= outlier_ceiling) {
            # more than 1 outlier

      # if big gaps exist
      breaths_inside_gaps <- c(outliers[1], diff(outliers))
      breaths_inside_gaps <- c(breaths_inside_gaps,
        length(peaks) - outliers[length(outliers)])

      # determine number of breaths in a set
      avg_breaths_in_set <- mean(breaths_inside_gaps) # avg breaths in a  set
      sd_breaths_in_set <- sd(breaths_inside_gaps)
      # find absolute difference to 10 of breaths_inside_gaps
      if ( (avg_breaths_in_set >= avg_breath_floor) &
        (avg_breaths_in_set <= avg_breath_ceiling)) {

        # if avg breatsh in set is >=4 and <= 25
        if (avg_breaths_in_set < big_set & sd_breaths_in_set <= sd_baseline){

          return (1)
        } else if (avg_breaths_in_set >= big_set &
            sd_breaths_in_set <= sd_leniant_baseline){

          return (1)
        } else{
          return (0)
        }

      } else{

        return (0)
      }
    }else{

      return (0)
    }

  }else{

    return (0)
  }

}


tag_sets <- function(breath_ids, pressure_detrend){
  # Function which tags breath peaks as belonging to each set.
  #   Used when peaks have already been identified (prominentPeaks),
  #   and you want a new column showing their membership of each set
  #
  # Args:
  #   breath_ids: a vector of unique breath_ids identifying each breath
  #
  # Returns:
  #   A vector (set_id) containing the integer value of the set ID of each peak
  #     If there are no discernible sets (decided by determine_sets() function),
  #     then all set_ids will remain 0

  # Heuristic cut-off used to find the boundaries of sets
  k_prop_cut_off <- 2

  # Create vector of zeros to overwrite with set IDs
  set_id <- rep(0, length(breath_ids))

  # Find where the peaks are
  peak_indices <- find_peak_index(breath_ids, pressure_detrend)


  # Find first order of difference in peaks
  gaps <- diff(peak_indices)

  # Get proportional difference of gaps,
  # each value is a proportion of the previous gap (leave out the last gap)
  prop <- diff(gaps) / gaps[- length(gaps)]

  # which gaps are 3 times bigger than the previous gap?
  # these gaps demarcate the beginning and end of sets
  set_boundaries <- which(prop >= k_prop_cut_off)

  # Check whether there are sensible, discernible sets to tag
  # If there aren't any, then all set_ids will remain zero

  if (determine_sets(breath_ids, pressure_detrend) == 1){
    # if sets identifiable, proceed to tagging set_ids

    # append first and last gap, to all the gaps between sets:
    set_boundaries <- c(0, set_boundaries, length(peak_indices))

    # use to iterate through each set in the treatment
    set_counter <- 1:(length(set_boundaries) - 1)

    for (i in set_counter){
      # note on convention:
      # peak n, gap n-1, peak n+1, gap n
      first_peak_of_set <- set_boundaries[i] + 2
      last_peak_of_set <- set_boundaries[i + 1] + 1

      peaks_in_set <- peak_indices[first_peak_of_set : last_peak_of_set]

      set_id[c(peaks_in_set)] <- i
    }
  }

  set_id
}


find_peak_index <- function(breath_ids, pressure_detrend){

  # Given a vector of unique breath_ids, this function finds the index of the
  # midpoint of each breath.
  # Returns the indices of breath midpoints.

  breath_d_vals <- unique(breath_ids)

  # Remove zero ID
  breath_d_vals <- breath_d_vals[!breath_d_vals == 0]
  k <- 1 # iterator
  # Initialise vector to store peak indices
  peak_indices <- 0

  for (i in breath_d_vals){

    # Breath width is all the points which have that breath ID
    breath_indexes <- which(breath_ids == i)
    breath_pressure_detrend <- pressure_detrend[breath_indexes]
    peak_index <- which.max(breath_pressure_detrend)
    # Mark the peak at its midpoint
    peak_indices[k] <- breath_indexes[peak_index]
    k <- k + 1
  }

  peak_indices
}



count_ids <- function(vector) {
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
