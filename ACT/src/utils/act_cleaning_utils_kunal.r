##################################################
# act_cleaning_utils.R
#
# This file takes raw ACT data and "cleans" it by:
#   (1) validating the schema, (2) renaming columns,
#    (3) dropping NA values, and (4) detrending the signal.
#
# Entrypoint Function: CleanRawACTData()
#
# -----------------------------------------------
#
# Functions in this file:
# * CleanACTData: update column names, drop NAs from data,
#     detrend pressure measurements
# * DetrendSignal: Drift in the ACT pressure measurements can be
#     corrected using the BEADS algorithm
# * Beads, etc.:
#
# NB: some functions have been
#   renamed to conform to our linting standards, but are
#     unchanged functionality-wise
#
###################################################

if (!require("functional")) install.packages("functional")
if (!require("Matrix")) install.packages("Matrix")
library(functional)
library(Matrix)
options(digits.secs = 3)

# The relative path below should be changed whn used on the DRE
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

clean_raw_act_data <- function(raw_act_data) {
  # ==== ENTRYPOINT FUNCTION ====
  # When provided raw ACT data, this function changes
  #   column names to be standardized format, drops
  #   NAs, makes the calls to detrend the recorded
  #   signal for expired breaths (on a session level).
  #
  # Args:
  #   raw_act_data: Tibble of ACT data (see RawACTSchema)
  #
  # Returns:
  #   cleaned ACT data
  #     patientId <chr> | sessionId <chr> |
  #     time <dttm> | pressureDetrend <dbl>

  validate_data_schema(raw_act_data, raw_act_schema)
  # Need to modify digits for seconds
  #   in order for duplication to work

  raw_act_data <- raw_act_data %>%
    rename(
      "session_id" = "id",
      "pressure_values" = "pressurevalues",
      "patient_id" = "patient_record_id"
    ) %>%
    mutate(
      session_id = as.character(session_id),
      pressure_values = as.numeric(pressure_values),
      time = lubridate::ymd_hms(time),
      patient_id = as.character(patient_id)
    ) %>%
    drop_na %>%
    group_by(session_id) %>%
    arrange(time) %>%
    mutate(duplicate = tag_duplicate_points(pressure_values)) %>%
    filter(duplicate == FALSE) %>%
    mutate(time = fix_deduplicated_time(time)) %>%
    mutate(pressure_detrend = detrend_signal(pressure_values)) %>%
    filter(pressure_detrend >= -10 & pressure_detrend <= 120) %>%
    select(patient_id, session_id, time,
           pressure_detrend, -duplicate, -pressure_values) %>%
    ungroup()

  raw_act_data
}

detrend_signal <- function(pressures) {
  # Detrend a signal using BEADS
  # (Baseline Estimation And Denoising with Sparsity)
  #
  # Args:
  #   pressures: A 1-dimensional vector of pressure values
  #                 (i.e. no column header)
  #
  # Returns:
  #   A detrended signal

  # There are no input checks inside
  #   the BEADS algorithm, so here we first check that
  # input signal has a valid size,
  #   to avoid getting a cryptic error from BEADS down the line.

  # If there are too few points to detrend,
  #   pass back the original pressure values, unmodified:
  if (length(pressures) < 3) {

    return(pressures)
  }

  # Set various BEADS constants:
  # You can find the details of these parameters
  #  in the original BEADS paper
  # These parameter names and values come
  #  straight from the supplementary material (sample code)
  # with some tweaking for this data

  # The most important parameter to tune is the
  #   cutoff frequency of the High Pass Filter:
  fc <- 0.005
  # fc : cut-off frequency (cycles/sample) = 1/200. 200 points = 20 seconds.
  # Baseline drift is in the order of 20 seconds

  d <- 1
  # d : filter order parameter (d = 1 or 2)
  r <- 6
  # r : asymmetry ratio. Positivity bias (peaks are positive).
  #   1 for symmetric peaks

  # Regularization parameters:
  amp <- 0.1
  # Low values if noise is low. High value will apply significant de-noising.
  # We want to apply very light de-noising, as we want detrended values to be
  # similar to original values
  lam0 <- 0.5 * amp
  # lambda values are manually tuned. Inversely proportional to the sparsity
  # of the signal and the signal derivative
  lam1 <- 5 * amp
  lam2 <- 4 * amp

  result <- beads(pressures, d, fc, r, lam0, lam1, lam2)
  pressure_detrend <- result$x@x
  # x is the object containing the pressure with baseline removed
  pressure_detrend
}


# Reference:
# Chromatogram baseline estimation and denoising using sparsity (BEADS)
# Xiaoran Ning, Ivan W. Selesnick, Laurent Duval
# Chemometrics and Intelligent Laboratory Systems (2014)
# > DOI: 10.1016/j.chemolab.2014.09.014
# Available online 30 September 2014
#####################################################


## Handle function ##
phi_v1 <- function(x, eps1) {

  sqrt(abs(x) ^ 2 + eps1)
}

w_fun_v1 <- function(x, eps1) {

  1 / (sqrt(abs(x) ^ 2 + eps1))
}

phi_v2 <- function(x, eps1) {

  abs(x) - eps1 * log(abs(x) + eps1)
}

w_fun_v2 <- function(x, eps1) {

  1 / (abs(x) + eps1)
}

theta <- function(x, eps0, r) {

  sum(x[which(x > eps0)]) - (r * sum(x[which(x < -eps0)])) +
    sum( (1 + r) / (4 * eps0) * x[which(abs(x) <= eps0)] ^ 2 +
          (1 - r) / 2 * x[which(abs(x) <= eps0)] + eps0 * (1 + r) / 4)
}

h <- function(x, a, b) {

  b %*% solve(a, x)
}

beads <- function(y, d, fc, r, lam0, lam1, lam2) {
  # Baseline estimation and denoising using sparsity (BEADS)
  #
  # INPUT
  #   y: Noisy observation
  #   d: Filter order (d = 1 or 2)
  #   fc: Filter cut-off frequency (cycles/sample) (0 < fc < 0.5)
  #   r: Asymmetry ratio
  #   lam0, lam1, lam2: Regularization parameters
  #
  # OUTPUT
  #   x: Estimated sparse-derivative signal
  #   f: Estimated baseline
  #   cost: Cost function history

  # The following parameter may be altered.
  nit <- 30
  # nit: Number of iterations
  pen <- "l1_v2"
  # pen : penalty function for sparse derivative ('l1_v1' or 'l1_v2')
  eps0 <- 1e-6
  # cost smoothing parameter for x (small positive value)
  eps1 <- 1e-6
  # cost smoothing parameter for derivatives (small positive value)

  switch(pen,
         l1_v1 = {

           phi <- Curry(phi_v1, eps1 = eps1)
           wfun <- Curry(w_fun_v1, eps1 = eps1)
         },
         l1_v2 = {

           phi <- Curry(phi_v2, eps1 = eps1)
           wfun <- Curry(w_fun_v2, eps1 = eps1)
         },

         res = {

           cat("penalty must be l1_v1, l1_v2")
           x <- c()
           cost <- c()
           f <- c()
           return()
         })

  y <- as.vector(y)
  x <- y
  cost <- matrix(0, nrow = 1, ncol = nit)

  len_y <- length(y)
  ba_filt_res <- ba_filt(d, fc, len_y)
  a <- ba_filt_res$cap_a
  cap_b <- ba_filt_res$cap_b

  e <- matrix(1, nrow = len_y - 1, ncol = 1)
  diag_d1 <- vector("list", 2)
  diag_d1[[1]] <- -e
  diag_d1[[2]] <- e

  diag_d2 <- vector("list", 3)
  diag_d2[[1]] <- e
  diag_d2[[2]] <- -2 * e
  diag_d2[[3]] <- e

  d1 <- bandSparse(len_y - 1, len_y, k = c(0, 1), diagonals = diag_d1)
  d2 <- bandSparse(len_y - 2, len_y, k = c(0, 1, 2), diagonals = diag_d2)
  d_bind <- rbind(d1, d2)

  btb <- t(cap_b) %*% cap_b

  w <- c(lam1 * matrix(1, nrow = len_y - 1, ncol = 1),
         lam2 * matrix(1, nrow = len_y - 2, ncol = 1))

  b <- (1 - r) / 2 * matrix(1, nrow = len_y, ncol = 1)
  d <- btb %*% solve(a, y) - lam0 * t(a) %*% b

  gamma <- matrix(1, nrow = len_y, ncol = 1)

  for (i in 1:nit) {

    diag <- vector("list", 1)
    diag[[1]] <- w * wfun(d_bind %*% x)
    lambda <- bandSparse( (2 * len_y) - 3, k = 0, diagonals = diag)
    k <- which(abs(x) > eps0)
    gamma[!k] <- ( (1 + r) / 4) / abs(eps0)
    gamma[k] <- ( (1 + r) / 4) / abs(x[k])
    diag_g <- vector("list", 1)
    diag_g[[1]] <- gamma
    cap_gamma <- bandSparse(len_y, k = 0, diagonals = diag_g)

    m <- (2 * lam0 * cap_gamma) + (t(d_bind) %*% lambda %*% d_bind)
    x <- a %*% (solve(btb + t(a) %*% m %*% a, d))

    cost[i] <- 0.5 * sum(abs(h(y - x, a, cap_b)) ^ 2)
        + lam0 * theta(x, eps0, r) + lam1 * sum(phi(diff(x)))
        + lam2 * sum(phi(diff(x, differences = 2)))
  }

  f <- y - x - h(y - x, a, cap_b)

  list(x = x, f = f, cost = cost)
}



ba_filt <- function(d, fc, n) {
  # [A, B] = BAfilt(d, fc, N)
  # Banded matrices for zero-phase high-pass filter.
  # The matrices are 'sparse' data type in MATLAB.
  # INPUT
  #   d  : degree of filter is 2d (use d = 1 or 2)
  #   fc : cut-off frequency (normalized frequency, 0 < fc < 0.5)
  #   N  : length of signal

  b1 <- c(1, -1)
  if (d > 1) {

    for (i in 1:(d - 1)) {

      b1 <- convolve(b1, rev(c(-1, 2, -1)), type = "open")
    }
  }

  b <- convolve(b1, rev(c(-1, 1)), type = "open")

  omc <- 2 * pi * fc
  t <- ( (1 - cos(omc)) / (1 + cos(omc))) ^ d

  a <- 1
  for (i in 1:d) {

    a <- convolve(a, rev(c(1, 2, 1)), type = "open")
  }

  a <- b + (t * a)
  nb_diag <- length(0:d)
  diag_a <- vector("list", nb_diag)
  diag_b <- vector("list", nb_diag)

  for (i in 0:d) {

    diag_a[[i + 1]] <- a[d - i + 1] * matrix(1, nrow = 1, ncol = (n - i))
    diag_b[[i + 1]] <- b[d - i + 1] * matrix(1, nrow = 1, ncol = (n - i))
  }

  cap_a <- bandSparse(n, k = c(0:d), diagonals = diag_a, symm = TRUE)
  cap_b <- bandSparse(n, k = c(0:d), diagonals = diag_b, symm = TRUE)

  list(cap_a = cap_a, cap_b = cap_b)
}


tag_duplicate_points <- function(points, duplicate_size = 9) {
  # Creates a vector of booleans where each value is TRUE if and only if
  # corresponding index in points is a duplicate value. The function first
  # computes the duplicateLevel. It tags the first duplicate_size points as
  # FALSE and keeps the next duplicate_size*duplicateLevel points as TRUE.
  # If the last recorded points after a duplicate group is < duplicate_size,
  # those remaining points cannot have duplicates.
  #
  # Args:
  #   points: A vector of pressure values in a session, sorted by time
  #
  # Returns:
  #   A vector of booleans for each session

  duplicate_level <- get_duplicate_level(points, duplicate_size)

  # Check case where there are no duplicates
  if (duplicate_level == 0) {

    return(rep(FALSE, length(points)))
  }

  duplicate_bools <- rep(TRUE, length(points))

  # Group consists of points and their duplicates
  duplicate_group_size <- duplicate_size * (duplicate_level + 1)
  num_duplicate_groups <- floor(length(points) / duplicate_group_size)

  # Tag points that are able to have duplicates in their group
  for (group in 1:num_duplicate_groups) {

    start_idx <- (group - 1) * duplicate_group_size
    for (i in 1:duplicate_size) {

      duplicate_bools[start_idx + i] <- FALSE
    }
  }
  # Tag trailing points at end that cannot have duplicates
  idx <- num_duplicate_groups * duplicate_group_size + 1
  while (idx <= length(points)) {

    duplicate_bools[idx] <- FALSE
    idx <- idx + 1
  }

  duplicate_bools
}


get_duplicate_level <- function(points, duplicate_size) {
  # Computes duplicateLevel for points by counting
  #     how many times the first duplicate_size
  # values are repeated.
  #
  # Args:
  #   points: A vector of pressure values in a session, sorted by time
  #
  # Returns:
  #   Integer denoting number of duplicates
  #     for each set of duplicate_size points

  if (length(points) < duplicate_size * 2) {

    return(0)
  }

  duplicate_level <- 0
  first_group <- points[1:duplicate_size]
  # Start at end of first_group
  start_idx <- duplicate_size
  while (start_idx <= length(points)) {

    # Check if next group is equal to first_group
    for (i in 1:duplicate_size) {

      # Stop if group is smaller than DUPLICATE SIZE
      # or contains value not equal to first_group

      if (start_idx + i > length(points) |
        !near(points[start_idx + i], first_group[i])) {

        return(duplicate_level)
      }
    }
    # All values match first_group so increase duplicate_level
    duplicate_level <- duplicate_level + 1
    start_idx <- start_idx + duplicate_size
  }

  duplicate_level
}


fix_deduplicated_time <- function(times, session_time_step = 0.1) {
  # Fixes time steps in deduplicated data by adding 100 miliseconds
  # to session start time for each element in time vector.
  #     This function assumes that the input time vector is
  #     sorted and has the same length as the deduplicated data.
  #
  # Args:
  #   time: A vector of sorted session time steps
  #
  # Returns:
  #   A vector with fixed time steps

  # check to make sure there exists at least one timepoint

  if (length(times) < 1) {

    return(c())
  }

  start_time <- times[1]
  # Starting with empty vector, changes format of
  #     datetime when appending new values
  fixed_times <- c(start_time)
  time_idx <- 2 # Note: while loop avoids out of bound errors
  while (time_idx <= length(times)) {

    fixed_times <- c(fixed_times,
        start_time + (time_idx - 1) * session_time_step)

    time_idx <- time_idx + 1
  }

  fixed_times
}
