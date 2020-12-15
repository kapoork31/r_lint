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
    "act_validation_utils.R"
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

  ValidateDataSchema(raw_act_data, RawACTSchema)
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

  index <- 1:length(pressures)

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
