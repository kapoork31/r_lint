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
# NB: some functions have been renamed to conform to our linting standards, but are
#     unchanged functionality-wise
#
###################################################

if (!require("functional")) install.packages("functional")
if (!require("Matrix")) install.packages("Matrix")
library(functional)
library(Matrix)
options(digits.secs=3)

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

CleanRawACTData <- function(raw_act_data) {
  # ==== ENTRYPOINT FUNCTION ====
  # When provided raw ACT data, this function changes column names to be standardized format, drops
  #   NAs, makes the calls to detrend the recorded signal for expired breaths (on a session level).
  #
  # Args:
  #   raw_act_data: Tibble of ACT data (see RawACTSchema)
  #
  # Returns:
  #   cleaned ACT data
  #     patientId <chr> | sessionId <chr> | time <dttm> | pressureDetrend <dbl>
  
  ValidateDataSchema(raw_act_data, RawACTSchema)
  # Need to modify digits for seconds in order for duplication to work
  curr_digits <- getOption("digits.secs")
  options(digits.secs = 3)
  
  raw_act_data <- raw_act_data %>%
    rename(
      "sessionId" = "id",
      "pressureValues" = "pressurevalues",
      "patientId" = "patient_record_id"
    ) %>%
    mutate(
      sessionId = as.character(sessionId),
      pressureValues = as.numeric(pressureValues),
      time = lubridate::ymd_hms(time),
      patientId = as.character(patientId)
    ) %>%
    drop_na %>%
    group_by(sessionId) %>%
    arrange(time) %>%
    mutate(duplicate = tag_duplicate_points(pressureValues)) %>%
    filter(duplicate == FALSE) %>%
    mutate(time = fix_deduplicated_time(time)) %>%
    mutate(pressureDetrend = DetrendSignal(pressureValues)) %>%
    filter(pressureDetrend >= -10 & pressureDetrend <= 120) %>%
    select(patientId, sessionId, time, pressureDetrend, -duplicate, -pressureValues) %>%
    ungroup()
  
  # Reset options
  #options(digits.secs = currDigits)
  
  raw_act_data
}
