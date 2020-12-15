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

  index <- seq_len(length(pressures))

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

  sqrt(abs(x)^2 + eps1)
}

w_fun_v1 <- function(x, eps1) {

  1 / (sqrt(abs(x)^2 + eps1))
}

phi_v1 <- function(x, eps1) {

  abs(x) - eps1 * log(abs(x) + eps1)
}

w_fun_v2 <- function(x, eps1) {

  1 / (abs(x) + eps1)
}

theta <- function(x, eps0, r) {

  sum(x[which(x > eps0)]) - (r * sum(x[which(x < -eps0)])) +
    sum((1 + r) / (4 * eps0) * x[which(abs(x) <= eps0)]^2 +
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
  pen <- "L1_v2"
  # pen : penalty function for sparse derivative ('L1_v1' or 'L1_v2')
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
         {
           cat("penalty must be L1_v1, L1_v2")
           x <- c()
           cost <- c()
           f <- c()
           return()
         })

  y <- as.vector(y)
  x <- y
  cost <- matrix(0, nrow = 1, ncol = nit)

  len_y <- length(y)
  ba_filt_res <- BAfilt(d, fc, len_y)
  a <- ba_filt_res$A
  cap_b <- ba_filt_res$B

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
    lambda <- bandSparse((2 * len_y) - 3, k = 0, diagonals = diag)
    k <- which(abs(x) > eps0)
    gamma[!k] <- ((1 + r) / 4) / abs(eps0)
    gamma[k] <- ((1 + r) / 4) / abs(x[k])
    diag_g <- vector("list", 1)
    diag_g[[1]] <- gamma
    gamma <- bandSparse(len_y, k = 0, diagonals = diag_g)

    m <- (2 * lam0 * gamma) + (t(d_bind) %*% lambda %*% d_bind)
    x <- a %*% (solve(btb + t(a) %*% M %*% a, d))

    cost[i] <- 0.5 * sum(abs(h(y - x, a, cap_b))^2) + lam0 * theta(x, eps0, r) +
      lam1 * sum(phi(diff(x))) + lam2 * sum(phi(diff(x, differences = 2)))
  }

  f <- y - x - h(y - x, a, cap_b)

  list(x = x, f = f, cost = cost)
}
