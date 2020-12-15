detrend_signal <- function(pressures) {
  # Detrend a signal using BEADS
  # (Baseline Estimation And Denoising with Sparsity)
  #
  # Args:
  #   pressures: A 1-dimensional vector of pressure values (i.e. no column header)
  #
  # Returns:
  #   A detrended signal
  
  # There are no input checks inside the BEADS algorithm, so here we first check that
  # input signal has a valid size, to avoid getting a cryptic error from BEADS down the line.
  
  # If there are too few points to detrend, pass back the original pressure values, unmodified:
  if (length(pressures) < 3) {
    
    return(pressures)
  }
  
  index <- 1:length(pressures)
  
  # Set various BEADS constants:
  # You can find the details of these parameters in the original BEADS paper
  # http://www.laurent-duval.eu/siva-beads-baseline-background-removal-filtering-sparsity.html
  # These parameter names and values come straight from the supplementary material (sample code)
  # with some tweaking for this data
  
  # The most important parameter to tune is the cutoff frequency of the High Pass Filter:
  fc <- 0.005  # fc : cut-off frequency (cycles/sample) = 1/200. 200 points = 20 seconds.
  # Baseline drift is in the order of 20 seconds
  
  d <- 1       # d : filter order parameter (d = 1 or 2)
  r <- 6       # r : asymmetry ratio. Positivity bias (peaks are positive). 1 for symmetric peaks
  
  # Regularization parameters:
  amp <- 0.1        # Low values if noise is low. High value will apply significant de-noising.
  # We want to apply very light de-noising, as we want detrended values to be
  # similar to original values
  lam0 <- 0.5 * amp # lambda values are manually tuned. Inversely proportional to the sparsity
  # of the signal and the signal derivative
  lam1 <- 5 * amp
  lam2 <- 4 * amp
  
  result <- Beads(pressures, d, fc, r, lam0, lam1, lam2)
  pressureDetrend <- result$x@x # x is the object containing the pressure with baseline removed
  pressureDetrend
}
