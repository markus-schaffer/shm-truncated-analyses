# Title: SMPS with smoothing using a moving weighted average

# Purpose : This script test the SPMS algorithm with smoothing based on
# a moving weighted average. Testing different weightings (non, linear, exponential).
# Further it tests different maximum allowed pointwise deviations (0.1, 0.2,
# ..., 1.0)

# Data file: 'data/02_prepared_data.RDS'
# Function files: 'functions/fn_evaluation.R' & 'functions/fn_scale.R'

# Author: M. Schaffer
# Contact details: msch@build.aau.dk


# Load packages -----------------------------------------------------------
library(purrr)
library(dplyr)
library(furrr)
library(tidyr)
library(zoo)
library(progressr)
library(data.table)

setDTthreads(1)

source("fn_evaluation.R")
source("fn_scale.R")



# Load processed high resolution data -------------------------------------

high_res_dt <- readRDS("data/02_prepared_data.RDS")
setnames(high_res_dt, "total", "demand")


# Process data ------------------------------------------------------------

# Calculate the truncated values and mimic thereby the process of data transmitted
# by commercial smart heat meters
high_res_dt[, day := yday(time)]
high_res_dt[, demand := demand / 1000]
high_res_dt[, cum := cumsum(demand), by = group]
high_res_dt[, cum_trunc := trunc(cum)]
high_res_dt[, demand_trunc := cum_trunc - shift(cum_trunc), by = group]
high_res_dt[is.na(demand_trunc), demand_trunc := cum_trunc]


# Smoothing --------------------------------------------------------------

# right = The current point is the rightmost end of the window
# center = The current point is the center of the window
# left = The current point is the leftmost point of the window.

# Pad vector with NA - needed to apply the moving avg with "adaptive" window size
pad_fn <- function(vec = c(1, 1, 1, 1), window = 3, align = "left") {
  if (align == "left") {
    c(vec, rep(NA, window - 1))
  } else if (align == "right") {
    c(rep(NA, window - 1), vec)
  } else if (align == "center") {
    if ((window %% 2) == 0) { # even
      c(rep(NA, (window / 2) - 1), vec, rep(NA, (window / 2)))
    } else { # odd
      c(rep(NA, (window / 2)), vec, rep(NA, (window / 2)))
    }
  } else {
    stop("'align' must be one of the following: 'left', 'center', 'right'.")
  }
}

# Define the different weights, depending on type, align and window size
weights_fn <- function(window = 5, type = "simple", align = "left") {
  if (type == "simple") {
    rep(1, window)
  } else if (type == "linear") {
    if (align == "left") {
      c(window:1)
    } else if (align == "center" & (window %% 2) == 0) {
      c(1:(window / 2), (window / 2):1)
    } else if (align == "center" & (window %% 2) != 0) {
      c(1:ceiling(window / 2), (ceiling(window / 2) - 1):1)
    } else if (align == "right") {
      c(1:window)
    }
  } else if (type == "exponential") {
    if (align == "left") {
      c(2 / (1:window + 1))
    } else if (align == "center" & (window %% 2) == 0) {
      c(2 / ((window / 2):1 + 1), 2 / (1:(window / 2) + 1))
    } else if (align == "center" & (window %% 2) != 0) {
      c(2 / (ceiling(window / 2):1 + 1), 2 / ((2:ceiling(window / 2)) + 1))
    } else if (align == "right") {
      c(2 / (window:1 + 1))
    }
  } else {
    stop("No propper weighting arguments found. Check 'window','type', and 'align' inputs.")
  }
}

# Main smoothing function
smooth_main_fn <- function(data, window = 3, align = "left", type = "simple", offset = 0.5, pg, pad_fn, weights_fn, scale_fn, eval_criteria) {
  # Smooth the data
  data[, smd := rollapplyr(
    pad_fn(vec = demand_trunc, window = window, align = align),
    width = window,
    weighted.mean,
    na.rm = TRUE,
    w = weights_fn(window = window, type = type, align = align),
    partial = FALSE,
    align = align
  ), by = group]

  # Split data up per day for further prepossessing
  data_lst <- split(data, by = c("group", "day"))

  # Scale the smoothed results to obey the point wise offset and
  # the cumulative trend per day
  smd_result <-
    future_map_dfr(offset, function(offset) {
      map_dfr(data_lst, function(day) {
        scale_fn(main = day, offset = offset, eval_criteria = eval_criteria)
      })
    })

  pg()
  return(
    cbind(
      smd_result,
      data.table(align = align, window = window, type = type)
    )
  )
}

# Parameters and steps for progress bar
param <- cross_df(list(align = c("left", "right", "center"), window = 2:6, type = c("simple", "linear", "exponential")))
offset <- seq(0.1, 1, 0.1)

# Nested parallel processing with the outer loop iterating through the different
# parameters for the smoothing and the scaling
# the inner loop iterating through the days (inside the function)

plan(list(
  tweak(multisession, workers = availableCores() %/% 4),
  tweak(multisession, workers = 4)
))
with_progress({
  pg <- progressor(steps = nrow(param))
  result <-
    future_pmap_dfr(param, function(align, window, type) {
      smooth_main_fn(
        data = high_res_dt,
        align = align,
        window = window,
        type = type,
        offset = offset,
        pg = pg,
        pad_fn = pad_fn,
        weights_fn = weights_fn,
        scale_fn = scale_fn,
        eval_criteria = eval_criteria
      )
    }, .options = furrr_options(scheduling = 1))
})
plan(sequential)


saveRDS(result, "data/03b_wma_result.RDS")
