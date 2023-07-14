# Title: SMPS with smoothing using a moving weighted average

# Purpose : This script performs SPMS using a moving average with linear
# weighting and a centred window with a width of 5.

# Data file: 'data/02_prepared_data.RDS'
# Function files: 'functions/fn_scale_return_data.R'

# Author: M. Schaffer
# Contact details: msch@build.aau.dk



# Load packages -----------------------------------------------------------
library(purrr)
library(dplyr)
library(furrr)
library(tidyr)
library(zoo)
library(data.table)
setDTthreads(1)
source("functions/fn_scale_return_data.R")

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
smooth_main_fn <- function(data, window = 3, align = "left", type = "simple", offset = 0.5, pad_fn, weights_fn, scale_fn) {
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

  data_lst <- split(data, by = c("group", "day"))

  # Scale the smoothed results to obey the point wise offset and
  # the cumulative trend per day
  plan(multisession)
  smd_result <-
    future_map_dfr(data_lst, function(day) {
      scale_fn(main = day, offset = offset)
    })
  plan(sequential)

  return(
    cbind(
      smd_result,
      data.table(align = align, window = window, type = type)
    )
  )
}


# Parameters and steps for progress bar
param <- cross_df(list(align = c("center"), window = 5, type = c("linear")))
offset <- 0.4

result <- smooth_main_fn(
  data = high_res_dt,
  align = param$align,
  window = param$window,
  type = param$type,
  offset = offset,
  pad_fn = pad_fn,
  weights_fn = weights_fn,
  scale_fn = scale_fn
)

saveRDS(result, "data/05_restored_data.RDS")
