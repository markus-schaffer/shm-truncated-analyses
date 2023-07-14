# Title: SMPS with regression using Fourier basis functions

# Purpose : This script test the SPMS algorithm with smoothing based on
# Regression using Fourier basis functions. It test different number of basis
# functions (2 to 23) - accounting sine and cosine pairs plus the constant.
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
library(progressr)
library(fda)
library(data.table)
setDTthreads(1)

source("functions/fn_evaluation.R")
source("functions/fn_scale.R")

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

# Main smoothing function
smooth_main_fn <- function(data_dt, data_array, nbasis = 3, offset = 0.5, pg, timepoints = 24, eval_criteria) {
  # Smooth the data
  basisobj <- create.fourier.basis(rangeval = range(1:timepoints), nbasis = nbasis)
  ys <- smooth.basis(argvals = 1:timepoints, y = data_array, fdParobj = basisobj)
  data_dt[, smd := as.vector(eval.fd(1:timepoints, ys$fd))]

  # Split data up per day for further prepossessing 
  data_lst <- split(data_dt, by = c("group", "day"))

  # Scale the smoothed results to obey the point wise offset and
  # the cumulative trend per day
  smd_result <-
    future_map2_dfr(data_lst, ys$gcv, function(days, gcv) {
      map_dfr(offset, function(offset) {
        cbind(scale_fn(main = days, offset = offset, eval_criteria = eval_criteria), data.table(gcv = gcv))
      })
    })

  pg()
  return(
    cbind(
      smd_result,
      data.table(nbasis = nbasis)
    )
  )
}

# Reshape the rounded down demand data to a matrix
timepoints <- 24
demand_array <- matrix(high_res_dt$demand_trunc, nrow = timepoints)

# Parameters and steps for progress bar
offset <- seq(0.1, 1, 0.1)
bases <- seq(3, (timepoints - 1), 2)

# Nested parallel processing with the outer loop iterating through the different
# number of basis functions and 
# the inner loop iterating through the days (inside the function)

plan(list(
  tweak(multisession, workers = availableCores() %/% 4),
  tweak(multisession, workers = 4)
))
with_progress({
  pg <- progressor(steps = length(bases))
  result <- future_map_dfr(bases, function(n) {
    smooth_main_fn(
      data_dt = high_res_dt,
      data_array = demand_array,
      nbasis = n,
      offset = offset,
      pg = pg,
      timepoints = timepoints,
      eval_criteria = eval_criteria
    )
  })
})
plan(sequential)

saveRDS(result, "data/03a_fourier_result.RDS")
