# Title: Scaling function

# Purpose :
# 1 - it ensures that all values are within the set tolerance, i.e. no values
# deviates more than +- the offset.
# 2 - it enforces that per supplied segment (e.g. day) the smoothed and original
# data accumulates to the same amount
# 3 - Evaluate the result based on the supplied evaluation criteria

# Author: M. Schaffer
# Contact details: msch@build.aau.dk


scale_fn <- function(main, offset, eval_criteria) {

  # smd_off id the smoothed results for processing
  main[, smd_off := smd]
  main[smd_off < 0, smd_off := 0]

  # Evaluate which values deviate by more then the offset
  # Set these values to maximum deviate by the offset
  main[, too_small := smd_off < demand_trunc - (offset - 1e-6)]
  main[, too_large := smd_off > demand_trunc + (offset - 1e-6)]
  main[too_large == TRUE, smd_off := demand_trunc + (offset - 1e-6)]
  main[too_small == TRUE, smd_off := demand_trunc - (offset - 1e-6)]

  # positive = smd_off too small
  dif <- main[, sum(demand_trunc) - sum(smd_off)]

  # Move all smoothed data in needed direction till difference is nearly zero
  # (for stability not zero). Uses defined offset as maximum allowed point wise
  # deviation
  i <- 1
  while (abs(dif) > 1e-6) {
    if (dif > 0) {
      main[too_large == FALSE, smd_off := smd_off + dif / nrow(main[too_large == FALSE])]
      main[, too_large := smd_off >= demand_trunc + (offset - 1e-6)]
      main[too_large == TRUE, smd_off := demand_trunc + (offset - 1e-6)]
      dif <- main[, sum(demand_trunc) - sum(smd_off)]
    } else if (dif < 0) {
      main[too_small == FALSE, smd_off := smd_off + dif / nrow(main[too_small == FALSE])]
      main[smd_off < 0, smd_off := 0]
      main[, too_small := smd_off <= demand_trunc - (offset - 1e-6)]
      main[too_small == TRUE, smd_off := demand_trunc - (offset - 1e-6)]
      dif <- main[, sum(demand_trunc) - sum(smd_off)]
    }
    i <- i + 1
    if (i > 1000) {
      # If smoothed values do not converge - indicate using an error code
      main[, smd_off := NA_real_]
      break
    }
  }

  # Evaluate the resulting data against the high resolution data
  # Using the defined evaluation criteria
  eval_real <- main[, map(eval_criteria, ~ .x(actual = demand, predicted = smd_off))]
  setnames(eval_real, paste0(names(eval_real), "_actual"))

  # Evaluate the resulting data against the low resolution rounded data
  # Using the defined evaluation criteria
  eval_trunc <- main[, map(eval_criteria, ~ .x(actual = demand_trunc, predicted = smd_off))]
  setnames(eval_trunc, paste0(names(eval_trunc), "_trunced"))

  return(cbind(eval_real, eval_trunc, data.table(offset = offset, day = main[, unique(day)])))
}
