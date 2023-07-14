# Title: Plot the results of the different SPMS settings

# Purpose : This script evaluates the results of the different SPMS settings.

# Data file: 'data/03a_fourier_result.RDS' & 'data/03b_wma_result.RDS' &
#            'data/02_prepared_data.RDS'
# Function files: 'functions/fn_evaluation.R'

# Author: M. Schaffer
# Contact details: msch@build.aau.dk


# Load packages -----------------------------------------------------------

library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(patchwork)
library(scales)
library(lubridate)
source("functions/fn_evaluation.R")

# Load results ------------------------------------------------------------

fourier_result <- readRDS("data/03a_fourier_result.RDS")
fourier_result[, type := "fourier"]
wma_result <- readRDS("data/03b_wma_result.RDS")

# Load processed high resolution data -------------------------------------

high_res_dt <- readRDS("data/02_prepared_data.RDS")
setnames(high_res_dt, "total", "demand")

# Process data ------------------------------------------------------------

# Calculate the rounded values and mimic thereby the process of data transmitted
# by commercial smart heat meters
high_res_dt[, day := yday(time)]
high_res_dt[, demand := demand / 1000]
high_res_dt[, cum := cumsum(demand), by = group]
high_res_dt[, cum_round := trunc(cum)]
high_res_dt[, demand_trunc := cum_round - shift(cum_round), by = group]
high_res_dt[is.na(demand_trunc), demand_trunc := cum_round]
high_res_dt[, month := month(time, label = TRUE, locale = "English_United States")]

criteria <- c(paste0(names(eval_criteria), "_actual"), paste0(names(eval_criteria), "_trunced"))

result_plot <- function(season, save = FALSE, ref_data, wma_result, fourier_result) {
  if (season == "summer") {
    months <- c("May", "Jun", "Jul", "Aug", "Sep")
    days <- seq(122, 243)
  } else if (season == "winter") {
    months <- c("Mar", "Apr", "Oct", "Nov", "Dec")
    days <- c(seq(1, 121), seq(244, 365))
  } else {
    months <- c("Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    days <- seq(1, 365)
  }

  # Select the data according to the season
  ref_data <- ref_data[month %in% months]
  fourier_result <- fourier_result[day %in% days]
  wma_result <- wma_result[day %in% days]

  # Calculate the reference line, i.e. the rounded down data vs the actual data
  reference <- ref_data[, map(eval_criteria, ~ .x(actual = demand, predicted = demand_trunc)), by = list(group, day)]
  reference <- reference[, map(.SD, ~ mean(.x, na.rm = TRUE)), .SDcols = names(eval_criteria)]

  # Average the data across
  fourier_result_offset <- fourier_result[, map(.SD, ~ mean(.x, na.rm = TRUE)), by = list(offset, nbasis, type), .SDcols = c(criteria, "gcv")]
  wma_result_offset <- wma_result[, map(.SD, ~ mean(.x, na.rm = TRUE)), by = list(offset, type, window, align), .SDcols = criteria]
  combined_result_offset <- rbind(fourier_result_offset, wma_result_offset, fill = TRUE)

  # needed for easier plotting
  combined_result_offset[, offset_plot := paste0("±", offset)]
  combined_result_offset[, offset_plot := as.factor(offset_plot)]
  combined_result_offset[, type := factor(type, levels = c("fourier", "simple", "linear", "exponential"))]

  # Evaluate the different offsets for NRMSE
  plot_off_nrmse <- ggplot(
    combined_result_offset,
    aes(x = offset_plot, y = nrmse_actual, fill = type)
  ) +
    geom_boxplot(
      outlier.shape = 21, outlier.size = 0.5, outlier.stroke = 0.05,
      linetype = "solid", size = 0.05
    ) +
    theme_bw() +
    scale_fill_manual(
      values = c("#CC6677", "#88CCEE", "#44AA99", "#DDCC77"),
      labels = c(
        "fourier" = "RFB", "simple" = "MA simple weighting",
        "linear" = "MA linear weighting", "exponential" = " MA exp. weighting"
      ),
      name = ""
    ) +
    labs(x = "maximum allowed pointwise deviation - kWh", y = "NRMSE") +
    theme(
      text = element_text(size = 8, color = "black"),
      axis.text = element_text(size = 6, color = "black"),
      legend.title = element_text(size = 6, color = "black"),
      line = element_line(color = "black", linewidth = 0.1),
      rect = element_rect(color = "black", linewidth = 0.1),
      axis.ticks = element_line(color = "black", linewidth = 0.1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.1),
      legend.position = "none"
    )

  # Evaluate the different offsets for PCC
  plot_off_corr <- ggplot(
    combined_result_offset,
    aes(x = offset_plot, y = corr_actual, fill = type)
  ) +
    geom_boxplot(
      outlier.shape = 21, outlier.size = 0.5, outlier.stroke = 0.05,
      linetype = "solid", size = 0.05
    ) +
    theme_bw() +
    scale_fill_manual(
      values = c("#CC6677", "#88CCEE", "#44AA99", "#DDCC77"),
      labels = c(
        "fourier" = "RFB", "simple" = "MA simple weighting",
        "linear" = "MA linear weighting", "exponential" = " MA exp. weighting"
      ),
      name = ""
    ) +
    labs(x = "maximum allowed pointwise deviation - kWh", y = "PCC") +
    theme(
      text = element_text(size = 8, color = "black"),
      axis.text = element_text(size = 6, color = "black"),
      legend.title = element_text(size = 6, color = "black"),
      line = element_line(color = "black", linewidth = 0.1),
      rect = element_rect(color = "black", linewidth = 0.1),
      axis.ticks = element_line(color = "black", linewidth = 0.1),
      legend.text.align = 0,
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.1),
      legend.position = "bottom"
    )

  # Combine plots
  plot_off_final <- plot_off_nrmse + plot_off_corr +
    plot_layout(guides = "collect") & theme(
    legend.position = "bottom",
    legend.box.margin = unit(c(0, 0, 0, 0.2), "cm")
  )


  if (save) {
    ggsave(
      filename = paste0("plots/04_offset_plot_", season, ".pdf"),
      plot = plot_off_final,
      device = cairo_pdf,
      width = 170,
      height = 170 * (4 / 6) / 2,
      units = "mm"
    )
  }

  # Analyse the results in detail for SPMS with weighted moving average
  # smoothing with linear weighting and an offset of ± 0.4
  detailed_result <- combined_result_offset[offset == 0.4 & type == "linear"]

  # Plot NRMSE for different window sizes and alignments
  plot_detailed_nrmse <- ggplot(
    detailed_result,
    aes(x = window, y = nrmse_actual, group = align, color = align)
  ) +
    geom_line(size = 0.5) +
    geom_point(size = 0.75) +
    scale_color_manual(
      values = c("left" = "#CC6677", "center" = "#88CCEE", "right" = "#44AA99"),
      name = "window\nalignment"
    ) +
    geom_hline(yintercept = reference$nrmse, size = 0.25, linetype = 5) +
    annotate(
      geom = "text",
      x = 3,
      y = reference$nrmse,
      label = paste0("truncated data = ", round(reference$nrmse, 3)),
      hjust = 0,
      vjust = 1.5,
      size = 6 / .pt
    ) +
    theme_bw() +
    theme(plot.margin = unit(c(5.5, 5.5, 15.5, 5.5), "pt")) +
    labs(y = "NRMSE", x = "") +
    scale_x_continuous(minor_breaks = NULL, name = NULL, labels = NULL) +
    theme(
      legend.position = "bottom",
      text = element_text(size = 8, color = "black"),
      axis.text = element_text(size = 6, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      line = element_line(color = "black", linewidth = 0.1),
      rect = element_rect(color = "black", linewidth = 0.1),
      axis.ticks = element_line(color = "black", linewidth = 0.1),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.1),
      plot.margin = unit(c(0, 0.0, 0.2, 0), "cm")
    )

  # Plot PCC for different window sizes and alignments
  plot_detailed_corr <- ggplot(
    detailed_result,
    aes(x = window, y = corr_actual, group = align, color = align)
  ) +
    geom_line(size = 0.5) +
    geom_point(size = 0.75) +
    scale_color_manual(
      values = c("left" = "#CC6677", "center" = "#88CCEE", "right" = "#44AA99"),
      name = "window\nalignment"
    ) +
    geom_hline(yintercept = reference$corr, size = 0.25, linetype = 5) +
    annotate(
      geom = "text",
      x = 3,
      y = reference$corr,
      label = paste0("truncated data = ", round(reference$corr, 3)),
      hjust = 0,
      vjust = -1.0,
      size = 6 / .pt
    ) +
    theme_bw() +
    labs(y = "PCC") +
    scale_x_continuous(minor_breaks = NULL, name = "window size") +
    theme(
      legend.position = "bottom",
      text = element_text(size = 8, color = "black"),
      axis.text = element_text(size = 6, color = "black"),
      legend.title = element_text(size = 8, color = "black"),
      line = element_line(color = "black", linewidth = 0.1),
      rect = element_rect(color = "black", linewidth = 0.1),
      axis.ticks = element_line(color = "black", linewidth = 0.1),
      legend.text.align = 0,
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.1),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.1)
    )


  # Combine plots
  plot_detailed_final <- plot_detailed_nrmse / plot_detailed_corr +
    plot_layout(guides = "collect") & theme(
    legend.position = "bottom",
    legend.text.align = 0,
    legend.box.margin = unit(c(0, 0, 0, 0.0), "cm")
  )

  if (save) {
    ggsave(
      filename = paste0("plots/04_detailed_plot_linear_", season, ".pdf"),
      plot = plot_detailed_final,
      device = cairo_pdf,
      width = 81,
      height = 120 * (4 / 6),
      units = "mm"
    )
  } else {
    return(list("general" = plot_off_final, "detailed" = plot_detailed_final))
  }
}

if (!dir.exists(file.path("plots"))) {
  dir.create(file.path("plots"))
}


# Plot 04_offset_plot_all.pdf corresponds to Figure 6
# Plot 04_detailed_plot_linear_all.pdf corresponds to Figure 7
walk(c("summer", "winter", "all"), ~ result_plot(
  season = .x, save = TRUE, ref_data = high_res_dt,
  wma_result = wma_result,
  fourier_result = fourier_result
))
