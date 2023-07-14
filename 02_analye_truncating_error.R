# Title: Analyses of truncation error

# Purpose : This script analyses the truncating error of the rounded SHM data 
#           for both cumulative and demand values. Additional the error is 
#           analysed for the summer and the winter period separately.

# Data file: 'data/02_prepared_data.RDS'

# Author: M. Schaffer
# Contact details: msch@build.aau.dk


# Load packages -----------------------------------------------------------
library(purrr)
library(dplyr)
library(ggplot2)
library(data.table)
library(patchwork)
library(lubridate)
library(ggnewscale)

# So all plots are with English month names
Sys.setlocale("LC_TIME", "English")

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


# Calculate the truncating error for cumulative and demand values
high_res_dt[, cum_error := cum - cum_trunc, by = group]
high_res_dt[, demand_error := demand - demand_trunc, by = group]
high_res_dt[, month := month(time, label = TRUE, locale = "English_United States")]



# Plot zero demand distribution -------------------------------------------

# zero demand
high_res_dt[, demand_zero := demand == 0]
high_res_dt[, demand_trunc_null := demand_trunc == 0]

# Calculate daily and monthly means of mean number of null demands per day
demand_zero <- high_res_dt[, list(original = sum(demand_zero), truncated = sum(demand_trunc_null)), by = c("group", "day", "month")]
demand_zero_daily <- demand_zero[, map(.SD, ~ mean(.x)), by = "day", .SDcols = c("original", "truncated")]
demand_zero_daily[, time := as.POSIXct("2022-01-01") + lubridate::days(day)]
demand_zero_daily[, type := "daily"]
demand_zero_monthly <- demand_zero[, map(.SD, ~ mean(.x)), by = "month", .SDcols = c("original", "truncated")]
demand_zero_monthly[, time := as.POSIXct(paste0("2022-", as.numeric(month), "-15"))]
demand_zero_monthly[, type := "monthly"]

# Combine for plotting
zero_plot <- rbind(demand_zero_monthly[, -"month"], demand_zero_daily[, -"day"])
zero_plot <- melt(zero_plot, id.vars = c("time", "type"))
zero_plot[, variable := paste0(variable, "_", type)]


# Plot number of daily null demands as monthly and daily mean
p_zero_demand <- ggplot() +
  geom_line(data = zero_plot[variable == "original_monthly" | variable == "truncated_monthly"], aes(x = time, y = value, group = variable, color = variable), size = 0.2) +
  geom_point(data = zero_plot[variable == "original_monthly" | variable == "truncated_monthly"], aes(x = time, y = value, group = variable, color = variable), size = 0.7) +
  scale_color_manual(
    values = c("original_monthly" = "#762A83", "truncated_monthly" = "#1B7837"),
    labels = c("original_monthly" = "full resolution", "truncated_monthly" = "truncated"),
    name = "monthly mean"
  ) +
  guides(color = guide_legend(nrow = 2)) +
  new_scale_color() +
  geom_line(data = zero_plot[variable == "original_daily" | variable == "truncated_daily"], aes(x = time, y = value, group = variable, color = variable), size = 0.2) +
  scale_color_manual(
    values = c("original_daily" = "#C2A5CF", "truncated_daily" = "#ACD39E"),
    labels = c("original_daily" = "full resolution", "truncated_daily" = "truncated"),
    name = "daily mean"
  ) +
  scale_y_continuous(name = "number of hours with 0 kWh \nenergy use per day") +
  scale_x_datetime(date_breaks = "month", date_labels = "%b", name = "") +
  theme_bw() +
  theme(text = element_text(size = 8, colour = "black")) +
  theme(axis.text.y = element_text(size = 6, colour = "black")) +
  theme(axis.text.x = element_text(size = 6, colour = "black")) +
  theme(line = element_line(colour = "black", linewidth = 0.2)) +
  theme(rect = element_rect(colour = "black", linewidth = 0.2)) +
  theme(axis.ticks = element_line(colour = "black", linewidth = 0.2)) +
  theme(panel.spacing = unit(2, "mm")) +
  theme(strip.background = element_blank(), strip.text.y = element_text(angle = 0)) +
  theme(plot.margin = margin(0, 0, 0, 0, unit = "cm")) +
  theme(legend.position = "bottom", legend.justification = c(1.05, 1), legend.direction = "horizontal") +
  guides(color = guide_legend(nrow = 2)) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.margin = margin(-0.5, 0, 0, 0, unit = "cm"))


ggsave(
  filename = "plots/Figure_4.pdf",
  plot = p_zero_demand,
  device = cairo_pdf,
  width = 81,
  height = 50,
  units = "mm"
)


# Analyse the distribution of the truncating error ------------------------

# Define summer based on zero demand
demand_zero_monthly[, summer := truncated > 10]
high_res_dt <- merge.data.table(high_res_dt, demand_zero_monthly, all = TRUE, by = "month")

# Function to analyse the truncation error for the different periods
fn_dist_plot <- function(data, period, save = FALSE) {
  if (period == "summer") {
    plot_data <- data[summer == TRUE]
  } else if (period == "winter") {
    plot_data <- data[summer == FALSE]
  } else if (period == "all") {
    plot_data <- data
  }else if (period == "no_zero") {
    plot_data <- data[demand_zero == FALSE]
  }

  # Perform the Kolmogorov-Smirnov Tests for the assumption of an uniform distribution (0,1)
  ks_cum <- plot_data[, ks.test(cum_error, "punif", 0, 1, exact = TRUE)[c("statistic", "p.value")], by = month]
  # Label positions
  ks_cum[, x := 1.02]
  ks_cum[, y := 0.5]


  p_error_cum <- ggplot() +
    geom_histogram(data = plot_data, aes(x = cum_error, y = after_stat(density), group = month), fill = "grey80", colour = "grey60", size = 0.1, binwidth = 0.05, boundary  = 0) +
    facet_grid(row = vars(month)) +
    stat_function(fun = function(x) {
      dunif(x, min = 0, max = 1)
    }, n = 1e3, xlim = c(-0.1, 1.1), size = 0.15) +
    geom_text(
      data = ks_cum, aes(x = x, y = y, label = paste0("p-value = ", formatC(ks_cum$p.value, format = "e", digits = 2))),
      hjust = "left", vjust = "inward", size = 6 / .pt
    ) +
    theme_classic() +
    scale_y_continuous(breaks = c(0, 1), expand = c(0, 0)) +
    scale_x_continuous(
      breaks = seq(0, 1, length.out = 3),
      expand = c(0, 0), name = "truncating error of cummulative values - kWh"
    ) +
    coord_cartesian(xlim = c(-0.1, 1.6), ylim = c(0, 1.2)) +
    theme(text = element_text(size = 8, color = "black"),
          axis.text = element_text(size = 6, color = "black"),
          axis.line = element_line(color = "black", linewidth = 0.2),
          axis.ticks = element_line(color = "black", linewidth = 0.2),
          panel.spacing = unit(2, "mm"),
          strip.background = element_blank(),
          strip.text.y = element_blank(),
          plot.margin = margin(0, 0, 0, 0, "cm"))
  
  p_error_demand <- ggplot() +
    geom_histogram(data = plot_data, aes(x = demand_error, y = after_stat(density), group = month), fill = "grey80", colour = "grey60", size = 0.1, binwidth = 0.05, boundary  = 0) +
    facet_grid(row = vars(month)) +
    theme_classic() +
    scale_y_continuous(breaks = c(0, 2.5), expand = c(0, 0), name = "") +
    scale_x_continuous(
      breaks = seq(-1, 1, length.out = 5), expand = c(0, 0),
      name = "truncating error of energy use values - kWh"
    ) +
    coord_cartesian(xlim = c(-1.1, 1.1), ylim = c(0, 3)) +
    theme(
      text = element_text(size = 8, colour = "black"),
      axis.text.y = element_text(size = 6, colour = "black"),
      axis.text.x = element_text(size = 6, colour = "black"),
      line = element_line(colour = "black", linewidth = 0.2),
      rect = element_rect(colour = "black", linewidth = 0.2),
      axis.ticks = element_line(colour = "black", linewidth = 0.2),
      panel.spacing = unit(2, "mm"),
      strip.background = element_blank(), 
      strip.text.y = element_text(angle = 0),
      plot.margin = margin(0, 0, 0, 0, "cm")
    )

  p_error_total <- p_error_cum + p_error_demand

  if (save) {
    ggsave(
      filename = paste0("plots/02_trunc_error_", period, ".pdf"),
      plot = p_error_total,
      device = cairo_pdf,
      width = 170,
      height = 75,
      units = "mm"
    )
  } else {
    return(list("demand_error" = p_error_demand, "cum_error" = p_error_cum))
  }
}


if (!dir.exists(file.path("plots"))) {
  dir.create(file.path("plots"))
}

# Plot 02_trunc_error_all.pdf corresponds to Figure 3
# Plot 02_trunc_error_no_zero.pdf corresponds to Figure 5
walk(c("no_zero", "summer", "winter","all"), ~ fn_dist_plot(data = high_res_dt, period = .x, save = TRUE))
