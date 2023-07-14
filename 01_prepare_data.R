# Title: Data preparation

# Purpose : This script performs the initial data processing including the
#           imputation based on the average of the 24h & 48h leading and lagged 
#           values. The data is split at the remaining gaps and only sequences 
#           longer than 7 days are kept. 

# Data file: 'data/01_high_res_data.csv'

# Author: M. Schaffer
# Contact details: msch@build.aau.dk


# Load packages -----------------------------------------------------------

library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(lubridate)


# Read high resolution data ----------------------

high_res_dt <- fread("data/01_high_res_data.csv")


# Process data ------------------------------------------------------------

# Ensure that missing values for every hour are encoded NA
time_range <- high_res_dt[!is.na(Total)][, list(start = min(Time), end = max(Time)), by = ID]
full_time <- time_range[, list(Time = seq.POSIXt(from = start, to = end, by = "hours")), by = ID]
high_res_dt <- merge.data.table(high_res_dt, full_time, all = TRUE)


# Impute missing data -----------------------------------------------------

# Impute missing data based on the mean of the 24 & 48 leading and lagged value
high_res_dt[, Total_imputed := Total]
while (sum(is.na(high_res_dt$Total_imputed) != 0)) {
  high_res_dt[, day_before := shift(Total_imputed, n = 24, type = "lag"), by = ID]
  high_res_dt[, day_after := shift(Total_imputed, n = 24, type = "lead"), by = ID]
  high_res_dt[, tday_before := shift(Total_imputed, n = 48, type = "lag"), by = ID]
  high_res_dt[, tday_after := shift(Total, n = 48, type = "lead"), by = ID]
  high_res_dt[is.na(Total_imputed), Total_imputed := rowMeans(.SD, na.rm = TRUE), .SDcols = c("day_before", "day_after", "tday_before", "tday_after"), by = ID]
}
high_res_dt[, c("day_before", "day_after", "tday_before", "tday_after") := NULL]

# Set all gaps longer than 48 to NA again
# Based on imputeTS
maxgap <- 48
rlencoding <- rle(is.na(high_res_dt$Total))
rlencoding$values[rlencoding$lengths <= maxgap] <- FALSE
en <- inverse.rle(rlencoding)
high_res_dt[en == TRUE, Total_imputed := NA]


# Plot data ---------------------------------------------------------------
high_res_dt[, day := yday(Time)]


# Select randomly three weeks with at least one 48 hour gap and select data so
# that the gap is the middle of the plotted data
ismissing_org <- high_res_dt[, rle(is.na(Total)), by = ID]
day_gaps <- which(ismissing_org$lengths == 48 & ismissing_org$value == TRUE)
set.seed(123)
dg <- high_res_dt[map_dbl(sample(day_gaps, 3), ~ sum(ismissing_org[1:.x]$lengths)), c("ID", "day")]
day_plot <- high_res_dt[dg[, list(day = seq(day - 3, day + 3)), by = ID], on = c(ID = "ID", day = "day")]
setorder(day_plot, "Time", "ID")
day_plot[, x := 1:(24 * 7), by = ID]

day_plot[, missing := is.na(Total)]
day_plot[, ID := as.numeric(as.factor(ID))]


p1 <- ggplot(day_plot, aes(x = x, y = Total_imputed, colour = missing, group = ID)) +
  geom_line() +
  facet_grid(row = vars(ID)) +
  scale_color_manual(values = c("FALSE" = "#1B7837", "TRUE" = "#762A83"), labels = c("TRUE" = "imputed", "FALSE" = "original"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 24 * 7, by = 24), name = "hours") +
  scale_y_continuous(name = "demand - Wh") +
  theme_bw() +
  theme(
    text = element_text(size = 10, colour = "black"),
    axis.text.y = element_text(size = 8, colour = "black"),
    axis.text.x = element_text(size = 8, colour = "black"),
    line = element_line(colour = "black", linewidth = 0.2),
    rect = element_rect(colour = "black", linewidth = 0.2),
    axis.ticks = element_line(colour = "black", linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.1),
    panel.grid.major = element_line(linewidth = 0.2),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
    strip.background = element_rect(fill = NA, colour = "black", linewidth = .2),
    legend.position = "bottom"
  )



ggsave(
  filename = "plots/Figure_2_extended.pdf",
  plot = p1,
  device = cairo_pdf,
  width = 140,
  height = 140 * (4 / 6),
  units = "mm"
)


# Plot only one week of data for publication plot
p2 <- ggplot(day_plot[ID == 2], aes(x = x, y = Total_imputed, colour = missing, group = ID)) +
  geom_line() +
  scale_color_manual(values = c("FALSE" = "#1B7837", "TRUE" = "#762A83"), labels = c("TRUE" = "imputed", "FALSE" = "original"), name = NULL) +
  scale_x_continuous(breaks = seq(0, 24 * 7, by = 24), name = "hours") +
  scale_y_continuous(name = "demand - Wh") +
  theme_bw() +
  theme(
    text = element_text(size = 8, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.text.x = element_text(size = 6, colour = "black"),
    line = element_line(colour = "black", linewidth = 0.2),
    rect = element_rect(colour = "black", linewidth = 0.2),
    axis.ticks = element_line(colour = "black", linewidth = 0.2),
    panel.grid.minor = element_line(linewidth = 0.05),
    panel.grid.major = element_line(linewidth = 0.1),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),
    strip.background = element_rect(fill = NA, colour = "black", linewidth = .2),
    legend.position = "top",
    legend.justification = "left",
    legend.text.align = 0,
    legend.title = element_blank(),
    legend.background = element_rect(fill = "white", color = "black", linewidth = 0.1),
    legend.position = c(0.5, 0.8),
    legend.margin = margin(unit(c(1, 1, 1, 1), "mm")),
    legend.box.margin = unit(c(0, 0, 0, 0), "mm"),
    legend.spacing.x = unit(1, "mm"),
    legend.spacing.y = unit(0, "mm")
  )


ggsave(
  filename = "plots/Figure_2.pdf",
  plot = p2,
  device = cairo_pdf,
  width = 81,
  height = 50,
  units = "mm"
)


# Save data ---------------------------------------------------------------

# Split the data into groups without missing data under the condition that every
# sequence is at least 7 full days long
ismissing <- high_res_dt[, rle(is.na(Total_imputed)), by = ID]
high_res_dt[, group := rep(1:nrow(ismissing), ismissing$lengths)]
high_res_dt[, missing := is.na(Total_imputed)]
high_res_dt[, fullday := .N == 24, by = list(group, day)]
high_res_lst <- split(high_res_dt[missing == FALSE & fullday == TRUE], by = "group")
week <- map_lgl(high_res_lst, ~ (nrow(.x) / 24) >= 7)
final_data <- high_res_lst[week]
final_data <- rbindlist(final_data)
final_data[, c("day", "missing", "fullday") := NULL]
final_data[, group := as.numeric(as.factor(group))]
final_data[, Total:= Total_imputed]
final_data[, Total_imputed:= NULL]
setnames(final_data, tolower(names(final_data)))

saveRDS(final_data, "data/02_prepared_data.RDS")
