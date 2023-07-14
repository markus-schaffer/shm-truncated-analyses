# Title: Analyse the results of the chosen SPMS settings

# Purpose: This script evaluates the data from SPMS using a moving average with linear
# weighting and a centred window with a width of 5.

# Data file: 'data/05_restored_data.RDS'

# Author: M. Schaffer
# Contact details: msch@build.aau.dk


# Load packages -----------------------------------------------------------

library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(patchwork)
library(stringr)
library(lubridate)
library(mgsub)



# Load data ---------------------------------------------------------------

data_dt <- readRDS("data/05_restored_data.RDS")
data_dt[, group_day := paste0(group, "_", day)]



# Plot example days  - Figure 8 -----------------------------------------------

selection <- c("9_268", "62_104")
plot_data <- data_dt[group_day %in% selection]
plot_data <- plot_data[, c("group_day", "time", "demand_trunc", "demand", "smd_off")]
plot_data <- melt(plot_data, id.vars = c("group_day", "time"), variable.factor = F)
plot_data[, group_day := as.numeric(factor(group_day, levels = c("9_268", "62_104")))]
plot_data[, variable := mgsub(variable,
  pattern = c("demand_trunc", "demand", "smd_off"),
  replacement = c("truncated", "original", "SPMS")
)]

day1_plot <- ggplot(plot_data[group_day == 1], aes(x = hour(time), y = value, group = variable, colour = variable, size = variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values = c("original" = "black", "truncated" = "#9970AB", "SPMS" = "#5AAE61"), name = "energy use") +
  scale_size_manual(values = c("original" = 0.3, "truncated" = 0.3, "SPMS" = 0.3), name = "energy use") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 23), name = "", labels = NULL) +
  labs(x = NULL, y = "demand - kWh") +
  theme(
    text = element_text(size = 8, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.text.x = element_text(size = 6, colour = "black"),
    line = element_line(colour = "black", size = 0.1),
    rect = element_rect(colour = "black", size = 0.1),
    axis.ticks = element_line(colour = "black", size = 0.1),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.1),
    panel.border = element_rect(color = "black", fill = NA, size = 0.1),
    strip.background = element_rect(fill = "white"),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )


day2_plot <- ggplot(plot_data[group_day == 2], aes(x = hour(time), y = value, group = variable, colour = variable, size = variable)) +
  geom_line() +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values = c("original" = "black", "truncated" = "#9970AB", "SPMS" = "#5AAE61"), name = "energy use") +
  scale_size_manual(values = c("original" = 0.3, "truncated" = 0.3, "SPMS" = 0.3), name = "energy use") +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 23), name = "hour of the day") +
  labs(x = NULL, y = "demand - kWh") +
  theme(
    text = element_text(size = 8, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.text.x = element_text(size = 6, colour = "black"),
    line = element_line(colour = "black", size = 0.1),
    rect = element_rect(colour = "black", size = 0.1),
    axis.ticks = element_line(colour = "black", size = 0.1),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.1),
    panel.border = element_rect(color = "black", fill = NA, size = 0.1),
    strip.background = element_rect(fill = "white"),
    plot.margin = unit(c(0, 0, 0, 0), "cm")
  )

plot_data_final <- day1_plot / day2_plot +
  plot_layout(guides = "collect") & theme(
  legend.position = "bottom",
  legend.text.align = 0,
  legend.box.margin = unit(c(-0.3, 0, 0, 0.0), "cm")
)


if (!dir.exists(file.path("plots"))) {
  dir.create(file.path("plots"))
}

ggsave(
  filename = "plots/Figure_8.pdf",
  plot = plot_data_final,
  device = cairo_pdf,
  width = 81,
  height = 140 * 4 / 6,
  units = "mm"
)


# Plot development of evaluation criteria  - Figure 9 --------------------------

nrmse <- function(actual, predicted) {
  if (all(is.na(actual)) | sum(actual, na.rm = TRUE) == 0) {
    return(NA_real_)
  } else {
    b <- ((is.na(actual) + is.na(predicted)) == 0)
    
    ((predicted[b] - actual[b])^2) |>
      mean() |>
      sqrt() / mean(actual[b])
  }
}


corr_data <- data_dt[, list(
  org = cor(demand, demand_trunc),
  new = cor(demand, smd_off)
),
by = group_day
]

nrmse_data <- data_dt[, list(
  org = nrmse(demand, demand_trunc),
  new = nrmse(demand, smd_off)
),
by = group_day
]


corr_data[, neg := org > new]
nrmse_data[, neg := org > new]

corr_data <- na.omit(corr_data)
nrmse_data <- na.omit(nrmse_data)


corr_plot <- ggplot(corr_data, aes(x = org, y = new, color = neg)) +
  geom_point(alpha = 0.05, size = 0.75) +
  annotate("text", label = paste0(round((sum(corr_data$neg) / nrow(corr_data)) * 100), "%"), x = 0.8, y = 0.2, size = 6 / .pt, colour = "#5AAE61") +
  annotate("text", label = paste0("improved\n", 100 - round((sum(corr_data$neg) / nrow(corr_data)) * 100), "%"), x = 0.2, y = 0.8, size = 6 / .pt, colour = "#762A83") +
  geom_abline(intercept = 0, slope = 1, size = 0.2) +
  scale_color_manual(values = c("FALSE" = "#762A83", "TRUE" = "#5AAE61"), labels = c("FALSE" = "increased", "TRUE" = "decreased")) +
  labs(x = "PCC truncated", y = "PCC recovered") +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme_bw() +
  theme(
    legend.position = "right",
    legend.title = element_blank(),
    aspect.ratio = 1,
    text = element_text(size = 8, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.text.x = element_text(size = 6, colour = "black"),
    line = element_line(colour = "black", size = 0.1),
    rect = element_rect(colour = "black", size = 0.1),
    axis.ticks = element_line(colour = "black", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.1)
  )

nrmse_plot <- ggplot(nrmse_data, aes(x = org, y = new, color = neg)) +
  geom_point(alpha = 0.05, size = 0.75) +
  annotate("text", label = paste0("improved\n", round((sum(nrmse_data$neg) / nrow(nrmse_data)) * 100), "%"), x = 30 * 0.8, y = 30 * 0.2, size = 6 / .pt, colour = "#5AAE61") +
  annotate("text", label = paste0(100 - round((sum(nrmse_data$neg) / nrow(nrmse_data)) * 100), "%"), x = 30 * 0.2, y = 30 * 0.8, size = 6 / .pt, colour = "#762A83") +
  geom_abline(intercept = 0, slope = 1, size = 0.2) +
  scale_color_manual(values = c("FALSE" = "#762A83", "TRUE" = "#5AAE61"), labels = c("FALSE" = "increased", "TRUE" = "decreased")) +
  labs(x = "NRMSE truncated", y = "NRMSE recovered") +
  coord_fixed(xlim = c(0, 30), ylim = c(0, 30)) +
  guides(color = guide_legend(override.aes = list(size = 3, alpha = 1))) +
  theme_bw() +
  theme(
    legend.position = "none",
    legend.title = element_blank(),
    aspect.ratio = 1,
    text = element_text(size = 8, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    axis.text.x = element_text(size = 6, colour = "black"),
    line = element_line(colour = "black", size = 0.1),
    rect = element_rect(colour = "black", size = 0.1),
    axis.ticks = element_line(colour = "black", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(size = 0.1)
  )

final_comp_plot <- nrmse_plot + corr_plot +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

ggsave(
  filename = "plots/Figure_9.pdf",
  plot = final_comp_plot,
  device = cairo_pdf,
  width = 170,
  height = 81,
  units = "mm"
)
