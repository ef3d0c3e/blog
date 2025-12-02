
#!/usr/bin/env Rscript

# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)
library(patchwork)  # for aligned stacking

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript script.R <file.csv> <name>")
}

file_path <- args[1]
plot_name <- args[2]

# Function to parse size suffixes (vectorized)
parse_size <- function(size_str) {
  result <- numeric(length(size_str))
  for (i in seq_along(size_str)) {
    s <- size_str[i]
    if (grepl("M$", s)) {
      result[i] <- as.numeric(sub("M$", "", s)) * 2^20
    } else if (grepl("K$", s)) {
      result[i] <- as.numeric(sub("K$", "", s)) * 2^10
    } else {
      result[i] <- as.numeric(s)
    }
  }
  return(result)
}

# Read the CSV file
data <- read.csv(file_path, header = TRUE, stringsAsFactors = FALSE, check.names = FALSE)

# Parse column names to get byte sizes
col_names <- colnames(data)
byte_sizes <- parse_size(col_names)

# Prepare data for plotting
data_long <- data %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(cols = -row_id, names_to = "size_name", values_to = "time") %>%
  filter(!is.na(time) & time > 0) %>%
  mutate(
    byte_size = parse_size(size_name),
    log2_time = log2(time)
  ) %>%
  select(-row_id)

if (nrow(data_long) == 0) {
  stop("No valid data points found. Check your CSV file format.")
}

data_long <- data_long %>%
  mutate(size_name = factor(size_name, levels = col_names))

# Calculate statistics for violin/median points
stats_data <- data_long %>%
  group_by(size_name, byte_size) %>%
  summarise(
    min = min(time),
    q1 = quantile(time, 0.25),
    median = median(time),
    mean = mean(time),
    q3 = quantile(time, 0.75),
    max = max(time),
    n = n(),
    .groups = "drop"
  )

# Compute regression lines in plotting coordinates (convert y to log2 for plotting)
reg_line <- function(lm_model, x_range) {
  x_vals <- seq(x_range[1], x_range[2], length.out = 50)
  y_vals <- lm_model$coefficients[1] + lm_model$coefficients[2] * x_vals
  data.frame(byte_size = x_vals, log2_time = log2(y_vals))
}

# Color palette
n_sizes <- length(unique(data_long$size_name))
primary5 <- c(
  "#1f77b4",  # Blue
  "#2ca02c",  # Green
  "#d62728",  # Red
  "#9467bd",  # Purple
  "#ff7f0e"   # Orange
)
colors <- rep(primary5, length.out = n_sizes)
names(colors) <- levels(data_long$size_name)

# Shared x-axis limits and breaks
x_min <- min(data_long$log2_time, na.rm = TRUE)
x_max <- max(data_long$log2_time, na.rm = TRUE)
x_range <- x_max - x_min
pad <- ifelse(x_range == 0, 0.5, 0.03 * x_range)
x_limits <- c(x_min - pad, x_max + pad)
x_breaks <- pretty(x_limits, n = 8)

# Main plot (violin + median points + piecewise regression)
p1 <- ggplot(data_long, aes(x = log2_time, y = byte_size)) +
  geom_violin(
    aes(group = size_name, fill = size_name),
    alpha = 0.5, color = NA, scale = "width", trim = TRUE
  ) +
  geom_point(
    data = stats_data,
    aes(x = log2(q1 + (q3 - q1)/2), y = byte_size, color = size_name),
    shape = 21, fill = "white", size = 2, stroke = 0.5
  ) +
  scale_fill_manual(values = colors, name = "Size") +
  scale_color_manual(values = colors, name = "Size") +
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  scale_x_continuous(
    name = "Time (μs)",
    limits = x_limits,
    breaks = x_breaks,
    labels = function(x) round(2^x, 1),
    expand = c(0, 0)
  ) +
  scale_y_log10(
    name = "Byte Size",
    labels = comma,
    breaks = 2^(0:20)
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 9),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(
    title = paste("Benchmark:", plot_name),
  )

# Histogram by relative frequency
bin_data <- data_long %>%
  group_by(size_name) %>%
  summarise(
    avg_log2_time = mean(log2_time),
    bin_width = 0.05,
    .groups = "drop"
  )

hist_list <- lapply(split(data_long, data_long$size_name), function(df) {
  size <- unique(df$size_name)
  bin_width <- bin_data$bin_width[bin_data$size_name == size]
  if (is.na(bin_width) || bin_width <= 0) {
    bin_width <- (max(df$log2_time) - min(df$log2_time)) / 30
    if (bin_width == 0) bin_width <- 0.5
  }
  min_val <- min(df$log2_time)
  max_val <- max(df$log2_time)
  start_break <- floor(min_val / bin_width) * bin_width
  n_bins <- ceiling((max_val - start_break) / bin_width) + 1
  breaks <- start_break + (0:n_bins) * bin_width
  if (max(breaks) < max_val) breaks <- c(breaks, max(breaks) + bin_width)
  if (min(breaks) > min_val) breaks <- c(min(breaks) - bin_width, breaks)
  hist_data <- hist(df$log2_time, breaks = breaks, plot = FALSE)
  counts_vec <- hist_data$counts
  total_counts <- sum(counts_vec)
  freq_vec <- if (total_counts > 0) counts_vec / total_counts else counts_vec
  non_zero <- counts_vec > 0
  data.frame(
    size_name = size,
    mids = hist_data$mids[non_zero],
    counts = counts_vec[non_zero],
    frequency = freq_vec[non_zero],
    bin_width = bin_width,
    stringsAsFactors = FALSE
  )
})

hist_combined <- do.call(rbind, hist_list)
hist_combined$size_name <- factor(hist_combined$size_name, levels = col_names)

p2 <- ggplot(hist_combined, aes(x = mids, y = frequency, fill = size_name)) +
  geom_col(alpha = 0.5, position = "identity", width = hist_combined$bin_width * 0.9) +
  scale_y_continuous(name = "Relative Frequency", labels = scales::number_format(accuracy = 0.01)) +
  scale_x_continuous(
    name = "Time (μs)",
    limits = x_limits,
    breaks = x_breaks,
    labels = function(x) round(2^x, 1),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = colors, name = "Size") +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(subtitle = "Histogram (relative frequency per group)")

# Combine plots with patchwork
combined_plot <- p1 / p2 + plot_layout(heights = c(3, 1))

# Save the plot
output_file <- paste0(plot_name, "_copy.svg")
ggsave(output_file, plot = combined_plot, width = 14, height = 10, dpi = 300)

cat(sprintf("\nPlot saved to: %s\n", output_file))
