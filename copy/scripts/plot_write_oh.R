#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
})

data_dir <- "../data/overhead"
files <- list.files(data_dir, pattern = "^write_\\d+\\.csv$", full.names = TRUE)

# Read file column-wise
read_multi <- function(file) {
  buffer_size <- as.numeric(str_extract(basename(file), "\\d+"))
  df <- read_csv(file, col_names = FALSE, show_col_types = FALSE)
  df_long <- df %>%
    pivot_longer(everything(), names_to = "measurement", values_to = "time_us") %>%
    mutate(buffer_size = buffer_size)
  df_long
}

data <- map_dfr(files, read_multi)

trim_outliers <- function(x, lower = 0.00, upper = 1.0) {
  q <- quantile(x, probs = c(lower, upper), na.rm = TRUE)
  x[x >= q[1] & x <= q[2]]
}

# Trim 1% low and 1% high
per_measurement <- data %>%
  group_by(buffer_size, measurement) %>%
  summarise(
    trimmed = list(trim_outliers(time_us, 0.01, 0.99)),
    mean_time = mean(trimmed[[1]], na.rm = TRUE),
    .groups = "drop"
  )

# Per buffer-size mean + 99.5CI
summary_data <- per_measurement %>%
  group_by(buffer_size) %>%
  summarise(
    mean_of_means = mean(mean_time, na.rm = TRUE),
    sd_of_means = sd(mean_time, na.rm = TRUE),
    n = n(),
    se = sd_of_means / sqrt(n),
    ci995 = ifelse(n > 1, qt(0.9975, df = n - 1) * se, NA_real_),
    .groups = "drop"
  )

# Regression over every column's mean
fit <- lm(mean_time ~ buffer_size, data = per_measurement)
summary_fit <- summary(fit)
slope <- coef(fit)[2]
intercept <- coef(fit)[1]
r2 <- summary_fit$r.squared

subtitle_text <- sprintf(
  "Regression: time = %.3e * buffer_size + %.3f | R² = %.4f",
  slope, intercept, r2
)

# Sample only half the points for the jitter plot, otherwise svg becomes too big.
set.seed(1)
per_measurement_sample <- per_measurement %>%
  sample_frac(0.5)

# Plot
p <- ggplot() +
	geom_violin(
				data = data,
				aes(x = buffer_size, y = time_us, group = factor(buffer_size)),
				fill = "skyblue", color = "gray", scale = "width", alpha = 1.0
				) +
	geom_jitter(
				data = per_measurement_sample,
				aes(x = buffer_size, y = mean_time, color = "Measurement means"),
				width = 0.05, height = 0, alpha = 0.6, size = 1.2
				) +
	geom_errorbar(
				  data = summary_data,
				  aes(
					  x = buffer_size,
					  ymin = mean_of_means - ci995,
					  ymax = mean_of_means + ci995,
					  color = "99.5% CI"
					  ),
				  width = 0.1, size = 0.8
				  ) +
	geom_smooth(
				data = per_measurement,
				aes(x = buffer_size, y = mean_time, color = "Linear regression"),
				method = "lm", se = FALSE, linetype = "dashed", size = 0.7
				) +
	scale_x_continuous(trans = "log2", breaks = 2^(0:20)) +
	scale_y_continuous(trans = "log2") +
	scale_color_manual(
					   name = "Legend",
					   values = c(
								  "Measurement means" = "orange",
								  "99.5% CI" = "black",
								  "Linear regression" = "red"
					   )
					   ) +
	labs(
		 title = "Write Time vs Buffer Size (2% trimmed)",
		 subtitle = subtitle_text,
		 x = "Buffer size (bytes)",
		 y = "Write time (µs)"
		 ) +
	theme(
		  panel.background = element_rect(fill = "white", color = NA),
		  plot.background  = element_rect(fill = "white", color = NA),
		  panel.grid.major = element_line(color = "gray90"),
		  panel.grid.minor = element_line(color = "gray95"),
		  axis.text = element_text(color = "black"),
		  axis.ticks = element_line(color = "black"),
		  legend.background = element_rect(fill = "white"),
		  legend.key = element_rect(fill = "white"),
		  legend.position = "right"
	)

ggsave("write_timings_means_ci995.svg", p, width = 18, height = 10, dpi = 300)
