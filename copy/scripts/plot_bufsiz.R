library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(ggridges)

# Define buffer sizes
buffer_sizes <- c(64, 128, 192, 256, 384, 512, 768, 1024, 1536, 2048, 3072, 4096, 6144, 8192, 12288, 16384, 24576, 32768, 49152, 65536)

# Function to parse size strings to bytes
parse_size <- function(size_str) {
  if (grepl("M$", size_str)) {
    return(as.numeric(sub("M$", "", size_str)) * 2^20)
  } else if (grepl("K$", size_str)) {
    return(as.numeric(sub("K$", "", size_str)) * 2^10)
  } else {
    return(as.numeric(size_str))
  }
}

# Read and process all data
all_data <- list()

for (bufsize in buffer_sizes) {
  file_path <- sprintf("../data/bufsiz/posix_buffered_%d.csv", bufsize)
  
  # Read the CSV
  data <- read_csv(file_path, col_names = TRUE, show_col_types = FALSE)
  
  # Get column names (copy sizes)
  copy_sizes <- colnames(data)
  
  # Convert to long format
  data_long <- data %>%
    mutate(run = row_number()) %>%
    pivot_longer(cols = -run, names_to = "copy_size_str", values_to = "time_us") %>%
    filter(!is.na(time_us)) %>%
    mutate(
      copy_size_bytes = sapply(copy_size_str, parse_size),
      buffer_size = bufsize,
      time_ns = time_us * 1000,
      ns_per_byte = time_ns / copy_size_bytes
    )
  
  all_data[[as.character(bufsize)]] <- data_long
}

# Combine all data
combined_data <- bind_rows(all_data)

# Convert buffer_size to factor for proper ordering and coloring
combined_data$buffer_size <- factor(combined_data$buffer_size, levels = buffer_sizes)

# Calculate 99.5% CI for each buffer_size and copy_size combination
ci_data <- combined_data %>%
  group_by(buffer_size, copy_size_bytes) %>%
  summarise(
    mean_ns_per_byte = mean(ns_per_byte),
    se = sd(ns_per_byte) / sqrt(n()),
    n = n(),
    ci_lower = mean_ns_per_byte - qt(0.9975, df = n - 1) * se,
    ci_upper = mean_ns_per_byte + qt(0.9975, df = n - 1) * se,
    .groups = "drop"
  )

# Generate colors and accent colors for CI brackets
n_sizes <- length(buffer_sizes)
colors <- rainbow(n_sizes, s = 0.7, v = 0.8)
names(colors) <- as.character(buffer_sizes)

# Create brighter accent colors for CI brackets
accent_colors <- rainbow(n_sizes, s = 0.9, v = 0.95)
names(accent_colors) <- as.character(buffer_sizes)

# Define X-axis labels and values
x_labels <- c("64", "128", "256", "512", "1K", "2K", "4K", "8K", "16K", "32K", 
              "64K", "128K", "256K", "512K", "1M")
x_values <- c(64, 128, 256, 512, 1024, 2048, 4096, 8192, 16384, 32768,
              65536, 131072, 262144, 524288, 1048576)

# Function to format Y-axis labels in linear scale
format_y_labels <- function(breaks) {
  sapply(breaks, function(x) {
    if (is.na(x) || is.infinite(x)) {
      return("")
    }
    value <- 2^x
    if (is.na(value) || is.infinite(value)) {
      return("")
    }
    if (value >= 1) {
      format(value, scientific = FALSE, big.mark = ",")
    } else {
      format(value, scientific = FALSE)
    }
  })
}

combined_data <- combined_data %>%
  mutate(copy_size_factor = factor(copy_size_bytes, 
                                   levels = sort(unique(copy_size_bytes))))
ci_data <- ci_data %>%
  mutate(copy_size_factor = factor(copy_size_bytes, 
                                   levels = sort(unique(copy_size_bytes))))
# Create the main plot
p <- ggplot(combined_data, aes(x = copy_size_factor, y = log2(ns_per_byte))) +
  # Violin plots for distributions
  geom_violin(aes(group = interaction(copy_size_bytes, buffer_size),
                  fill = buffer_size),
              position = position_dodge(width = 1.5),
              scale = "width",
              alpha = 0.7,
              trim = TRUE,
    		  width = 1.6,
              color = NA) +

	scale_x_discrete(labels = x_labels) +
  scale_fill_manual(values = colors, name = "Buffer Size") +
  scale_color_manual(values = accent_colors, name = "Buffer Size", guide = "none") +
  #scale_x_continuous(breaks = log2(x_values), labels = x_labels) +
  scale_y_continuous(labels = format_y_labels) +
  guides(
    fill = guide_legend(order = 1),
    linetype = guide_legend(
      title = "",
      override.aes = list(color = "gray30", linewidth = 1.2),
      order = 2
    )
  ) +
  labs(
    title = "Buffer Size Performance Benchmark",
    subtitle = "Distribution of ns/byte across different buffer sizes and copy sizes",
    x = "Bytes Copied",
    y = "ns/byte"
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.box = "vertical",
    plot.background = element_rect(fill = "white", color = NA),
  )

# Display the plot
print(p)

# Optionally save the plot
ggsave("buffer_benchmark_plot.svg", plot = p, width = 30, height = 12, dpi = 96)
