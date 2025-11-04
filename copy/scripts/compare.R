#!/usr/bin/env Rscript

suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

args <- commandArgs(trailingOnly = TRUE)
if(length(args) < 4) stop("Usage: Rscript compare_csv_violin_colored.R <csvA> <nameA> <csvB> <nameB>")

csvA_path <- args[1]
nameA <- args[2]
csvB_path <- args[3]
nameB <- args[4]

# Read CSVs
dfA <- read.csv(csvA_path, header = TRUE, check.names = FALSE)
dfB <- read.csv(csvB_path, header = TRUE, check.names = FALSE)
if(ncol(dfA) != ncol(dfB)) stop("CSV files must have the same number of columns")

# Convert column names to numeric sizes for sorting
size_to_bytes <- function(s) {
  s <- toupper(s)
  multiplier <- 1
  if(grepl("K$", s)) multiplier <- 2^10
  if(grepl("M$", s)) multiplier <- 2^20
  as.numeric(gsub("[KM]$", "", s)) * multiplier
}
col_sizes <- sapply(colnames(dfA), size_to_bytes)
col_order <- order(col_sizes)
col_levels <- colnames(dfA)[col_order]

# Function: Bootstrap 99.5% CI for mean speedup
bootstrap_ci <- function(a, b, nboot = 10000, conf = 0.995) {
  min_len <- min(length(a), length(b))
  a <- a[1:min_len]
  b <- b[1:min_len]

  speedup <- b / a
  speedup <- speedup[!is.na(speedup) & is.finite(speedup)]
  
  if(length(speedup) == 0) return(c(NA, NA))

  boot_means <- replicate(nboot, mean(sample(speedup, length(speedup), replace = TRUE), na.rm = TRUE))
  alpha <- (1 - conf)/2
  quantile(boot_means, probs = c(alpha, 1-alpha), na.rm = TRUE)
}

# Compute CI per column and create labels for legend
ci_df <- data.frame(
  Column = factor(colnames(dfA), levels = col_levels),
  Lower = NA,
  Upper = NA,
  Mean = NA,
  Label = NA
)
for(i in 1:ncol(dfA)) {
  ci <- bootstrap_ci(dfA[[i]], dfB[[i]], conf = 0.995)
  ci_df$Lower[i] <- ci[1]
  ci_df$Upper[i] <- ci[2]
  ci_df$Mean[i] <- mean(dfB[[i]] / dfA[[i]], na.rm = TRUE)
  ci_df$Label[i] <- sprintf("%s (mean=%.2f, 99.5%%CI=[%.2f,%.2f])",
                            col_levels[i], ci_df$Mean[i], ci_df$Lower[i], ci_df$Upper[i])
}

# Prepare data for violin plot
long_df <- data.frame(
  Column = rep(colnames(dfA), each = nrow(dfA)),
  Speedup = as.numeric(unlist(dfB / dfA))
)
long_df <- long_df[!is.na(long_df$Speedup) & is.finite(long_df$Speedup), ]
long_df$Column <- factor(long_df$Column, levels = col_levels)

# Map labels for coloring
long_df$Label <- factor(long_df$Column, levels = col_levels)
levels(long_df$Label) <- ci_df$Label

# Create color palette
n_bins <- length(col_levels)
colors <- rainbow(n_bins, s = 0.7, v = 0.8)
names(colors) <- ci_df$Label

# Plot
p <- ggplot(long_df, aes(x = Label, y = Speedup, fill = Label)) +
  # Gray outline for violins
  geom_violin(alpha = 0.6, color = "gray30", scale = "width") +
  # Mean points (circle with correct outline color)
  geom_point(data = ci_df,
             aes(x = Label, y = Mean, fill = Label),
             shape = 21, color = "black", size = 2, stroke = 0.5, inherit.aes = FALSE) +
  # Thin error bars for CI
  geom_errorbar(data = ci_df,
                aes(x = Label, ymin = Lower, ymax = Upper),
                width = 0.2, color = "black", inherit.aes = FALSE) +
  # Colors and legend
  scale_fill_manual(values = colors, name = "Bin (mean ± 99.5% CI)", labels = ci_df$Label) +
  #scale_y_log10() +
  labs(title = paste("Speedup of", nameA, "over", nameB),
       x = "Data Size (bytes)",
       y = paste("Speedup (", nameA, "/", nameB, ")", sep="")) +
  scale_x_discrete(labels = col_levels) +  # only show byte sizes on x-axis
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Save plot
output_file <- paste0("speedup_", nameA, "_vs_", nameB, ".svg")
ggsave(output_file, p, width = 14, height = 6)
cat("Plot saved to", output_file, "\n")
