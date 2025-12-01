#!/usr/bin/env Rscript

suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

args <- commandArgs(trailingOnly = TRUE)
if(length(args) < 3) stop("Usage: Rscript compare_csv_violin_colored.R <csv_base> <name_base> <csv_other1> <name_other1> [<csv_other2> <name_other2> ...]")

# Parse arguments: first dataset is baseline
csv_paths <- args[seq(1, length(args), by = 2)]
names_list <- args[seq(2, length(args), by = 2)]

# Read all CSVs
dfs <- lapply(csv_paths, read.csv, header = TRUE, check.names = FALSE)

# Check all CSVs have same number of columns
ncols <- sapply(dfs, ncol)
if(length(unique(ncols)) != 1) stop("All CSV files must have the same number of columns")

# Convert column names to numeric sizes for sorting
size_to_bytes <- function(s) {
  s <- toupper(s)
  multiplier <- 1
  if(grepl("K$", s)) multiplier <- 2^10
  if(grepl("M$", s)) multiplier <- 2^20
  as.numeric(gsub("[KM]$", "", s)) * multiplier
}
col_sizes <- sapply(colnames(dfs[[1]]), size_to_bytes)
col_order <- order(col_sizes)
col_levels <- colnames(dfs[[1]])[col_order]

# Function: Bootstrap 99.5% CI for mean speedup
bootstrap_ci <- function(base, other, nboot = 10000, conf = 0.995) {
  min_len <- min(length(base), length(other))
  base <- base[1:min_len]
  other <- other[1:min_len]
  
  speedup <- base / other
  speedup <- speedup[!is.na(speedup) & is.finite(speedup)]
  
  if(length(speedup) == 0) return(c(NA, NA))
  
  boot_means <- replicate(nboot, mean(sample(speedup, length(speedup), replace = TRUE), na.rm = TRUE))
  alpha <- (1 - conf)/2
  quantile(boot_means, probs = c(alpha, 1-alpha), na.rm = TRUE)
}

# Compute CI per column and dataset
ci_list <- list()
for(j in 2:length(dfs)) {  # skip baseline
  ci_df <- data.frame(
    Column = factor(colnames(dfs[[1]]), levels = col_levels),
    Lower = NA,
    Upper = NA,
    Mean = NA,
    Dataset = names_list[j]
  )
  for(i in 1:ncol(dfs[[1]])) {
    ci <- bootstrap_ci(dfs[[1]][[i]], dfs[[j]][[i]], conf = 0.995)
    ci_df$Lower[i] <- ci[1]
    ci_df$Upper[i] <- ci[2]
    ci_df$Mean[i] <- mean(dfs[[1]][[i]] / dfs[[j]][[i]], na.rm = TRUE)
  }
  ci_list[[j-1]] <- ci_df
}
ci_df_all <- do.call(rbind, ci_list)

# Prepare long dataframe for violin plot
long_list <- list()
for(j in 2:length(dfs)) {
  base <- dfs[[1]]
  other <- dfs[[j]]
  long_df <- data.frame(
    Column = rep(colnames(base), each = nrow(base)),
    Speedup = as.numeric(unlist(base / other)),
    Dataset = names_list[j]
  )
  long_df <- long_df[!is.na(long_df$Speedup) & is.finite(long_df$Speedup), ]
  long_df$Column <- factor(long_df$Column, levels = col_levels)
  long_list[[j-1]] <- long_df
}
long_df_all <- do.call(rbind, long_list)

# Create color palette per dataset
datasets <- unique(long_df_all$Dataset)
colors <- rainbow(length(datasets), s = 0.7, v = 0.8)
names(colors) <- datasets

# Plot
p <- ggplot(long_df_all, aes(x = Column, y = Speedup, fill = Dataset)) +
  geom_violin(alpha = 0.6, color = "gray30", scale = "width") +
  geom_point(data = ci_df_all,
             aes(x = Column, y = Mean, fill = Dataset),
             shape = 21, color = "black", size = 2, stroke = 0.5, inherit.aes = FALSE) +
  geom_errorbar(data = ci_df_all,
                aes(x = Column, ymin = Lower, ymax = Upper),
                width = 0.2, color = "black", inherit.aes = FALSE) +
  scale_fill_manual(values = colors, name = "Dataset") +
  scale_y_log10() +
  scale_y_log10(breaks = c(1, 1.5, 2, 4, 8, 10)) +
  labs(title = paste("Speedup compared to", names_list[1]),
       x = "Data Size (bytes)",
       y = "Speedup") +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Save plot
output_file <- paste0("speedup_vs_", names_list[1], ".svg")
ggsave(output_file, p, width = 14, height = 6)
cat("Plot saved to", output_file, "\n")
