#!/usr/bin/env Rscript

suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(tidyr))

args <- commandArgs(trailingOnly = TRUE)
if(length(args) < 3) stop("Usage: Rscript compareNlow.R <csv_base> <name_base> <csv_other1> <name_other1> [<csv_other2> <name_other2> ...]")

# Parse arguments: first dataset is baseline
csv_paths <- args[seq(1, length(args), by = 2)]
names_list <- args[seq(2, length(args), by = 2)]

# Read all CSVs
dfs <- lapply(csv_paths, read.csv, header = TRUE, check.names = FALSE)

# --- FIX 1: normalize all column names (remove whitespace etc.) ---
dfs <- lapply(dfs, function(df) {
    colnames(df) <- trimws(colnames(df))
    df
})

# Check all CSVs have same number of columns
ncols <- sapply(dfs, ncol)
if(length(unique(ncols)) != 1)
    stop("All CSV files must have the same number of columns")

# Convert column names to numeric sizes for sorting
size_to_bytes <- function(s) {
    s <- toupper(trimws(s))
    multiplier <- 1
    if(grepl("K$", s)) multiplier <- 2^10
    if(grepl("M$", s)) multiplier <- 2^20
    as.numeric(gsub("[KM]$", "", s)) * multiplier
}

# Use vapply to guarantee numeric output
col_sizes <- vapply(colnames(dfs[[1]]), size_to_bytes, numeric(1))

if (any(is.na(col_sizes))) {
    stop("Column names must be numeric sizes. Failed to parse: ",
         paste(colnames(dfs[[1]])[is.na(col_sizes)], collapse = ", "))
}

# Sorted order of columns
col_order  <- order(col_sizes)
col_levels <- colnames(dfs[[1]])[col_order]

# Prepare long dataframe for violin plot
long_list <- list()
for(j in 2:length(dfs)) {
    base  <- dfs[[1]]
    other <- dfs[[j]]
    
    long_df <- data.frame(
        Column  = rep(colnames(base), each = nrow(base)),
        Speedup = as.numeric(unlist(base / other)),
        Dataset = names_list[j]
    )
    
    long_df <- long_df[!is.na(long_df$Speedup) & is.finite(long_df$Speedup), ]
    long_df$Column <- factor(long_df$Column, levels = col_levels)
    
    long_list[[j-1]] <- long_df
}

long_df_all <- do.call(rbind, long_list)
dataset_order <- names_list[-1]  # skip baseline
long_df_all$Dataset <- factor(long_df_all$Dataset, levels = dataset_order)

# Create color palette per dataset
datasets <- unique(long_df_all$Dataset)
primary6 <- c(
    "#1f77b4",  # Blue
    "#2ca02c",  # Green
    "#d62728",  # Red
    "#F67f6d",  # Red
    "#9467bd",  # Purple
    "#ff7f0e",   # Orange
    "#Ff00Ff"   # Magenta
)
colors <- rep(primary6, length.out = length(datasets))
names(colors) <- datasets

# Plot
p <- ggplot(long_df_all, aes(x = Column, y = Speedup, fill = Dataset)) +
    geom_violin(alpha = 0.6, color = NA, scale = "width") +
    scale_fill_manual(values = colors, name = "Buffer Size") +
    scale_y_log10(breaks = c(1, 0.75, 0.5, .25, 0.1, 0.075, 0.05, 0.025, 0.01, 0.0075, 0.005, 0.0025, 0.001)) +
	geom_hline(
			   yintercept = 1.0,      # line at Y = 1
			   color = "black",       # line color
			   alpha = 0.6,           # fully opaque
			   linetype = "solid",    # optional: solid line
			   linewidth = 0.3             # optional: line thickness
	) +
    labs(
        title = paste("Speedup compared to", names_list[1]),
        x = "Data Size (bytes)",
        y = "Speedup"
    ) +
    theme_minimal(base_size = 12) +
    theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        axis.text.x      = element_text(angle = 45, hjust = 1),
        legend.position  = "right",
		panel.grid.minor = element_blank()  # <-- removes minor grid lines
    )

# Save plot
output_file <- paste0("speedup_vs_", names_list[1], ".svg")
ggsave(output_file, p, width = 14, height = 6)
cat("Plot saved to", output_file, "\n")
