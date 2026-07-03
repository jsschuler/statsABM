# parse_axtell_data.R
#
# Parses Mathematica-formatted output files from the Axtell firm simulation
# into clean R data frames, saved as CSV files in data/axtell/.
#
# Input:  ../AxtellFirm/* (Mathematica list syntax)
# Output: data/axtell/*.csv
#
# Run this script once from the project root before rendering the book or slides.

library(tidyverse)

# ---- Paths ------------------------------------------------------------------

input_dir  <- "../AxtellFirm"
output_dir <- "data/axtell"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Parsing helpers --------------------------------------------------------

# Strip outer Mathematica braces and whitespace, then split on row boundaries.
# Each row in these files looks like: {val1, val2, ...},
# except time series rows look like: {year, {val1, val2, ...}},
split_rows <- function(text) {
  # Remove leading/trailing outer brace and whitespace
  text <- str_trim(text)
  text <- str_remove(text, "^\\{")
  text <- str_remove(text, "\\}$")
  # Split on pattern: }, (optional whitespace) newline {
  # i.e. the boundary between rows
  rows <- str_split(text, "\\},\\s*\n\\s*\\{")[[1]]
  rows <- str_remove_all(rows, "^\\{|\\}$")
  str_trim(rows)
}

# Parse a simple rectangular file: each row is {v1, v2} or {v1, v2, v3, v4}
parse_rectangular <- function(path, col_names) {
  text <- read_file(path)
  rows <- split_rows(text)
  rows <- rows[nzchar(rows)]

  map_dfr(rows, function(r) {
    vals <- as.numeric(str_split(r, ",")[[1]])
    set_names(as.list(vals), col_names)
  })
}

# Parse a nested time series file: each row is {year, {v1, v2}} or {year, {v1, v2, v3}}
# Assigns a run_id column by detecting year resets (year decreases from one row to the next),
# which indicates a new simulation run was concatenated into the same file.
parse_ts <- function(path, col_names) {
  text <- read_file(path)
  pattern <- "\\{\\s*([0-9.e+\\-]+)\\s*,\\s*\\{([^}]+)\\}\\s*\\}"
  matches <- str_match_all(text, pattern)[[1]]

  years <- as.numeric(matches[, 2])
  # Detect run boundaries: wherever year[i] <= year[i-1], a new run started
  run_id <- cumsum(c(1L, as.integer(diff(years) <= 0)))

  map_dfr(seq_len(nrow(matches)), function(i) {
    inner_vals <- as.numeric(str_split(str_trim(matches[i, 3]), "\\s*,\\s*")[[1]])
    set_names(as.list(c(run_id[i], years[i], inner_vals)), c("run_id", col_names))
  })
}

# ---- Parse each file --------------------------------------------------------

# Firm size distribution: {size, count}
firm_size <- parse_rectangular(
  file.path(input_dir, "Firm size distribution"),
  c("size", "count")
)

# Firm age distribution: {age, count}
firm_age <- parse_rectangular(
  file.path(input_dir, "Firm age distribution"),
  c("age", "count")
)

# Job tenure distribution: {tenure, count}
job_tenure <- parse_rectangular(
  file.path(input_dir, "Job tenure distribution"),
  c("tenure", "count")
)

# Firm output distribution: {output_bin, count}
firm_output_dist <- parse_rectangular(
  file.path(input_dir, "Firm output distribution"),
  c("output_bin", "count")
)

# Avg firm age by firm size: {size, mean_age, sd_age, n}
avg_age_by_size <- parse_rectangular(
  file.path(input_dir, "Avg firm age by firm size"),
  c("size", "mean_age", "sd_age", "n")
)

# Avg firm size by firm age: {age, mean_size, sd_size, n}
avg_size_by_age <- parse_rectangular(
  file.path(input_dir, "Avg firm size by firm age"),
  c("age", "mean_size", "sd_size", "n")
)

# Firm output by size: {size, mean_output, sd_output, n}
firm_output_by_size <- parse_rectangular(
  file.path(input_dir, "Firm output by size data"),
  c("size", "mean_output", "sd_output", "n")
)

# Firm size TS: {year, {mean_size, n_firms}}
firm_size_ts <- parse_ts(
  file.path(input_dir, "Firm size TS"),
  c("year", "mean_size", "n_firms")
)

# Number of firms TS: {year, {total, entries, exits}}
n_firms_ts <- parse_ts(
  file.path(input_dir, "Number of firms TS"),
  c("year", "total_firms", "entries", "exits")
)

# Output TS: {year, {mean_output, sd_output}}
output_ts <- parse_ts(
  file.path(input_dir, "Output TS"),
  c("year", "mean_output", "sd_output")
)

# ---- Write CSVs -------------------------------------------------------------

write_csv(firm_size,         file.path(output_dir, "firm_size_distribution.csv"))
write_csv(firm_age,          file.path(output_dir, "firm_age_distribution.csv"))
write_csv(job_tenure,        file.path(output_dir, "job_tenure_distribution.csv"))
write_csv(firm_output_dist,  file.path(output_dir, "firm_output_distribution.csv"))
write_csv(avg_age_by_size,   file.path(output_dir, "avg_age_by_size.csv"))
write_csv(avg_size_by_age,   file.path(output_dir, "avg_size_by_age.csv"))
write_csv(firm_output_by_size, file.path(output_dir, "firm_output_by_size.csv"))
write_csv(firm_size_ts,      file.path(output_dir, "firm_size_ts.csv"))
write_csv(n_firms_ts,        file.path(output_dir, "n_firms_ts.csv"))
write_csv(output_ts,         file.path(output_dir, "output_ts.csv"))

# ---- Quick sanity checks ----------------------------------------------------

cat("firm_size_distribution:  ", nrow(firm_size), "rows | size range:",
    min(firm_size$size), "-", max(firm_size$size), "\n")
cat("firm_age_distribution:   ", nrow(firm_age), "rows\n")
cat("job_tenure_distribution: ", nrow(job_tenure), "rows\n")
cat("firm_output_distribution:", nrow(firm_output_dist), "rows\n")
cat("avg_age_by_size:         ", nrow(avg_age_by_size), "rows\n")
cat("avg_size_by_age:         ", nrow(avg_size_by_age), "rows\n")
cat("firm_output_by_size:     ", nrow(firm_output_by_size), "rows\n")
cat("firm_size_ts:            ", nrow(firm_size_ts), "rows | year range:",
    min(firm_size_ts$year), "-", max(firm_size_ts$year), "\n")
cat("n_firms_ts:              ", nrow(n_firms_ts), "rows\n")
cat("output_ts:               ", nrow(output_ts), "rows\n")

cat("\nAll files written to", output_dir, "\n")
