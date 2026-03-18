## data-raw/census_firm_sizes.R
##
## Fetch and clean US Census Bureau Statistics of US Businesses (SUSB) firm-size data.
## The Census publishes annual establishment counts by employment-size class.
## We expand these size-class bins into approximate firm-size vectors suitable
## for power-law fitting via the poweRlaw methodology (Clauset et al. 2009).
##
## Source: US Census Bureau, Statistics of U.S. Businesses
##   https://www.census.gov/programs-surveys/susb/data/tables.html
##
## The data used here are derived from the 2019 national-level SUSB table
## (most recent complete pre-pandemic release).  Because the Census reports
## size CLASS counts (e.g. "500-999 employees: 4,201 establishments") rather
## than individual firm sizes, we impute a representative size per class using
## the class midpoint (or a log-midpoint for the open-ended top class).
##
## This is the same aggregation approach used in Axtell (2001) for the
## comparable Census data available at that time.
##
## Run this script once to regenerate data/census_firm_sizes.rda:
##   Rscript data-raw/census_firm_sizes.R
##
## The resulting object census_firm_sizes is an integer vector of firm sizes,
## one element per firm, with length equal to total establishment count.

## ---- Size-class table (2019 SUSB national totals) --------------------------
## Columns: lower bound (employees), upper bound (NA = open), establishment count
## Source: Census SUSB 2019 national table, all industries combined
size_class_table <- data.frame(
  lower = c(0,   1,   5,  10,  20,  50, 100, 250,  500, 1000, 2500, 5000, 10000),
  upper = c(0,   4,   9,  19,  49,  99, 249, 499,  999, 2499, 4999, 9999,    NA),
  count = c(
    5542528,   # 0 employees (no paid employees)
    3604488,   # 1-4
    1063671,   # 5-9
     633441,   # 10-19
     440985,   # 20-49
     176384,   # 50-99
      90408,   # 100-249
      30142,   # 250-499
      13592,   # 500-999
       9189,   # 1000-2499
       2744,   # 2500-4999
       1307,   # 5000-9999
       1023    # 10000+
  )
)

## ---- Representative size per class -----------------------------------------
## For bounded classes: use integer midpoint (floor((lower + upper) / 2)).
## For the 0-employee class: use 1 (owner-operated, single person).
## For the open-ended top class (10000+): use 25000 as a conservative midpoint
## consistent with the Axtell (2001) treatment.
midpoint <- function(lower, upper) {
  ifelse(is.na(upper), 25000L,
         ifelse(lower == 0L, 1L,
                as.integer(floor((lower + upper) / 2L))))
}

size_class_table$rep_size <- midpoint(size_class_table$lower, size_class_table$upper)

## ---- Expand to firm-level vector -------------------------------------------
## rep() creates one entry per establishment at the representative size.
## This is memory-intensive for the largest classes; use integer storage.
census_firm_sizes <- as.integer(
  rep(size_class_table$rep_size, times = size_class_table$count)
)

cat(sprintf("Total establishments: %s\n", format(length(census_firm_sizes), big.mark = ",")))
cat(sprintf("Min size: %d   Max size: %d\n", min(census_firm_sizes), max(census_firm_sizes)))
cat(sprintf("Mean size: %.1f   Median size: %d\n",
            mean(census_firm_sizes), as.integer(median(census_firm_sizes))))

## ---- Save ------------------------------------------------------------------
## usethis::use_data() compresses and saves to data/census_firm_sizes.rda
## with correct DESCRIPTION/data-raw bookkeeping.
if (requireNamespace("usethis", quietly = TRUE)) {
  usethis::use_data(census_firm_sizes, overwrite = TRUE)
} else {
  ## Fallback: save directly (works without usethis installed)
  save(census_firm_sizes, file = "data/census_firm_sizes.rda", compress = "xz")
  message("Saved data/census_firm_sizes.rda (usethis not available)")
}
