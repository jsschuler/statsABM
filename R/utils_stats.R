#' Fit a discrete power law using the poweRlaw package
#'
#' Uses the Clauset, Shalizi & Newman (2009) method via the `poweRlaw`
#' package to fit a discrete power-law distribution and compute the
#' Kolmogorov-Smirnov goodness-of-fit statistic.
#'
#' @param x Integer or numeric vector of positive values.
#' @return Named list with elements:
#'   - `alpha`: estimated power-law exponent
#'   - `xmin`: estimated lower cutoff
#'   - `ks_statistic`: KS distance between fitted and empirical distribution
#'   - `ks_p_value`: p-value from bootstrap KS test (may be NA if not computed)
#'   - `n`: number of observations at or above `xmin`
#' @export
#' @examples
#' x <- round(poweRlaw::rplcon(200, xmin = 1, alpha = 2.5))
#' x <- x[x >= 1]
#' fit <- fit_power_law(x)
#' fit$alpha
fit_power_law <- function(x) {
  x <- as.integer(round(x))
  x <- x[x >= 1L]

  m <- poweRlaw::displ$new(x)
  poweRlaw::setXmin(m, poweRlaw::estimate_xmin(m))
  poweRlaw::setParameters(m, poweRlaw::estimate_pars(m))

  ks  <- poweRlaw::dist_ll(m)  # log-likelihood, not KS directly
  # Use bootstrap_p for KS statistic; use a small number of bootstraps
  # to keep test run time low
  gof <- tryCatch(
    poweRlaw::bootstrap_p(m, no_of_sims = 100L, threads = 1L, seed = 1L),
    error = function(e) NULL
  )

  list(
    alpha        = m$pars,
    xmin         = m$xmin,
    ks_statistic = if (!is.null(gof)) gof$gof  else NA_real_,
    ks_p_value   = if (!is.null(gof)) gof$p    else NA_real_,
    n            = sum(x >= m$xmin)
  )
}

#' Compute the Gini coefficient
#'
#' The Gini coefficient measures statistical dispersion of a distribution.
#' A value of 0 corresponds to perfect equality; 1 to maximal inequality.
#'
#' @param x Non-negative numeric vector.
#' @return Scalar in [0, 1].
#' @export
#' @examples
#' gini(c(1, 1, 1, 1))   # 0 -- perfect equality
#' gini(c(0, 0, 0, 100)) # close to 1 -- maximal inequality
gini <- function(x) {
  x <- as.numeric(x)
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(NA_real_)
  x <- sort(x)
  n <- length(x)
  # Analytic formula: G = (2 * sum(i * x_i) / (n * sum(x))) - (n + 1) / n
  numerator   <- 2 * sum(seq_len(n) * x)
  denominator <- n * sum(x)
  if (denominator == 0) return(0)
  numerator / denominator - (n + 1L) / n
}

#' Compare two distributions via KS test and summary statistics
#'
#' @param simulated Numeric vector of simulated values.
#' @param empirical Numeric vector of empirical values.
#' @return Named list with elements:
#'   - `ks_statistic`: KS test statistic
#'   - `ks_p_value`: two-sample KS test p-value
#'   - `simulated_mean`: mean of simulated values
#'   - `empirical_mean`: mean of empirical values
#'   - `simulated_gini`: Gini coefficient of simulated values
#'   - `empirical_gini`: Gini coefficient of empirical values
#' @export
#' @examples
#' compare_distributions(rnorm(100), rnorm(100, mean = 1))
compare_distributions <- function(simulated, empirical) {
  ks_result <- stats::ks.test(simulated, empirical)
  list(
    ks_statistic    = unname(ks_result$statistic),
    ks_p_value      = ks_result$p.value,
    simulated_mean  = mean(simulated, na.rm = TRUE),
    empirical_mean  = mean(empirical, na.rm = TRUE),
    simulated_gini  = gini(simulated),
    empirical_gini  = gini(empirical)
  )
}
