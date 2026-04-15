# =============================================================================
# Agent-Based Modeling for Statisticians
# JSM Short Course — Chapter 3: The Size of Firms, and What It Tells Us
#
# This script covers the Axtell firm size empirics. No agent-based model
# is run here. The R code serves a different purpose: to show, with the
# statistical tools statisticians already trust, that the firm size power
# law is not merely a poorly-fitting alternative to standard distributions.
# It is a regularity that standard models cannot generate because it requires
# a mechanism — heterogeneous agents, local returns to scale, endogenous
# team formation — that their languages do not contain.
#
# NOTE ON DATA:
#   This script uses synthetic firm size data generated from a known power
#   law distribution as a placeholder. For production use, substitute
#   US Census Bureau Statistics of US Businesses (SUSB) microdata, available at:
#     https://www.census.gov/programs-surveys/susb.html
#   The SUSB provides employment counts by firm for all US establishments.
#   All analysis below applies unchanged to the real data. The synthetic data
#   reproduces the qualitative empirical pattern with alpha ~ 2, consistent
#   with Axtell (2001) and subsequent replication studies.
#
# Key reference:
#   Axtell, R. L. (2001). Zipf Distribution of U.S. Firm Sizes.
#   Science, 293(5536), 1818-1820.
#
# Author: John Lynham
# Course: Agent-Based Modeling for Statisticians, JSM 2025
# =============================================================================


# =============================================================================
# SETUP
# =============================================================================

library(tidyverse)   # data manipulation and ggplot2
library(poweRlaw)    # Clauset-Shalizi-Newman power law fitting and testing
library(broom)       # tidy model output (used for lognormal comparison)
library(patchwork)   # compositing multiple ggplot2 panels


# Custom theme (self-contained — no dependence on prior chapters)
course_theme <- theme_minimal(base_family = "Source Serif 4") +
  theme(
    plot.title       = element_text(family = "Raleway", face = "plain",
                                    size = 14, margin = margin(b = 8)),
    plot.subtitle    = element_text(family = "Source Serif 4", size = 11,
                                    color = "#555555", margin = margin(b = 12)),
    axis.title       = element_text(family = "Source Serif 4", size = 10),
    legend.position  = "bottom",
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "#FAFAF7", color = NA)
  )

theme_set(course_theme)


# =============================================================================
# SECTION 3.1: GENERATE SYNTHETIC FIRM SIZE DATA
# =============================================================================
# rplcon() generates random deviates from a continuous power law with
# lower cutoff xmin and tail exponent alpha.
# P(X >= x) ~ x^{-(alpha-1)} for x >= xmin.
# Alpha ~ 2.06 is the empirical estimate from Axtell (2001) for US firms.

set.seed(2025)   # reproducibility

n_firms    <- 5000   # number of synthetic firms (real SUSB has ~6 million)
alpha_true <- 2.06   # power law exponent — matches Axtell's empirical estimate
xmin_true  <- 1      # lower cutoff: smallest firm has at least 1 employee

# Generate from continuous power law, then round to get integer employee counts.
# In the real data, employment is always integer-valued.
firm_sizes_raw <- rplcon(n_firms, xmin = xmin_true, alpha = alpha_true)
firm_df        <- tibble(employment = pmax(1L, round(firm_sizes_raw)))

cat("Synthetic firm size data:\n")
cat("  N firms:        ", nrow(firm_df), "\n")
cat("  Median size:    ", median(firm_df$employment), "employees\n")
cat("  Mean size:      ", round(mean(firm_df$employment), 1), "employees\n")
cat("  Max size:       ", max(firm_df$employment), "employees\n")
cat("  Ratio max/med:  ", round(max(firm_df$employment) / median(firm_df$employment)), "\n")
# The ratio of max to median is the key visual fact: the dynamic range
# defeats linear-scale visualization entirely.


# =============================================================================
# SECTION 3.1a: THE DYNAMIC RANGE PROBLEM — LINEAR SCALE
# =============================================================================
# Show the histogram on a linear scale first. The dynamic range defeats it.
# The defeat is instructive: it demonstrates why the normal tools of
# distributional visualization fail here, and why the log-log scale matters.

p_linear_hist <- ggplot(firm_df, aes(x = employment)) +
  geom_histogram(
    bins  = 60,
    fill  = "#2D5F8A",
    color = "#FAFAF7",
    alpha = 0.85
  ) +
  labs(
    title    = "Firm Size Distribution: Linear Scale",
    subtitle = "The dynamic range makes most of the distribution invisible",
    x        = "Employment (number of employees)",
    y        = "Count"
  )

print(p_linear_hist)


# =============================================================================
# SECTION 3.1b: LOG-LOG CCDF PLOT
# =============================================================================
# The CCDF (complementary cumulative distribution function) P(X >= x) is
# the natural display for power laws: on log-log axes, a power law
# P(X >= x) = (x/xmin)^{-(alpha-1)} appears as a straight line.
# This is both more informative and more honest than a log-log histogram
# (which is sensitive to bin width choice).

# Compute empirical CCDF: for each unique size x, P(X >= x) = (rank from top) / N
firm_ccdf <- firm_df %>%
  arrange(employment) %>%
  mutate(
    rank     = rev(seq_len(n())),     # rank 1 = largest firm
    ccdf     = rank / n()             # empirical P(X >= x)
  )

p_loglog <- ggplot(firm_ccdf, aes(x = employment, y = ccdf)) +
  geom_point(size = 0.8, alpha = 0.4, color = "#2D5F8A") +
  scale_x_log10(
    labels = scales::label_comma(),
    name   = "Employment (log scale)"
  ) +
  scale_y_log10(
    labels = scales::label_percent(accuracy = 0.01),
    name   = "P(X ≥ x) (log scale)"
  ) +
  labs(
    title    = "Firm Size Distribution: Log-Log CCDF",
    subtitle = "A straight line on log-log axes is the signature of a power law"
  )

print(p_loglog)


# =============================================================================
# SECTION 3.2: POWER LAW FITTING VIA MAXIMUM LIKELIHOOD
# =============================================================================
# The Clauset, Shalizi, Newman (2009) methodology uses maximum likelihood
# estimation rather than OLS on a log-log histogram (which gives biased
# estimates). The poweRlaw package implements this approach.
#
# Two steps:
#   1. Estimate xmin: the lower bound of the power law scaling region.
#      estimate_xmin() searches over candidate xmin values and selects the
#      one that minimizes the KS distance between empirical and fitted CCDF.
#   2. Given xmin, estimate alpha by MLE.

# Create a discrete power law object (displ = discrete power law)
# Use displ (discrete) because employment is integer-valued.
m_pl <- displ$new(firm_df$employment)

# Step 1: Estimate xmin
est_xmin <- estimate_xmin(m_pl)    # finds optimal lower cutoff
m_pl$setXmin(est_xmin)             # set the estimated xmin on the model object

# Step 2: Estimate alpha given xmin
m_pl$setPars(estimate_pars(m_pl))  # MLE of alpha, using data >= xmin

# Report estimates
cat("\nPower law fit (Clauset-Shalizi-Newman method):\n")
cat("  Estimated x_min (lower cutoff):  ", m_pl$getXmin(), "\n")
cat("  Estimated alpha (tail exponent): ", round(m_pl$getPars(), 4), "\n")
cat("  True alpha (data-generating):   ", alpha_true, "\n")
cat("  N obs. in scaling region:        ",
    sum(firm_df$employment >= m_pl$getXmin()), "\n")

# Standard error via bootstrap (takes ~ 30s; reduce no_of_sims for speed)
# Uncomment to run:
# bs_result <- bootstrap(m_pl, no_of_sims = 100, threads = 1)
# cat("  Bootstrap SE of alpha: ",
#     round(sd(bs_result$bootstraps$pars), 4), "\n")


# =============================================================================
# SECTION 3.2a: FITTED LINE ON LOG-LOG PLOT
# =============================================================================
# Add the MLE-fitted power law CCDF to the empirical log-log plot.
# The fitted line should track the empirical points in the scaling region
# (employment >= xmin).

# Generate the theoretical CCDF from the fitted model for x in the scaling region
x_seq    <- seq(m_pl$getXmin(), max(firm_df$employment), length.out = 300)
ccdf_fit <- (x_seq / m_pl$getXmin())^(-(m_pl$getPars() - 1))   # power law CCDF formula

fit_line_df <- tibble(employment = x_seq, ccdf_fitted = ccdf_fit)

# Scale the fitted CCDF to match the empirical tail probability at xmin
# (the fitted line is a conditional probability; we need to anchor it)
empirical_tail_prob <- mean(firm_df$employment >= m_pl$getXmin())
fit_line_df <- fit_line_df %>%
  mutate(ccdf_fitted = ccdf_fitted * empirical_tail_prob)

p_loglog_fit <- p_loglog +
  geom_line(
    data    = fit_line_df,
    mapping = aes(x = employment, y = ccdf_fitted),
    color   = "#8A4A2D",
    linewidth = 1.1
  ) +
  # Vertical line at xmin to show where the scaling region begins
  geom_vline(
    xintercept = m_pl$getXmin(),
    linetype   = "dotted",
    color      = "#555555",
    linewidth  = 0.7
  ) +
  annotate(
    "text",
    x     = m_pl$getXmin() * 1.3,
    y     = 0.003,
    label = paste0("x_min = ", m_pl$getXmin()),
    hjust = 0,
    family = "Source Serif 4",
    size   = 3.2,
    color  = "#555555"
  ) +
  labs(
    subtitle = paste0(
      "MLE-fitted power law (rust line), α̂ = ",
      round(m_pl$getPars(), 3),
      ". Dotted line: estimated x_min."
    )
  )

print(p_loglog_fit)


# =============================================================================
# SECTION 3.2b: LOGNORMAL COMPARISON
# =============================================================================
# The lognormal distribution is a natural alternative to the power law for
# right-skewed size distributions — it arises from multiplicative random
# growth (Gibrat's law). A power law and a lognormal can look similar over
# a limited range on a log-log plot. We distinguish them rigorously.
#
# Approach: fit a lognormal to the same truncated data (x >= xmin) and
# compare log-likelihoods. The poweRlaw package supports this via
# compare_distributions().

# Fit lognormal to data in the same scaling region
m_ln <- dislnorm$new(firm_df$employment)     # discrete lognormal object
m_ln$setXmin(m_pl$getXmin())                 # same xmin as power law fit
m_ln$setPars(estimate_pars(m_ln))            # MLE of (meanlog, sdlog)

cat("\nLognormal fit parameters (discrete, x >= xmin):\n")
cat("  mu_log    =", round(m_ln$getPars()[1], 4), "\n")
cat("  sigma_log =", round(m_ln$getPars()[2], 4), "\n")

# Log-likelihood comparison
ll_pl <- dist_ll(m_pl)   # log-likelihood of power law fit
ll_ln <- dist_ll(m_ln)   # log-likelihood of lognormal fit

cat("\nLog-likelihood comparison (truncated to x >= xmin):\n")
cat("  Power law log-likelihood:  ", round(ll_pl, 2), "\n")
cat("  Lognormal log-likelihood:  ", round(ll_ln, 2), "\n")
cat("  Difference (PL - LN):      ", round(ll_pl - ll_ln, 2), "\n")
cat("  Positive favors power law; negative favors lognormal.\n")

# Vuong-style likelihood ratio test (compare_distributions computes this)
vuong_test <- compare_distributions(m_pl, m_ln)
cat("\nVuong test for non-nested distribution comparison:\n")
cat("  Test statistic: ", round(vuong_test$test_statistic, 4), "\n")
cat("  p-value:        ", round(vuong_test$p_two_sided, 4), "\n")
cat("  (p < 0.05 with positive test stat favors power law over lognormal)\n")


# =============================================================================
# SECTION 3.3: WHAT THE STANDARD MODELS CANNOT SAY
# =============================================================================
# A model that treats mean firm size as a sufficient statistic for aggregate
# dynamics produces a distribution with a characteristic scale — a typical
# firm size around which variation is distributed. An ODE of industry dynamics
# produces smooth trajectories toward equilibrium. Neither generates a power
# law, because neither contains the mechanism: heterogeneous agents, local
# increasing returns to scale, endogenous team formation and dissolution.
#
# The sufficiency assumption applied to the distribution of firms is the
# mean-field assumption operating at the level of firm characteristics rather
# than interactions. It collapses the entire heterogeneity to a point mass —
# the distribution is replaced by its mean.
#
# To make this concrete: show what a mean-sufficient model "predicts" about
# the firm size distribution — namely, a bell-shaped distribution around the
# mean, with thin tails.

# Illustrate: what the mean-sufficient model predicts about firm size distribution
# vs. what the data shows.

# Simulate a Gaussian / characteristic-scale distribution (what RA models imply)
set.seed(2025)
ra_pred_df <- tibble(
  employment = pmax(1, round(rnorm(n_firms,
                                   mean = mean(firm_df$employment),
                                   sd   = mean(firm_df$employment) * 0.3)))
)

# Compare CCDFs: empirical power law vs. mean-sufficient model prediction
firm_ccdf_ra <- ra_pred_df %>%
  arrange(employment) %>%
  mutate(rank = rev(seq_len(n())), ccdf = rank / n(), source = "Mean-sufficient prediction")

firm_ccdf_pl <- firm_df %>%
  arrange(employment) %>%
  mutate(rank = rev(seq_len(n())), ccdf = rank / n(), source = "Empirical (power law)")

ccdf_compare <- bind_rows(firm_ccdf_ra, firm_ccdf_pl)

p_compare <- ggplot(ccdf_compare, aes(x = employment, y = ccdf,
                                       color = source, shape = source)) +
  geom_point(size = 0.8, alpha = 0.35) +
  scale_x_log10(labels = scales::label_comma(), name = "Employment (log scale)") +
  scale_y_log10(labels = scales::label_percent(accuracy = 0.01),
                name = "P(X ≥ x) (log scale)") +
  scale_color_manual(
    values = c("Empirical (power law)"      = "#2D5F8A",
               "Mean-sufficient prediction" = "#8A4A2D")
  ) +
  scale_shape_manual(
    values = c("Empirical (power law)"      = 16,
               "Mean-sufficient prediction" = 17)
  ) +
  labs(
    title    = "Power law vs. mean-sufficient model prediction",
    subtitle = "The sufficiency assumption has no words for the upper tail",
    color    = NULL,
    shape    = NULL
  )

print(p_compare)


# =============================================================================
# SECTION 3.4: AXTELL'S MODEL — EMERGENCE
# =============================================================================
# No code runs the Axtell model here — that would require hours of simulation.
# Instead, this section documents the qualitative logic in comments,
# and visualizes the key empirical property: the power law exponent is
# structurally robust across parameter variations.
#
# Axtell's model rules (qualitative):
#   - Agents allocate effort between solitary work and team membership.
#   - Teams form when agents find collaborations that increase their returns
#     under local increasing returns to scale.
#   - Teams dissolve when agents find better alternatives elsewhere.
#   - No central planner. No equilibrium assumption. No assumption that the mean is sufficient.
#
# The power law emerges from the dynamics of team formation and dissolution.
# It is not calibrated — it is a structural consequence of the interaction rules.
# The exponent is not sensitive to most parameter choices. This is the
# canonical social science example of emergence: a macro regularity that is
# not in the rules but is generated by them.

# Visualize exponent robustness: show that alpha estimates are stable
# across different synthetic datasets (simulating what Axtell found across
# different parameter regimes and historical periods).

# Fit power law to several synthetic datasets with slightly different
# data-generating alpha values, and show the estimates cluster.

set.seed(999)
alpha_inputs  <- seq(1.9, 2.3, by = 0.1)   # range of "true" alphas
n_reps_robust <- 5   # replications per alpha value

robustness_df <- map_dfr(alpha_inputs, function(alpha_in) {
  map_dfr(seq_len(n_reps_robust), function(rep_i) {
    # Generate synthetic data for this alpha
    sz <- rplcon(2000, xmin = 1, alpha = alpha_in)
    sz <- pmax(1L, round(sz))

    # Fit power law
    m    <- displ$new(sz)
    xest <- estimate_xmin(m)
    m$setXmin(xest)
    m$setPars(estimate_pars(m))

    tibble(
      alpha_true     = alpha_in,
      alpha_estimate = m$getPars(),
      xmin_estimate  = m$getXmin(),
      rep            = rep_i
    )
  })
})

p_robustness <- ggplot(robustness_df,
                       aes(x = factor(alpha_true), y = alpha_estimate)) +
  geom_boxplot(fill = "#2D5F8A", color = "#1C1C1E", alpha = 0.7,
               outlier.shape = 16, outlier.size = 1.5) +
  geom_hline(yintercept = alpha_inputs,
             linetype = "dotted", color = "#8A4A2D", linewidth = 0.5) +
  labs(
    title    = "Power Law Exponent: Estimation Robustness",
    subtitle = "Boxplots of estimated α across 5 replications per true α value",
    x        = "True α (data-generating)",
    y        = "Estimated α̂ (MLE)",
    caption  = "In Axtell's model, the exponent is structurally determined — not calibrated."
  )

print(p_robustness)


# =============================================================================
# SUMMARY: FOUR-PANEL OVERVIEW FIGURE
# =============================================================================

p_ch3_summary <- (p_linear_hist | p_loglog_fit) / (p_compare | p_robustness) +
  plot_annotation(
    title   = "Chapter 3: Firm Size and the Limits of Standard Models",
    caption = paste0(
      "Synthetic data (N = ", n_firms, " firms) from Pareto distribution with α = ", alpha_true, ". ",
      "Substitute SUSB microdata for production analysis. ",
      "Power law fit: Clauset-Shalizi-Newman (2009) MLE method."
    )
  )

print(p_ch3_summary)

# =============================================================================
# END OF CHAPTER 3
# =============================================================================
