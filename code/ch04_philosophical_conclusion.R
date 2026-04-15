# =============================================================================
# Agent-Based Modeling for Statisticians
# JSM Short Course — Chapter 4: The Possibility Space of Dynamics
#
# This script illustrates the philosophical arguments of the course's
# concluding chapter with R visualizations. There is almost no new modeling
# apparatus — the tools here are in service of an argument about what all
# the preceding models share, and what that implies for how statisticians
# should think about the scope of any model's conclusions.
#
# Three visualizations:
#   1. Regularization unification: ridge (L2) and LASSO (L1) penalty geometry
#      as Gaussian and Laplace priors — the same restriction, three descriptions.
#   2. Stationarity as a prior: logistic growth with a naive extrapolation
#      that cannot see the carrying capacity.
#   3. Bifurcation diagram of the logistic map: the possibility space
#      visible in the parameter dimension.
#
# Author: John Lynham
# Course: Agent-Based Modeling for Statisticians, JSM 2025
# =============================================================================


# =============================================================================
# SETUP
# =============================================================================

library(tidyverse)   # data manipulation and ggplot2
library(deSolve)     # ODE solver (logistic growth)
library(patchwork)   # compositing multiple ggplot2 panels


# Custom theme (self-contained)
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
# SECTION 4.2: THE REGULARIZATION UNIFICATION
# =============================================================================
# A Gaussian prior on regression coefficients produces ridge regression exactly.
# A Laplace prior produces LASSO exactly. These are not analogies — they are
# the same constraint written in two languages.
#
# The geometry makes this concrete: on a 2D coefficient plane, the feasible
# region defined by an L2 penalty (||beta||_2 <= t) is a disk (Gaussian prior
# contours); the feasible region defined by an L1 penalty (||beta||_1 <= t)
# is a diamond (Laplace prior contours). The L1 diamond has corners on the axes,
# which is why LASSO produces sparse solutions — the constraint is more likely
# to touch the loss contours at a corner, where one coefficient is zero.
#
# This is not a new observation. But naming it — every modeling assumption is
# a prior, every prior restricts expressive complexity — is the bridge from
# the statistical toolbox to the ABM framework.

# Build a grid of (beta1, beta2) values for visualizing penalty contours

beta_grid <- expand.grid(
  beta1 = seq(-2, 2, length.out = 300),
  beta2 = seq(-2, 2, length.out = 300)
) %>%
  as_tibble() %>%
  mutate(
    l1_norm = abs(beta1) + abs(beta2),     # L1 (LASSO / Laplace prior)
    l2_norm = sqrt(beta1^2 + beta2^2),     # L2 (Ridge / Gaussian prior)
    # Posterior density contours for visualization
    # Gaussian prior: log-density proportional to -lambda * (beta1^2 + beta2^2)
    log_gaussian = -(beta1^2 + beta2^2),
    # Laplace prior: log-density proportional to -lambda * (|beta1| + |beta2|)
    log_laplace  = -(abs(beta1) + abs(beta2))
  )

# Constraint boundary values (penalty budget)
# These define the feasible sets for ridge and LASSO at the same "budget" t
t_ridge <- 1.2   # L2 constraint: ||beta||_2 <= t_ridge (disk)
t_lasso <- 1.2   # L1 constraint: ||beta||_1 <= t_lasso (diamond)

# Build the constraint boundary curves explicitly
# Ridge (L2): a circle of radius t_ridge
theta_seq <- seq(0, 2 * pi, length.out = 500)
ridge_boundary <- tibble(
  beta1 = t_ridge * cos(theta_seq),
  beta2 = t_ridge * sin(theta_seq),
  type  = "Ridge (L2 / Gaussian prior)"
)

# LASSO (L1): a diamond (rotated square) with vertices at (+/-t_lasso, 0) and (0, +/-t_lasso)
# Parameterize using the L1 unit ball corners
lasso_boundary <- tibble(
  beta1 = c( t_lasso,  0,        -t_lasso,  0,         t_lasso),
  beta2 = c( 0,        t_lasso,   0,        -t_lasso,   0),
  type  = "LASSO (L1 / Laplace prior)"
)

# Construct a hypothetical loss function (OLS sum-of-squares ellipse)
# centered away from origin to make the geometry visible
# Loss: (beta1 - 1.8)^2 / 0.4 + (beta2 - 1.2)^2 / 0.15
beta_grid <- beta_grid %>%
  mutate(
    loss = (beta1 - 1.8)^2 / 0.4 + (beta2 - 1.2)^2 / 0.15
  )

# Loss contour values for display
loss_levels <- c(4, 7, 12, 20)

# -- Ridge plot --
p_ridge <- ggplot() +
  # Loss function contours (the OLS ellipses)
  geom_contour(
    data    = beta_grid,
    mapping = aes(x = beta1, y = beta2, z = loss),
    breaks  = loss_levels,
    color   = "#AAAAAA",
    linewidth = 0.5
  ) +
  # Ridge constraint: filled disk
  geom_polygon(
    data    = ridge_boundary,
    mapping = aes(x = beta1, y = beta2),
    fill    = "#2D5F8A",
    alpha   = 0.18,
    color   = "#2D5F8A",
    linewidth = 1.0
  ) +
  # Mark origin (the "all-zero" solution)
  annotate("point", x = 0, y = 0, size = 2, shape = 21,
           fill = "#FAFAF7", color = "#1C1C1E", stroke = 1) +
  # The solution (where the constraint boundary touches the innermost loss contour)
  # For ridge: the tangency point is typically NOT on an axis
  annotate("point", x = t_ridge * 0.62, y = t_ridge * 0.78, size = 3,
           color = "#2D5F8A") +
  annotate("text",  x = t_ridge * 0.62 + 0.18, y = t_ridge * 0.78 + 0.12,
           label = "solution\n(not sparse)", hjust = 0,
           family = "Source Serif 4", size = 2.8, color = "#2D5F8A") +
  coord_equal(xlim = c(-2, 2.2), ylim = c(-2, 2.2)) +
  labs(
    title    = "Ridge Regression (L2 / Gaussian Prior)",
    subtitle = "Circular constraint: solution rarely on an axis",
    x        = expression(beta[1]),
    y        = expression(beta[2])
  )

# -- LASSO plot --
p_lasso <- ggplot() +
  # Loss function contours
  geom_contour(
    data    = beta_grid,
    mapping = aes(x = beta1, y = beta2, z = loss),
    breaks  = loss_levels,
    color   = "#AAAAAA",
    linewidth = 0.5
  ) +
  # LASSO constraint: diamond
  geom_polygon(
    data    = lasso_boundary,
    mapping = aes(x = beta1, y = beta2),
    fill    = "#8A4A2D",
    alpha   = 0.18,
    color   = "#8A4A2D",
    linewidth = 1.0
  ) +
  annotate("point", x = 0, y = 0, size = 2, shape = 21,
           fill = "#FAFAF7", color = "#1C1C1E", stroke = 1) +
  # The LASSO solution tends to land at a corner (one coefficient = 0)
  annotate("point", x = t_lasso, y = 0, size = 3, color = "#8A4A2D") +
  annotate("text",  x = t_lasso + 0.1, y = 0.18,
           label = "solution\n(sparse: β₂ = 0)", hjust = 0,
           family = "Source Serif 4", size = 2.8, color = "#8A4A2D") +
  coord_equal(xlim = c(-2, 2.2), ylim = c(-2, 2.2)) +
  labs(
    title    = "LASSO (L1 / Laplace Prior)",
    subtitle = "Diamond constraint: solution often at a corner (sparsity)",
    x        = expression(beta[1]),
    y        = expression(beta[2])
  )

p_regularization <- p_ridge + p_lasso +
  plot_annotation(
    title   = "The Regularization Unification",
    caption = paste0(
      "The Gaussian prior IS ridge regression (posterior mode = ridge estimator). ",
      "The Laplace prior IS LASSO. ",
      "Every modeling assumption is a prior; every prior restricts expressive complexity."
    )
  )

print(p_regularization)


# =============================================================================
# SECTION 4.3: STATIONARITY AS A PRIOR — THE LOGISTIC GROWTH FAILURE
# =============================================================================
# Stationarity is a prior. It asserts that the spectral structure of the
# data-generating process is stable across time — that the past is informative
# about the future in a particular strong sense. When the system is capable
# of bifurcation, this prior is miscalibrated in exactly the way a Gaussian
# prior is miscalibrated when the true signal is sparse: the language simply
# does not have the words for what is about to happen.
#
# The logistic growth example makes this concrete. A process in its
# exponential phase looks like unbounded growth to any stationary time series
# model. The carrying capacity is exterior to the model's language — not
# uncertain, not unestimated, but grammatically forbidden by the stationarity
# assumption.

set.seed(2025)   # seed before any stochastic draws

K  <- 1000   # carrying capacity (the ceiling the naive model cannot see)
r  <- 0.3    # intrinsic growth rate
N0 <- 10     # initial population size

# Time grid for full logistic trajectory
times_logistic <- seq(0, 50, by = 0.5)

# Analytic solution: N(t) = K / (1 + ((K - N0)/N0) * exp(-r*t))
# No need for ODE solver here — the logistic has a closed form.
N_logistic <- K / (1 + ((K - N0) / N0) * exp(-r * times_logistic))

logistic_df <- tibble(t = times_logistic, N = N_logistic, source = "True trajectory")


# "Observe" only the first 10 time units (the exponential phase).
# In this window the growth looks linear on a log scale, and the carrying
# capacity is invisible — it has not yet constrained the dynamics.
obs_window <- 10
obs_df     <- filter(logistic_df, t <= obs_window)

cat("Logistic growth — observed window (t <= ", obs_window, "):\n")
cat("  Min N: ", round(min(obs_df$N), 1), "\n")
cat("  Max N: ", round(max(obs_df$N), 1), "\n")
cat("  As fraction of K:", round(max(obs_df$N) / K, 3), "\n")
# The system has barely begun to decelerate — a stationary log-linear model
# will estimate strong exponential growth and extrapolate accordingly.


# Fit a log-linear (exponential) model to the observed window.
# This is the stationarity assumption made explicit: the growth rate
# in the observed window will continue indefinitely.
lm_fit <- lm(log(N) ~ t, data = obs_df)

cat("\nLog-linear fit on observed window:\n")
cat("  Intercept (log scale): ", round(coef(lm_fit)[1], 4), "\n")
cat("  Slope (growth rate):   ", round(coef(lm_fit)[2], 4), "\n")
cat("  True r:                ", r, "\n")
# The estimated growth rate will be close to r — but the model has no
# mechanism to know that growth will stop.


# Generate predictions from the naive model beyond the observation window
pred_times <- seq(obs_window, max(times_logistic), by = 0.5)
pred_df    <- tibble(
  t      = pred_times,
  # exp() converts from log scale back to original scale
  N      = exp(predict(lm_fit, newdata = tibble(t = pred_times))),
  source = "Naive extrapolation (stationarity assumption)"
)


# Combine for plotting
plot_df <- bind_rows(
  logistic_df,
  obs_df %>% mutate(source = "Observed window"),
  pred_df
) %>%
  mutate(
    source = factor(source, levels = c(
      "True trajectory",
      "Observed window",
      "Naive extrapolation (stationarity assumption)"
    ))
  )

# Vertical line marking end of observation window
p_logistic <- ggplot() +
  # True trajectory
  geom_line(
    data    = filter(plot_df, source == "True trajectory"),
    mapping = aes(x = t, y = N),
    color   = "#2D5F8A",
    linewidth = 1.1
  ) +
  # Observed data (thick, solid)
  geom_line(
    data      = filter(plot_df, source == "Observed window"),
    mapping   = aes(x = t, y = N),
    color     = "#2D5F8A",
    linewidth = 2.2
  ) +
  # Naive extrapolation (dashed rust)
  geom_line(
    data      = filter(plot_df, source == "Naive extrapolation (stationarity assumption)"),
    mapping   = aes(x = t, y = N),
    color     = "#8A4A2D",
    linewidth  = 1.0,
    linetype   = "dashed"
  ) +
  # Horizontal line at carrying capacity K
  geom_hline(yintercept = K, linetype = "dotted",
             color = "#555555", linewidth = 0.7) +
  annotate("text", x = 1, y = K + 30, label = paste0("K = ", K),
           hjust = 0, family = "Source Serif 4", size = 3.2, color = "#555555") +
  # Vertical line at observation cutoff
  geom_vline(xintercept = obs_window, linetype = "solid",
             color = "#DCDCD4", linewidth = 0.8) +
  annotate("text", x = obs_window + 0.5, y = 80, label = "Observation\nwindow ends",
           hjust = 0, family = "Source Serif 4", size = 2.8, color = "#555555") +
  # Manual legend via annotate
  annotate("text", x = 32, y = 750,
           label = "True trajectory (blue)", hjust = 0,
           family = "Source Serif 4", size = 3, color = "#2D5F8A") +
  annotate("text", x = 32, y = 690,
           label = "Naive extrapolation (rust dashed)", hjust = 0,
           family = "Source Serif 4", size = 3, color = "#8A4A2D") +
  scale_y_continuous(limits = c(0, max(pred_df$N) * 1.05),
                     labels = scales::label_comma()) +
  labs(
    title    = "Stationarity as a Prior: What Extrapolation Cannot See",
    subtitle = paste0(
      "Naive log-linear fit on t ≤ ", obs_window,
      " predicts unbounded growth. The carrying capacity is exterior to its language."
    ),
    x = "Time",
    y = "Population N(t)"
  )

print(p_logistic)


# =============================================================================
# SECTION 4.4: BIFURCATION DIAGRAM — THE POSSIBILITY SPACE
# =============================================================================
# The logistic map x_{t+1} = r * x_t * (1 - x_t) is perhaps the canonical
# example of how a simple rule produces qualitatively different dynamics
# depending on a single parameter. For r in [1, 3]: stable fixed point.
# For r in (3, 3.45): period-2 oscillation. Then period-4, period-8...
# then chaos. The system does not merely change quantitatively — it
# changes regime. The bifurcation points are the boundaries of the
# possibility space.
#
# This is the spatial analogue of the mean-field failure: just as network
# topology is exterior to the ODE's language, regime change is exterior to
# a stationarity prior. A time series model estimated in the stable region
# cannot express what happens at the bifurcation.

# Iterate the logistic map for each r value to find its attractor(s).
# The standard approach: discard n_burn transient iterations (burn-in),
# then collect n_collect steady-state values.

logistic_map_attractor <- function(r_val,
                                    n_burn    = 1000,  # iterations to discard (transient)
                                    n_collect = 200) { # iterations to keep (attractor)

  x <- 0.5   # starting value (arbitrary interior point)

  # Burn-in: let transients die out
  for (i in seq_len(n_burn)) {
    x <- r_val * x * (1 - x)
  }

  # Collect attractor points
  xs <- numeric(n_collect)
  for (i in seq_len(n_collect)) {
    x    <- r_val * x * (1 - x)
    xs[i] <- x
  }

  # Return a tibble: r value paired with all collected x values
  tibble(r = r_val, x = xs)
}

# Sweep r from 2.5 to 4.0
# - [2.5, 3.0]: stable fixed point (single attractor)
# - (3.0, 3.45): period-2 oscillation (two attractor points)
# - (3.45, 3.54): period-4
# - ... period-doubling cascade to chaos
# - (3.57, 4.0): chaotic regime (with windows of order)

set.seed(2025)   # seed before iteration (not stochastic here, but for reproducibility)

r_vals        <- seq(2.5, 4.0, length.out = 1200)   # fine grid over r
bifurcation_df <- map_dfr(r_vals, logistic_map_attractor)

# Annotate the key bifurcation thresholds
bifurcation_thresholds <- tibble(
  r_val = c(3.0, 3.449, 3.544, 3.5688),
  label = c("Period 2", "Period 4", "Period 8", "Onset of chaos"),
  y_pos = c(0.88, 0.93, 0.95, 0.97)   # vertical position for labels
)

p_bifurcation <- ggplot(bifurcation_df, aes(x = r, y = x)) +
  geom_point(size = 0.05, alpha = 0.15, color = "#2D5F8A") +
  # Vertical lines at bifurcation thresholds
  geom_vline(
    data    = bifurcation_thresholds,
    mapping = aes(xintercept = r_val),
    color   = "#8A4A2D",
    linewidth = 0.5,
    linetype = "dashed",
    alpha    = 0.7
  ) +
  # Labels at bifurcation thresholds
  geom_text(
    data    = bifurcation_thresholds,
    mapping = aes(x = r_val + 0.01, y = y_pos, label = label),
    hjust   = 0,
    size    = 2.8,
    family  = "Source Serif 4",
    color   = "#8A4A2D"
  ) +
  # Shade the chaotic region
  annotate("rect",
           xmin = 3.57, xmax = 4.0,
           ymin = 0,    ymax = 1,
           fill = "#C0392B", alpha = 0.04) +
  annotate("text", x = 3.77, y = 0.08, label = "Chaotic\nregime",
           hjust = 0.5, family = "Source Serif 4", size = 2.8,
           color = "#C0392B") +
  labs(
    title    = "Bifurcation Diagram: The Logistic Map",
    subtitle = paste0(
      "x_{t+1} = r·x_t·(1 - x_t). Each vertical slice shows the attractor for that r value. ",
      "The bifurcation points bound qualitatively distinct regimes."
    ),
    x = "Parameter r",
    y = "Attractor x*",
    caption = paste0(
      "A model estimated in the stable region (r < 3) cannot express what happens ",
      "at r > 3. The turn is exterior to the language built from the early data."
    )
  )

print(p_bifurcation)


# =============================================================================
# SECTION 4.5: THE STATISTICIAN'S CHECKLIST
# =============================================================================
# The checklist question is not answered with a visualization. It is the
# organizing principle for interpreting everything that came before.
#
# At what resolution is this model working, and is the phenomenon I care about
# expressible at that resolution?
#
# The ABM is not the answer to every question it raises. It is evidence that:
#   - the resolution choice is a choice,
#   - the language restriction is a restriction, and
#   - richer expressive resources exist when the phenomenon demands them.
#
# The tools are harder. The answers are less clean.
# They are more likely to be true.

# A final visualization: show the same logistic growth trajectory at three
# "observation resolutions" — fine (observing every timestep), coarse
# (observing every 5 timesteps), and very coarse (every 15 timesteps).
# Each resolution makes different features visible and different features
# inexpressible.

times_full   <- seq(0, 50, by = 0.5)
N_full       <- K / (1 + ((K - N0) / N0) * exp(-r * times_full))
full_df      <- tibble(t = times_full, N = N_full, resolution = "Full (Δt = 0.5)")

# Coarse sampling: every 5th time unit
coarse_df <- full_df %>%
  filter(t %% 5 == 0) %>%
  mutate(resolution = "Coarse (Δt = 5)")

# Very coarse: every 15th time unit
vcoarse_df <- full_df %>%
  filter(t %% 15 == 0) %>%
  mutate(resolution = "Very coarse (Δt = 15)")

resolution_df <- bind_rows(full_df, coarse_df, vcoarse_df) %>%
  mutate(
    resolution = factor(resolution,
                        levels = c("Full (Δt = 0.5)",
                                   "Coarse (Δt = 5)",
                                   "Very coarse (Δt = 15)"))
  )

p_resolution <- ggplot(resolution_df, aes(x = t, y = N, color = resolution)) +
  # Full trajectory as background
  geom_line(
    data    = filter(resolution_df, resolution == "Full (Δt = 0.5)"),
    mapping = aes(x = t, y = N),
    color   = "#DCDCD4",
    linewidth = 1.0
  ) +
  # Coarse and very coarse as points connected by lines
  geom_point(
    data    = filter(resolution_df, resolution != "Full (Δt = 0.5)"),
    mapping = aes(x = t, y = N, color = resolution),
    size    = 2.5
  ) +
  geom_line(
    data    = filter(resolution_df, resolution != "Full (Δt = 0.5)"),
    mapping = aes(x = t, y = N, color = resolution),
    linewidth = 0.7
  ) +
  geom_hline(yintercept = K, linetype = "dotted",
             color = "#555555", linewidth = 0.6) +
  scale_color_manual(
    values = c(
      "Full (Δt = 0.5)"       = "#DCDCD4",
      "Coarse (Δt = 5)"       = "#2D5F8A",
      "Very coarse (Δt = 15)" = "#8A4A2D"
    ),
    breaks = c("Coarse (Δt = 5)", "Very coarse (Δt = 15)")
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  labs(
    title    = "Temporal Resolution and What It Makes Expressible",
    subtitle = paste0(
      "The same process observed at different sampling frequencies. ",
      "A quarterly model fitted in the exponential phase cannot see the ceiling."
    ),
    x     = "Time",
    y     = "N(t)",
    color = NULL,
    caption = paste0(
      "Coarse resolution is not wrong about the dynamics it was built to describe. ",
      "The error is resolution mismatch: applying a low-frequency language to a high-frequency phenomenon."
    )
  )

print(p_resolution)


# =============================================================================
# SUMMARY FIGURE
# =============================================================================

p_ch4_summary <- (p_regularization) / (p_logistic | p_bifurcation) +
  plot_annotation(
    title   = "Chapter 4: The Possibility Space of Dynamics",
    caption = paste0(
      "Top: Regularization = Bayesian prior (same constraint, two descriptions). ",
      "Bottom left: Stationarity miscalibrated by logistic ceiling. ",
      "Bottom right: Bifurcation diagram — qualitative regimes in parameter space."
    )
  )

print(p_ch4_summary)

# =============================================================================
# END OF CHAPTER 4
# =============================================================================
