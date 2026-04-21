# =============================================================================
# Agent-Based Modeling for Statisticians — JSM 2025
# Chapter 4: The Possibility Space of Dynamics
# =============================================================================
#
# This script contains all R code from Chapter 4 of the course book.
# It is self-contained: all required packages are loaded in the setup section.
#
# Sections:
#   1. Setup
#   2. Regularization geometry (L1 vs L2 constraint regions)
#   3. Logistic extrapolation trap
#   4. Bifurcation diagram (logistic map)
#   5. Two-window extrapolation (Kondratiev-style cycle)
#   6. Possibility space (multiple stochastic trajectories)
#
# Packages required:
#   tidyverse, deSolve, patchwork
#
# GitHub: [course repository]
# =============================================================================

## ----ch4-setup----------------------------------------------------------------
# Chapter 4 Setup
# Self-contained — does not depend on prior chapters.

library(tidyverse)
library(deSolve)
library(patchwork)

theme_set(
  theme_minimal(base_family = "Source Serif 4") +
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
)

#' 
#' ## Resolution {#sec-resolution}
#' 
#' A model does not merely approximate the world. It defines a language, and that language has an expressive boundary. A phenomenon that requires distinctions the model cannot make is not a failure of fit in the ordinary sense — it is not an error that better data or more careful estimation would reduce. It lies outside the language entirely. When a modeling tradition calls something a paradox or an anomaly, it is often naming a phenomenon that is simply exterior to its formal vocabulary: real, reproducible, and grammatically inexpressible in the available terms.
#' 
#' The restriction operates on different dimensions in different models, but the consequence is always the same. The mean-field assumption makes network topology and agent heterogeneity inexpressible — not unlikely, not poorly approximated, but absent from the language's vocabulary entirely. The assumption that aggregate dynamics are determined by mean firm size makes the firm size distribution inexpressible: the distribution requires heterogeneity, and the sufficiency of the mean is the hypothesis that heterogeneity does not matter. The stationarity family makes regime change and bifurcation inexpressible — the language built from data generated before the bifurcation has no vocabulary for the turn. An equilibrium assumption makes path dependence inexpressible: which equilibrium the system reaches is indeterminate, and the assumption selects one by fiat.
#' 
#' None of these is a failure that better technique remedies. Each is a structural fact about the model's language. More observations do not add words that the language lacks. More sophisticated estimation does not give the ODE a spatial dimension. The right question is not "can I fit this better?" but "can this language say what I need to say?"
#' 
#' Every modeling choice is a choice of resolution — temporal, spectral, expressive. The ODE, the sufficiency of the mean, stationarity, equilibrium, regularization penalties, the sampling frequency of a time series — all are restrictions on the language in which the model is written. Each restriction makes some phenomena expressible and others not. The phenomena made inexpressible are not less real than the ones the model can reach. They are simply outside the scope of what was chosen to be said.
#' 
#' ## The Regularization Unification {#sec-regularization}
#' 
#' The connection between regularization and the ODE-ABM relationship is not analogical. It is constructive. Writing it out makes visible something that the course has been approaching from several directions simultaneously.
#' 
#' In an ABM, each agent $i$ carries a property vector $\theta_i$ — a birth rate, a productivity, a threshold for infection. Write $\theta_i = \bar\theta + \varepsilon_i$, where $\bar\theta$ is the population mean and $\varepsilon_i$ is the deviation, centered so that $\sum_i \varepsilon_i = 0$. The ODE is the model that sets $\varepsilon_i = 0$ for every agent — all heterogeneity collapsed to the mean. Nothing about this restriction is forced by the biology or the economics. It is a structural choice about the distribution of agent properties, and that distribution is a point mass at $\bar\theta$. An L2 penalty on $\{\varepsilon_i\}$ shrinks agents toward their mean; as the penalty strength increases, the ABM continuously approaches the ODE. They are not separate models. They are the same model at different positions along a regularization path.
#' 
#' The contact structure submits to the same treatment. Represent it as a weighted adjacency matrix $W$, where $W_{ij} \geq 0$ is the contact rate between agents $i$ and $j$. Every network topology is a special case: $W_{ij} = 0$ removes an edge; a heavy-tailed degree distribution produces the scale-free network from Chapter 2; the lattice concentrates all weight on immediate neighbors. Write the mean-field contact structure as $\bar{W}_{ij} = 1/n$ for all $i \neq j$ — uniform weight, every agent contacting every other at the same rate. This is not a neutral choice made for mathematical convenience. It is the maximum entropy distribution over contact matrices subject to the constraint that each agent's total contact rate is fixed. When you know the average contact rate and nothing else about who contacts whom, $\bar{W}$ is what the principle of maximum ignorance requires. Every departure from $\bar{W}$ is information about contact structure beyond the mean.
#' 
#' Write $W = \bar{W} + \Delta W$. The constraint $W_{ij} \geq 0$ restricts $\Delta W_{ij} \geq -1/n$, which defines a convex feasible region. The ODE corresponds to $\Delta W = 0$. The two reparametrizations together give the unified statement: the ODE is the ABM under simultaneous regularization of agent heterogeneity toward $\bar\theta$ and contact structure toward $\bar{W}$. Formally, consider the objective
#' 
#' $$\mathcal{L}(\{\varepsilon_i\},\, \Delta W) \;+\; \lambda_\theta \sum_i \|\varepsilon_i\|^2 \;+\; \lambda_W \|\Delta W\|_F^2$$
#' 
#' As $\lambda_\theta \to \infty$ and $\lambda_W \to \infty$, the solution forces $\varepsilon_i \to 0$ and $\Delta W \to 0$, recovering the ODE exactly. At finite $\lambda$, the solution interpolates. The regularization strengths are hyperparameters: estimable by cross-validation when individual trajectories are observed, tunable by information criteria when only aggregate data are available. Both L2 and L1 penalties on $\Delta W$ are convex on the feasible region, so the structural regularization problem is convex throughout. The non-convexity in ABM calibration comes from the stochastic simulation mapping parameters to outputs, not from the geometry of the parameter space itself.
#' 
#' The choice between L2 and L1 on $\Delta W$ is a choice about what kind of network departure from mean-field is expected. L2 shrinks all edge weights uniformly toward $1/n$ — the contact structure becomes progressively closer to random mixing as $\lambda_W$ increases. L1 produces sparse $\Delta W$: most edges sit near their mean-field weight, a few carry substantial deviations. Sparse positive entries in $\Delta W$ are hubs — agents with far more contacts than the mean-field allocates. This is precisely the Barabási-Albert structure that drove the qualitatively different epidemic dynamics in Chapter 2. The L1 penalty on contact structure selects the epidemiologically consequential nodes, in exactly the sense that LASSO selects the consequential predictors. This is not an analogy. The mathematical operation is the same.
#' 
#' The Bayesian identity sharpens the argument further. A Laplace prior on $\Delta W_{ij}$ produces the L1 penalty at the posterior mode — exactly, not approximately, by the same algebra as for regression coefficients. A Gaussian prior produces L2. The maximum entropy interpretation completes the identification: a flat prior over contact matrices subject to fixed mean contact rate is a Gaussian centered at $\bar{W}$. Ridge regularization toward $\bar{W}$ is therefore the correct Bayesian update under maximum ignorance about contact structure, starting from a Gaussian prior. When the analyst has no structural knowledge of the contact network beyond its mean, L2 toward the mean-field is the principled choice. L1 is appropriate when the contact network is expected to be sparse — when most agent pairs have no meaningful contact and the consequential structure is concentrated in a few edges. The penalty encodes the prior whether or not the analyst names it as one.
#' 
#' The unification runs further still. L2 regularization is the limiting case of training on Gaussian-perturbed inputs: as the perturbation variance shrinks to zero, the expected loss over perturbed data converges to the L2-penalized loss. The distributionally robust optimizer minimizing worst-case expected loss over a Wasserstein ball around the empirical measure recovers regularized estimators as special cases. Bayesian prior, regularization penalty, adversarial perturbation budget — three framings of the same restriction. The literature on mean-field games formalizes the reparametrization argument at infinite scale: as $n \to \infty$ with individual effects shrinking proportionally, the discrete agent system converges to a continuum PDE — the $\lambda \to \infty$ limit of the regularization path, studied with the tools of optimal control.
#' 
## ----ch4-regularization-geometry, fig.cap="Geometry of regularization. Coefficient contours (ellipses) intersect the constraint region at the regularized solution. L2 (ridge) penalty produces a circular constraint region; the solution is generally interior with all coefficients shrunk toward zero. L1 (LASSO) penalty produces a diamond constraint region; the solution tends to land on a corner, producing sparse solutions with some coefficients exactly zero."----
# Visualize the geometry of L1 vs L2 regularization
# The plot shows coefficient contours and constraint regions

# RSS contours centered at OLS estimate (beta1=1.5, beta2=2.0 for illustration)
beta1_ols <- 1.5
beta2_ols <- 2.0

# Grid for contour computation
b1 <- seq(-2.5, 3, length.out = 300)
b2 <- seq(-1.5, 3.5, length.out = 300)
grid_reg <- expand.grid(b1 = b1, b2 = b2) %>% as_tibble()

# RSS surface (simplified: circular contours scaled from OLS)
# Using anisotropic ellipses to reflect correlated predictors
grid_reg <- grid_reg %>%
  mutate(
    rss = 0.7 * (b1 - beta1_ols)^2 +
          0.4 * (b2 - beta2_ols)^2 +
          0.3 * (b1 - beta1_ols) * (b2 - beta2_ols)
  )

# L2 constraint region: circle of radius r
r_l2 <- 1.2
constraint_l2 <- tibble(
  angle = seq(0, 2*pi, length.out = 300),
  b1    = r_l2 * cos(angle),
  b2    = r_l2 * sin(angle)
)

# L1 constraint region: diamond |b1| + |b2| <= r
r_l1 <- 1.2
constraint_l1 <- tibble(
  b1 = c(r_l1, 0, -r_l1, 0, r_l1),
  b2 = c(0, r_l1, 0, -r_l1, 0)
)

# Ridge solution: project OLS onto L2 ball (simple approximation)
scale_factor_l2 <- r_l2 / sqrt(beta1_ols^2 + beta2_ols^2)
ridge_soln <- tibble(b1 = beta1_ols * scale_factor_l2,
                     b2 = beta2_ols * scale_factor_l2,
                     label = "Ridge solution")

# LASSO solution: tends toward axis (corner of diamond)
# Approximate: project onto nearest corner
if (abs(beta1_ols) > abs(beta2_ols)) {
  lasso_soln <- tibble(b1 = r_l1, b2 = 0, label = "LASSO solution")
} else {
  lasso_soln <- tibble(b1 = 0, b2 = r_l1, label = "LASSO solution")
}

p_l2 <- ggplot() +
  geom_contour(data = grid_reg, aes(x = b1, y = b2, z = rss),
               color = "#AAAAAA", breaks = c(0.5, 1.5, 3.0, 5.5, 9.0)) +
  geom_path(data = constraint_l2, aes(x = b1, y = b2),
            color = "#2D5F8A", linewidth = 1.2) +
  geom_point(aes(x = beta1_ols, y = beta2_ols), color = "#1C1C1E", size = 2.5) +
  geom_point(data = ridge_soln, aes(x = b1, y = b2),
             color = "#2D5F8A", size = 3.5, shape = 18) +
  geom_segment(aes(x = beta1_ols, y = beta2_ols,
                   xend = ridge_soln$b1, yend = ridge_soln$b2),
               arrow = arrow(length = unit(0.2, "cm")), color = "#555555") +
  annotate("text", x = beta1_ols + 0.15, y = beta2_ols + 0.1,
           label = "OLS", family = "Source Serif 4", size = 3.2) +
  labs(title = "L2 (Ridge)",
       subtitle = "Circular constraint; solution shrinks toward origin",
       x = expression(beta[1]), y = expression(beta[2])) +
  coord_equal(xlim = c(-2.2, 2.8), ylim = c(-1.2, 3.0))

p_l1 <- ggplot() +
  geom_contour(data = grid_reg, aes(x = b1, y = b2, z = rss),
               color = "#AAAAAA", breaks = c(0.5, 1.5, 3.0, 5.5, 9.0)) +
  geom_polygon(data = constraint_l1, aes(x = b1, y = b2),
               fill = NA, color = "#8A4A2D", linewidth = 1.2) +
  geom_point(aes(x = beta1_ols, y = beta2_ols), color = "#1C1C1E", size = 2.5) +
  geom_point(data = lasso_soln, aes(x = b1, y = b2),
             color = "#8A4A2D", size = 3.5, shape = 18) +
  geom_segment(aes(x = beta1_ols, y = beta2_ols,
                   xend = lasso_soln$b1, yend = lasso_soln$b2),
               arrow = arrow(length = unit(0.2, "cm")), color = "#555555") +
  annotate("text", x = beta1_ols + 0.15, y = beta2_ols + 0.1,
           label = "OLS", family = "Source Serif 4", size = 3.2) +
  labs(title = "L1 (LASSO)",
       subtitle = "Diamond constraint; solution tends to corners (sparsity)",
       x = expression(beta[1]), y = expression(beta[2])) +
  coord_equal(xlim = c(-2.2, 2.8), ylim = c(-1.2, 3.0))

p_l2 + p_l1

#' 
#' Every model involves two levels of restriction, and the reparametrization makes both legible. The first is the parametric family — the structural commitment that determines what phenomena are in scope. The mean-field ODE is a family: it fixes $\varepsilon_i = 0$ and $\Delta W = 0$ before any data are seen, admitting mass-action interactions in continuous time and excluding network structure, spatial heterogeneity, and discrete individual effects entirely. The mean-sufficient model of industry dynamics is a family: it admits dynamics driven by aggregate employment and excludes the distributional heterogeneity that generates the power law tail. The stationary time series is a family: it admits processes whose spectral structure is constant across time and excludes regime transitions. These are not parameter choices. They are structural commitments that determine what the model can say, and no quantity of data revises them — they are the grammar within which the data are interpreted.
#' 
#' The second level is the prior on that family — the regularization, calibrated to what the data at a particular observation frequency can support. The choice of penalty is a choice of prior, and the prior encodes beliefs about how far the true system sits from the family's anchor point: how heterogeneous the agents are, how far the contact network departs from maximum entropy, how sparse the consequential deviations are. The ABM critic who objects to parameter proliferation is demanding stronger regularization within a richer family — legitimate when the observation frequency cannot distinguish the parameters, illegitimate when the phenomenon requires the richer family to exist at all. Both levels interact with observation frequency: the family must be appropriate at the resolution of measurement, and the prior on the family must be calibrated to what that resolution can support. When the phenomenon is the mean-field limit of large-population randomly-mixed dynamics, the ODE is correct and additional expressiveness contributes nothing. When the phenomenon is the shape of the firm size distribution, or the topology-dependence of an epidemic, the restricted family does not fit poorly — it excludes the phenomenon by design, before estimation begins.
#' 
#' ## Temporal Resolution and Near-Decomposability {#sec-temporal}
#' 
#' All data is time-indexed. The firm size distribution has a year attached to it. The epidemic curve begins at a date. The regression coefficients are estimated from observations generated over a period. Sometimes the time label is displayed; sometimes it is known and suppressed; sometimes it is genuinely unknown. But the time structure is always present in the data-generating process, and every choice — of parametric family, of prior on that family — is a choice about how to handle it. The parametric family determines which temporal structures are expressible. The prior on the family determines what is estimable at the available observation frequency. Making these choices explicit, rather than allowing them to hide in the model's functional form, is the discipline the framework demands.
#' 
#' Herbert Simon observed that complex adaptive systems tend to be nearly decomposable: subsystems interact tightly at fast time scales and loosely at slow ones. A firm coordinates its internal operations at time scales too fast for prices to clear across firms. The economy coordinates across firms at time scales over which transaction costs can be overcome. Each level of organization has its characteristic frequency. The levels are not isolated from each other, but they are not fully coupled either — the fast dynamics within each level are relatively independent of the slow dynamics between levels.
#' 
#' A model fit at a particular temporal resolution is a low-pass filter. It recovers structure at frequencies below the resolution's Nyquist limit. Structure at faster frequencies is averaged out — not represented as noise, but absorbed into the model's coefficients where it leaves no fingerprint. A quarterly macroeconomic model recovers annual and multi-year dynamics. It is linguistically incapable of expressing sub-quarterly phenomena: the model has no time steps at which to represent them. This is not a data limitation in the sense that better data collection would fix it. It is a resolution choice, and the resolution choice has consequences for what the model can say.
#' 
#' Stationarity is the temporal counterpart of the mean-field assumption — not analogously but structurally, because both are parametric family choices. The mean-field family averages over spatial heterogeneity within each time step, asserting that contact structure is uniform across all pairs of agents. The stationary family averages over temporal heterogeneity across the observation window, asserting that the data-generating process at time $t$ is drawn from the same distribution as at time $t - k$ for all $k$. One suppresses spatial structure; the other suppresses temporal structure. Both determine what the model can express, and both interact with observation frequency in the same way: the family is appropriate when the dimension being averaged over is genuinely negligible at the relevant resolution, and misspecified when it is not.
#' 
#' Sampling frequency determines which family choices are testable from the available data. A model estimated at weekly frequency cannot detect dynamics faster than two weeks — the Nyquist limit is a hard constraint on what signal is recoverable at that resolution. The stationary family chosen at weekly resolution is a commitment only about weekly-and-slower structure. Whether the system is stationary at that resolution is a distinct question from whether it is stationary at finer resolutions, and the two questions have different answers whenever the system is nearly decomposable. When the system is capable of bifurcation, the stationary family is misspecified in exactly the way the mean-field family is misspecified when contact structure matters: the family does not have the vocabulary for what is about to happen.
#' 
## ----ch4-logistic-extrapolation, fig.cap="The extrapolation trap. A logistic growth process observed only in its early exponential phase (solid black points, t $\\leq$ 10) yields an exponential fit (dashed red) that projects unbounded growth. The true trajectory (solid blue) reaches a carrying capacity that is invisible from the early data. The turn is not outside the confidence interval of the fit — it is outside the language of the model fitted to the early data."----
set.seed(2025)  # seed for reproducibility

K  <- 1000   # carrying capacity
r  <- 0.3    # intrinsic growth rate
N0 <- 10     # initial population

t_full <- seq(0, 50, by = 0.5)
N_true <- K / (1 + ((K - N0) / N0) * exp(-r * t_full))

logistic_df <- tibble(t = t_full, N = N_true)

# Observed data: only the exponential phase (t <= 10)
obs_df <- filter(logistic_df, t <= 10)

# Fit a linear model to log(N) ~ t on the observed data
# This corresponds to fitting an exponential model
lm_fit  <- lm(log(N) ~ t, data = obs_df)
r_hat   <- coef(lm_fit)["t"]
N0_hat  <- exp(coef(lm_fit)["(Intercept)"])

# Extrapolate using the fitted exponential
pred_df <- tibble(t = seq(0, 50, by = 0.5)) %>%
  mutate(N_pred = N0_hat * exp(r_hat * t))

ggplot() +
  # True logistic trajectory
  geom_line(data = logistic_df, aes(x = t, y = N),
            color = "#2D5F8A", linewidth = 1.0) +
  # Exponential extrapolation
  geom_line(data = pred_df, aes(x = t, y = N_pred),
            color = "#C0392B", linewidth = 0.9, linetype = "dashed") +
  # Observed data points
  geom_point(data = obs_df, aes(x = t, y = N),
             color = "#1C1C1E", size = 2.0) +
  # Carrying capacity reference
  geom_hline(yintercept = K, color = "#888888", linetype = "dotted") +
  annotate("text", x = 42, y = K + 30, label = paste0("K = ", K),
           family = "Source Serif 4", size = 3.5, color = "#888888") +
  annotate("text", x = 12, y = 30,
           label = "Observations (t \u2264 10)", size = 3.2,
           family = "Source Serif 4", color = "#1C1C1E") +
  annotate("segment", x = 10, xend = 10, y = 0, yend = K + 50,
           linetype = "dotted", color = "#888888") +
  scale_y_continuous(labels = scales::comma, limits = c(0, 1600)) +
  labs(
    title    = "Logistic growth: what extrapolation cannot see",
    subtitle = "Blue: true trajectory. Red dashed: exponential fit to early data. The carrying capacity is invisible from observations in the exponential phase.",
    x        = "Time",
    y        = "Population"
  )

#' 
#' The logistic example is the stripped-down version of a general problem. Any system capable of bifurcation will defeat stationarity-based extrapolation at the bifurcation point. The early-phase data are generated by one dynamical regime; the post-bifurcation data are generated by another. A model fitted to the early-phase data has learned the parameters of a regime that no longer exists. This is not a statistical point about forecast intervals being too narrow. It is a structural point about the limits of inductive inference from time series in nonlinear systems. The carrying capacity is not a parameter that the exponential model can estimate with more data. It is a parameter that the exponential model does not have.
#' 
## ----ch4-bifurcation, fig.cap="Bifurcation diagram of the logistic map $x_{t+1} = r x_t(1 - x_t)$. For $r < 3$, the system converges to a single fixed point. At $r = 3$, a period-doubling bifurcation occurs. By $r \\approx 3.57$, the system enters chaos. A time series model fitted to data from $r = 2.8$ has no representation for the behavior at $r = 3.5$: the bifurcation is exterior to the language of the stationary model."----
set.seed(2025)  # reproducibility

# Logistic map attractor: iterate the map and collect long-run values
map_attractor <- function(r_val, n_burn = 500, n_keep = 200) {
  # n_burn: transient steps discarded before collecting
  # n_keep: steps collected for plotting
  x <- 0.5  # initial condition (interior of [0,1])
  for (i in seq_len(n_burn)) x <- r_val * x * (1 - x)
  xs <- numeric(n_keep)
  for (i in seq_len(n_keep)) {
    x    <- r_val * x * (1 - x)
    xs[i] <- x
  }
  tibble(r = r_val, x = xs)
}

# Sweep over r values
bif_df <- map_dfr(
  seq(2.5, 4.0, length.out = 1200),
  map_attractor,
  n_burn = 500,
  n_keep = 150
)

ggplot(bif_df, aes(x = r, y = x)) +
  geom_point(size = 0.05, alpha = 0.3, color = "#2D5F8A") +
  geom_vline(xintercept = 3.0, linetype = "dashed", color = "#C0392B", linewidth = 0.6) +
  geom_vline(xintercept = 3.449, linetype = "dashed", color = "#8A4A2D",
             linewidth = 0.6, alpha = 0.7) +
  annotate("text", x = 3.02, y = 0.95, label = "Period 2",
           family = "Source Serif 4", size = 3.0, color = "#C0392B", hjust = 0) +
  annotate("text", x = 3.47, y = 0.95, label = "Period 4",
           family = "Source Serif 4", size = 3.0, color = "#8A4A2D", hjust = 0) +
  labs(
    title    = "Bifurcation diagram: logistic map",
    subtitle = "Long-run attractor values as the growth parameter r increases. The turn into chaos cannot be anticipated from pre-bifurcation data.",
    x        = "Growth parameter (r)",
    y        = "Long-run attractor value (x)"
  )

#' 
#' A time series model calibrated on data from the stable fixed-point regime ($r < 3$) has learned a stationary process. At $r = 3$, the system bifurcates to a period-two cycle. The stationary model has no representation for this — its language contains the word "stationary process" but not the word "bifurcation." The turn is not an outlier that the model underestimates. It is a structural change that moves the system outside the language the model was built in.
#' 
#' ## The Epistemology of Turning Points {#sec-turning-points}
#' 
#' Any trend extrapolated far enough produces an absurdity. A bond that compounds at five percent for long enough exceeds global GDP. A population growing at one percent doubles in seventy years and doubles again, until someone notices that the land does not. These projections are not errors of arithmetic. They are accurate outputs of models whose language contains no word for the ceiling. The problem is not extreme extrapolation. It is that any model built from data within a single phase of a cyclical system is, by construction, a model of that phase — and a phase, extrapolated, is infinite.
#' 
#' The observation that human affairs cycle rather than trend is old enough that its antiquity should give pause. Polybius, writing after Carthage, described anacyclosis: the sequence from monarchy through tyranny, from aristocracy through oligarchy, from democracy through mob rule and back to the need for monarchy. The form is not random deterioration but a structured progression, each stage generating the conditions that produce the next. The Platonic version is more pessimistic: the sequence runs in one direction, each form a degradation of the last. What both share, and what matters here, is the structure of the claim. Stability is not neutral. It is generative — of the conditions that will end it.
#' 
#' Ibn Khaldun, writing in the fourteenth century, gave this intuition its first formal treatment. Dynastic power rises on the strength of *asabiyyah* — group solidarity, the capacity for collective action that desert tribes possess in greater abundance than the sedentary populations they displace. Success dilutes it. Urban comfort, internal competition, the purchase of mercenary armies in place of kin-based ones: the mechanisms by which a dynasty's own dominance dissolves its original advantage are detailed, not vague. The cycle has a period: Ibn Khaldun estimated roughly four generations, approximately a century. The model is not falsified by the collapse of any particular dynasty. It is confirmed, because the collapse is what the model predicts.
#' 
#' Nikolai Kondratiev, writing in the 1920s, found a similar structure in industrialized economies. Long waves of approximately fifty years — expansion followed by contraction — seemed to characterize the historical series of prices, output, and interest rates since the beginning of industrial capitalism. Kondratiev did not explain the waves; he identified them. Schumpeter supplied the mechanism. Each wave is driven by a cluster of general-purpose technologies — the steam engine, the railways, electrification, the internal combustion engine — that open new productive frontiers, stimulate investment and employment through the expansion phase, and eventually saturate. The carrying capacity of any particular technological wave is finite. The expansion ends not because demand runs out but because the opportunities the technology created have been realized. The language of the expansion phase, fitted to expansion data, has no parameter for saturation.
#' 
#' Hyman Minsky's version is the sharpest. Stability, he argued, breeds instability — not as metaphor but as mechanism. A period of financial calm licenses leveraged risk-taking: lenders relax standards, borrowers extend, asset prices rise to reflect the expectation that they will continue to rise. The longer the calm persists, the more the system's structure shifts toward fragility, because the calm is evidence — compelling, well-fitted evidence — that leverage is safe. A lender operating from a model calibrated on the calm period is not being irrational. The model is a good fit to the data it was fitted on. It simply has no representation for the regime change that the calm's own dynamics are producing. The Minsky moment, when it comes, is not an outlier the model underweighted. It is a structural change into a regime the model could not express.
#' 
#' Reinhart and Rogoff catalogued eight centuries of financial crises and found the same pattern preceding every one: confident expert opinion that the old rules no longer applied, that this cycle was different, that the leverage was sustainable. This is not irrationality. It is the predictable output of models fitted to pre-crisis data by agents who understood their models perfectly and whose models were right about everything they were capable of saying. The crisis is exterior to the language of the models used to not predict it.
#' 
## ----ch4-two-windows, fig.cap="Two observation windows on the same cycle produce opposite extrapolations, both confident, both wrong at the turning point. A linear trend fitted to the expansion phase (blue, left window) predicts continued growth. The same model structure fitted to the contraction phase (red, right window) predicts continued decline. The turn between phases is precisely what neither model can anticipate: it is exterior to the language of any model fitted entirely within one phase. The structure is stylized after Kondratiev long waves, with a period of fifty time units."----
set.seed(2025)  # reproducibility

# Stylized long-wave cycle: sustained oscillation around a stable mean.
# Period of 50 units, echoing Kondratiev's empirical estimate.
t_full    <- seq(0, 130, by = 0.5)
baseline  <- 100
amplitude <- 35
period    <- 50
signal    <- baseline + amplitude * sin(2 * pi * t_full / period)

# Add a small amount of noise to represent real data
noise     <- rnorm(length(t_full), 0, 2)
cycle_df  <- tibble(t = t_full, true = signal, observed = signal + noise)

# Window A: expansion phase — the upswing (t ∈ [5, 20])
win_a <- filter(cycle_df, t >= 5, t <= 20)
fit_a <- lm(observed ~ t, data = win_a)

# Window B: contraction phase — the downswing (t ∈ [32, 47])
win_b <- filter(cycle_df, t >= 32, t <= 47)
fit_b <- lm(observed ~ t, data = win_b)

# Extrapolate each fit forward to t = 75
extrap_t <- seq(5, 75, by = 0.5)
extrap_df <- bind_rows(
  tibble(t = extrap_t, fit = predict(fit_a, tibble(t = extrap_t)), window = "Expansion fit"),
  tibble(t = extrap_t, fit = predict(fit_b, tibble(t = extrap_t)), window = "Contraction fit")
)

# Shade the observation windows
shade_a <- tibble(xmin = 5,  xmax = 20, ymin = 40, ymax = 165)
shade_b <- tibble(xmin = 32, xmax = 47, ymin = 40, ymax = 165)

ggplot() +
  # Shaded observation windows
  geom_rect(data = shade_a, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#2D5F8A", alpha = 0.08) +
  geom_rect(data = shade_b, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = "#C0392B", alpha = 0.08) +
  # True cycle (shown after observation period as dotted)
  geom_line(data = filter(cycle_df, t <= 75),
            aes(x = t, y = true),
            color = "#555555", linewidth = 0.8, linetype = "dotted") +
  # Observed data points in each window
  geom_point(data = win_a, aes(x = t, y = observed),
             color = "#2D5F8A", size = 1.5, alpha = 0.8) +
  geom_point(data = win_b, aes(x = t, y = observed),
             color = "#C0392B", size = 1.5, alpha = 0.8) +
  # Extrapolations
  geom_line(data = extrap_df,
            aes(x = t, y = fit, color = window, linetype = window),
            linewidth = 0.85) +
  scale_color_manual(
    values = c("Expansion fit" = "#2D5F8A", "Contraction fit" = "#C0392B")
  ) +
  scale_linetype_manual(
    values = c("Expansion fit" = "solid", "Contraction fit" = "solid")
  ) +
  annotate("text", x = 12.5, y = 155, label = "Window A\n(expansion)",
           family = "Source Serif 4", size = 3.0, color = "#2D5F8A") +
  annotate("text", x = 39.5, y = 155, label = "Window B\n(contraction)",
           family = "Source Serif 4", size = 3.0, color = "#C0392B") +
  coord_cartesian(ylim = c(45, 165)) +
  labs(
    title    = "Two windows, two extrapolations",
    subtitle = "Dotted: true cycle. Shaded: observation windows. Solid lines: linear trend extrapolations from each window.",
    x        = "Time",
    y        = "Index",
    color    = NULL,
    linetype = NULL
  ) +
  theme(legend.position = "bottom")

#' 
#' The two extrapolations in that figure are not bad statistical practice. Each is the correct output of a model well-fitted to its observation window. The residuals are small, the fit is good, the extrapolation is what the model implies. The models are wrong about the future not because they are poorly estimated but because they are built in a language that has no representation for the phase transition. More data from the same window would give tighter confidence bands and a more confident wrong answer.
#' 
#' This is the genealogy of the stationarity problem. Stationarity is not an assumption about noise — it is an assumption about regime. It asserts that the data-generating process visible in the observation window persists. When the system has a cyclical structure, this assertion is always temporarily true and permanently misleading: it is true within any phase long enough to fit a model, and it predicts the wrong turn every time. The model inherits nothing from its predecessors across the phase boundary because the phase boundary is precisely what the model cannot see.
#' 
#' The Kondratiev observation, the Minsky mechanism, the Ibn Khaldun genealogy of dynasties — these are all claims that the most important events in the system's history are phase transitions, and that models fitted within a phase are, by construction, blind to them. This is not a counsel of despair about time series modeling. It is a demand for precision about what time series models can and cannot say, and therefore about which questions they can and cannot answer. A model that fits the expansion phase fits the expansion phase. The question of whether the expansion will continue is a question about the regime structure — about where in the cycle the system currently sits, and what conditions govern the transition — and it is exterior to the language of any model that takes the expansion phase as its universe.
#' 
#' ## The Possibility Space {#sec-possibility}
#' 
#' The agent-based model does not produce better point forecasts. It is not a superior extrapolation engine. A model that represents the full heterogeneity of a firm population does not thereby know where the economy will be in ten years with more precision than a model tracking mean firm size does. Forecasting is not the point.
#' 
#' What the ABM offers is a map of the possibility space — the range of qualitatively distinct behaviors the system is capable of producing, the conditions under which each is realized, and the locations of the boundaries between them. These boundaries are not forecast errors. They are structural features of the system's dynamics. A map that shows them is a different kind of knowledge than a point forecast, and in many circumstances it is more useful.
#' 
#' Consider the contrast. A policy advisor working from a single trajectory — "the economy will grow at 2.3% next year, inflation will be 3.1%" — is navigating with a point. The point may be calibrated well or poorly; either way, it is a point. A policy advisor working from a possibility space map knows which dynamical regimes are accessible from the current state, what conditions would trigger a transition between them, and which interventions have leverage at the critical points. The map does not eliminate uncertainty. It characterizes it structurally, rather than reporting a number that suppresses the structure by design.
#' 
## ----ch4-possibility-space, fig.cap="Multiple trajectories from the same initial condition. A stochastic system starting from the same initial state can realize qualitatively different outcomes. The cloud of trajectories is the possibility space — not a confidence interval around a single forecast, but a map of qualitatively distinct futures the system can reach. The distinctions that matter are often between qualitative regimes, not between nearby trajectories within the same regime."----
set.seed(2025)  # reproducibility

# Stochastic logistic growth: same starting point, different trajectories
# Stochasticity comes from demographic noise (discreteness of individuals)
r_growth <- 0.3
K_cap    <- 500
N_start  <- 20
T_traj   <- 60
n_traj   <- 30

run_stochastic_logistic <- function(seed, N0, r, K, T) {
  set.seed(seed)
  N <- N0
  traj <- numeric(T + 1)
  traj[1] <- N

  for (tt in 2:(T + 1)) {
    # Expected growth rate from logistic equation
    expected_growth <- r * N * (1 - N / K)
    # Add demographic stochasticity: births and deaths are Poisson draws
    births <- rpois(1, max(0, expected_growth))
    deaths <- rpois(1, max(0, -expected_growth + r * N^2 / K))
    N      <- max(0, N + births - deaths)
    traj[tt] <- N
  }

  tibble(t = 0:T, N = traj, seed = seed)
}

trajectories_df <- map_dfr(1:n_traj, run_stochastic_logistic,
                            N0 = N_start, r = r_growth, K = K_cap, T = T_traj)

# Deterministic solution for reference
t_det <- seq(0, T_traj, by = 0.5)
N_det <- K_cap / (1 + ((K_cap - N_start) / N_start) * exp(-r_growth * t_det))
det_df <- tibble(t = t_det, N = N_det)

ggplot() +
  geom_line(
    data = trajectories_df,
    aes(x = t, y = N, group = seed),
    alpha = 0.2, linewidth = 0.4, color = "#2D5F8A"
  ) +
  geom_line(
    data = det_df,
    aes(x = t, y = N),
    color = "#1C1C1E", linewidth = 1.1
  ) +
  geom_hline(yintercept = K_cap, linetype = "dotted", color = "#888888") +
  annotate("text", x = T_traj - 3, y = K_cap + 18,
           label = paste0("K = ", K_cap), family = "Source Serif 4",
           size = 3.2, color = "#888888") +
  labs(
    title    = "Possibility space: stochastic logistic growth",
    subtitle = paste0(n_traj, " trajectories from the same initial condition (N\u2080 = ", N_start, "). ",
                      "Solid: deterministic trajectory."),
    x        = "Time",
    y        = "Population"
  )

#' 
#' This is what honest cartography looks like. Not "the population will be $K$ at time $T$" but "these are the trajectories accessible from this starting point, these are the conditions that push toward the upper envelope, and extinction is accessible when the population falls below a certain threshold from which recovery becomes unlikely." The map and the point forecast are not competing forecasts of the same thing. They are answers to different questions — and the question the map answers is often the more useful one.
#' 
#' ## The Statistician's Checklist {#sec-checklist}
#' 
#' The statistician's instinct toward honest uncertainty quantification is correct. Confidence intervals, posterior distributions, prediction intervals, cross-validation — these are all ways of refusing to report false precision. They are formal structures for saying "I do not know exactly, and here is what I do know about how much I do not know." This course has argued that the same instinct should extend from parameter uncertainty to structural uncertainty: not only "what are the plausible values of this parameter?" but "what class of data-generating procedures is consistent with what I observe, and what does each imply for the question I am asking?"
#' 
#' This reframe matters. A model is not merely a fitting device. It is a commitment to a class of mechanisms — a claim, made before data are consulted, about the space of DGPs that could have produced the observations. The ODE, equivalently the system dynamics model in the computational social science tradition, is a stock-and-flow structure whose flow equations embed the mean-field assumption: the same differential equations, whether written in mathematical notation or drawn as a Forrester diagram, impose $\varepsilon_i = 0$ and $\Delta W = 0$ before estimation begins. The ABM relaxes those constraints and thereby admits a broader class of DGPs. The apparent competition between system dynamics and agent-based modeling in that literature is the regularization tradeoff made methodological: it is the question of how strongly to constrain the mechanism space given what the data can support, not a disagreement about the underlying reality.
#' 
#' The checklist has two questions, which are two aspects of the same demand for honesty about constraints.
#' 
#' *At what resolution is this model working, and is the phenomenon I care about expressible at that resolution?* Stationarity constrains temporal variation to zero across the observation window — the Nyquist limit determines what frequencies are identifiable at all, and the stationarity prior zeros out variation within the identifiable band. A quarterly model cannot express sub-quarterly dynamics. A model fitted in the expansion phase of a long cycle cannot express the turn. When the phenomenon lives at a finer temporal scale, or requires a regime transition the model's prior forbids, additional data within the same resolution does not help: the constraint is structural, not statistical.
#' 
#' *What agent heterogeneity and contact structure has this model regularized to zero, and does the phenomenon require what was regularized away?* The mean-field ODE sets $\varepsilon_i = 0$ for all agents and $\Delta W = 0$ — it constrains every agent to the population mean and every contact pair to the maximum entropy rate. Hub-driven super-spreading lives in the sparse non-zero entries of $\Delta W$: it requires exactly the deviations from maximum entropy contact that the mean-field prior eliminates. The power law firm size distribution lives in the $\varepsilon_i$: it is a property of the full distribution of agent deviations, invisible to a model that has zeroed them out before estimation. The question is not whether the model fits poorly. It is whether the phenomenon exists within the model's feasible set at all.
#' 
#' These two questions share a structure: each asks whether the regularization the model applies — temporal, spatial, structural — is warranted for the phenomenon and the data. The answer depends on both simultaneously, and this is where the bias-variance tradeoff enters in two distinct senses that should not be conflated.
#' 
#' The first is the estimation sense. A richly parameterized ABM fitted to sparse data has high variance: small changes in the data produce large changes in the inferred heterogeneity and contact structure. Regularizing toward mean-field reduces that variance at the cost of bias — the estimates are stably wrong rather than noisily right. Under data sparsity, this is not a failure of the mean-field model. It is the correct conservative response: the model refuses to hallucinate structure the data cannot support, accepting systematic bias in exchange for stability. The ODE's implicit strong regularization is principled when the data is sparse in time or when the observation design cannot distinguish individual-level variation from population-level averages.
#' 
#' The second is the DGP-class sense. A narrow model family — mean-field, stationary, representative agent — has low uncertainty about which class of mechanism is operating, because it has committed strongly. But that commitment carries the risk of excluding the true DGP entirely. A wider family maintains higher uncertainty about the mechanism but lower risk of structural exclusion. These two senses interact but are not identical. A narrow family can have high estimation variance when data is sparse; a wide family can have low estimation variance when data is rich. The ODE advocate who objects to ABM parameter proliferation is speaking in the estimation sense — more parameters, higher variance. The response is in the DGP-class sense — those parameters describe mechanisms the narrow family cannot express, and whether the richer description is warranted depends on what the phenomenon requires and what the data can support. Both objection and response are correct within their own frame. The checklist resolves the apparent conflict by asking the prior question: does the phenomenon live inside the narrow family's feasible set? If it does, the narrow family's low-variance estimates are the right answer. If it does not, the narrow family's estimates are not low-variance approximations to the truth — they are precise answers to a different question.
#' 
#' Many phenomena are well-served by the ODE, the linear model, the stationary time series, the aggregate mean. Their implicit regularizations are warranted when the phenomena of interest sit well within their feasible sets — and many do. The checklist is not a case for ABM. It is a case for knowing which constraints you are applying and whether they are warranted for the phenomenon and the data jointly. When the answer is that the constraint zeros out exactly what the phenomenon requires, the path is not better estimation within the current model. It is a relaxation — a reduction in regularization strength, a richer family, a model that treats the zeroed-out structure as something to be inferred rather than assumed away. Name the constraint, identify what it zeros out, ask whether the phenomenon lives there. The rest follows from the answer.
#' 
#' The tools are harder. The proofs are less clean. The answers are less crisp. They are more likely to be true.
#' 
#' ---
#' 
#' ## References {-}
#' 
