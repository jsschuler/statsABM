# =============================================================================
# Agent-Based Modeling for Statisticians
# JSM Short Course — Chapter 1: The Differential Equations of Living Systems
#
# This script implements Lotka-Volterra and SIR models using deSolve and ggplot2.
# It is the student take-away companion to Chapter 1 of the course book.
#
# The central purpose is not to solve differential equations. It is to look at
# two classical models with fresh eyes — to see what assumptions are embedded
# in their structure, because those assumptions become the argument of the
# remaining three chapters.
#
# Author: John Lynham
# Course: Agent-Based Modeling for Statisticians, JSM 2025
# =============================================================================


# =============================================================================
# SETUP
# =============================================================================

library(tidyverse)   # data manipulation, ggplot2, pivot_longer, etc.
library(deSolve)     # numerical ODE solvers (lsoda by default)
library(patchwork)   # combining ggplot2 panels into composite figures


# Custom ggplot2 theme ---------------------------------------------------
# All plots in this course use a common theme for visual consistency.
# Source Serif 4 is the body font; Raleway is the heading font.
# If these fonts are not installed locally, substitute "serif" and "sans".

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


# Color palette ----------------------------------------------------------
# Named vector so colors are referenced by species/compartment, not position.
# This makes it easy to keep prey blue and predators rust throughout.

palette_ch1 <- c(
  prey        = "#2D5F8A",   # steel blue
  predator    = "#8A4A2D",   # warm rust
  susceptible = "#2D5F8A",   # same steel blue for S compartment
  infectious  = "#C0392B",   # red for I compartment
  recovered   = "#27AE60"    # green for R compartment
)


# =============================================================================
# SECTION 1.1: LOTKA-VOLTERRA
# =============================================================================
# The model encodes four verbal assumptions:
#   1. Prey grow exponentially absent predators.
#   2. Predators decline exponentially absent prey.
#   3. Encounters occur at a rate proportional to the *product* N*P.
#   4. Each encounter kills one prey and, with efficiency delta, produces one predator.
#
# The product term N*P is the first hint of what will later be named explicitly
# as the mean-field assumption: every predator is equally likely to encounter
# every prey individual at every moment.

# Parameters -------------------------------------------------------------
lv_params <- c(
  alpha = 0.8,   # per-capita prey birth rate (absent predators)
  beta  = 0.4,   # rate at which predator-prey encounters reduce prey
  delta = 0.3,   # efficiency: fraction of encounters that produce a new predator
  gamma = 0.6    # per-capita predator death rate (absent prey)
)

# Initial conditions: 10 prey (N), 5 predators (P)
lv_state <- c(N = 10, P = 5)


# ODE function -----------------------------------------------------------
# deSolve requires: function(t, state, params) returning a list of derivatives.

lv_ode <- function(t, state, params) {
  with(as.list(c(state, params)), {
    # dN/dt = alpha*N - beta*N*P
    # Prey grow at rate alpha; each encounter (rate beta*N*P) removes one prey.
    dN <- alpha * N - beta * N * P

    # dP/dt = delta*N*P - gamma*P
    # Predators gain from successful hunts (delta * encounters); die at rate gamma.
    dP <- delta * N * P - gamma * P

    list(c(dN, dP))   # deSolve expects derivatives as a list
  })
}


# Solve the ODE ----------------------------------------------------------
# seq() defines the output time grid. deSolve interpolates internally with
# adaptive step size; the grid here is purely for output resolution.

times_lv <- seq(0, 50, by = 0.1)   # 50 time units, output every 0.1

lv_out <- ode(
  y     = lv_state,    # initial conditions
  times = times_lv,
  func  = lv_ode,
  parms = lv_params
  # method defaults to "lsoda": stiff/non-stiff adaptive solver
)

# Convert the deSolve matrix to a tidy tibble for ggplot2.
# pivot_longer moves N and P from columns to rows, creating a "species" column.
lv_df <- as_tibble(as.data.frame(lv_out)) %>%
  pivot_longer(
    cols      = c(N, P),
    names_to  = "species",
    values_to = "population"
  ) %>%
  # Map the solver's variable names to the palette's named colors
  mutate(species = recode(species, N = "prey", P = "predator"))


# Time series plot -------------------------------------------------------
# Both species on shared axes. The neutral cycling is the key visual fact:
# the system oscillates indefinitely without damping or growth.

p_lv_time <- ggplot(lv_df, aes(x = time, y = population,
                                color = species, linetype = species)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = palette_ch1[c("prey", "predator")],
                     labels = c("Prey (N)", "Predator (P)")) +
  scale_linetype_manual(values = c("solid", "dashed"),
                        labels = c("Prey (N)", "Predator (P)")) +
  labs(
    title    = "Lotka-Volterra: Population Dynamics",
    subtitle = "Neutral cycles — a pendulum that never loses energy",
    x        = "Time",
    y        = "Population",
    color    = NULL,
    linetype = NULL
  )

print(p_lv_time)


# Phase portrait ---------------------------------------------------------
# The phase portrait plots N against P (eliminating time).
# Closed orbits confirm the neutral cycling: the system returns exactly
# to its starting point. Different initial conditions produce different
# orbits, all centered on the equilibrium (gamma/delta, alpha/beta).

# Equilibrium point: the intersection of the two nullclines.
# N-nullcline: P = alpha/beta (predator level where dN/dt = 0)
# P-nullcline: N = gamma/delta (prey level where dP/dt = 0)
lv_eq_N <- lv_params["gamma"] / lv_params["delta"]   # equilibrium prey
lv_eq_P <- lv_params["alpha"] / lv_params["beta"]    # equilibrium predator

# Several initial conditions spanning a range of orbit sizes.
# We loop over them and collect ODE solutions, labeling each orbit.
lv_ic_list <- list(
  c(N = 8,  P = 3),
  c(N = 10, P = 5),    # the base case from above
  c(N = 14, P = 7),
  c(N = 18, P = 4)
)

lv_orbits <- map_dfr(
  seq_along(lv_ic_list),
  function(i) {
    ic  <- lv_ic_list[[i]]
    out <- ode(y = ic, times = times_lv, func = lv_ode, parms = lv_params)
    as_tibble(as.data.frame(out)) %>%
      mutate(orbit = factor(i))   # label each orbit for color/group
  }
)

p_lv_phase <- ggplot(lv_orbits, aes(x = N, y = P, group = orbit, color = orbit)) +
  geom_path(linewidth = 0.7, alpha = 0.8) +
  # Mark the equilibrium point
  annotate("point", x = lv_eq_N, y = lv_eq_P, size = 3, shape = 21,
           fill = "#FAFAF7", color = "#1C1C1E", stroke = 1.2) +
  annotate("text", x = lv_eq_N + 0.4, y = lv_eq_P + 0.3,
           label = "Equilibrium", family = "Source Serif 4", size = 3.2) +
  scale_color_brewer(palette = "Blues", direction = 1) +
  labs(
    title    = "Lotka-Volterra: Phase Portrait",
    subtitle = "Closed orbits for four initial conditions",
    x        = "Prey (N)",
    y        = "Predator (P)",
    color    = "Orbit"
  ) +
  theme(legend.position = "none")   # orbits are visual; legend not needed

print(p_lv_phase)

# Combine into a two-panel figure
p_lv_combined <- p_lv_time + p_lv_phase +
  plot_annotation(
    title   = "Lotka-Volterra Predator-Prey System",
    caption = "ODE solution via deSolve (lsoda). Parameters: α=0.8, β=0.4, δ=0.3, γ=0.6"
  )

print(p_lv_combined)


# =============================================================================
# SECTION 1.2: THE MEAN-FIELD ASSUMPTION
# =============================================================================
# The product term β*N*P says something precise: every predator is equally
# likely to encounter every prey at every moment. The population is perfectly
# mixed. Space does not exist. Individuals have no locations, no persistent
# relationships, no neighborhoods.
#
# This is the mean-field assumption. It is not stated as an assumption in
# most presentations. It is embedded in the functional form. Naming it now
# is the setup for everything that follows in Chapters 2 and 3.
#
# (No code in this section — the prose carries the argument.)


# =============================================================================
# SECTION 1.3: SIR EPIDEMIC MODEL
# =============================================================================
# A closed population of N individuals. Each is Susceptible, Infectious,
# or Recovered (and immune). The mass-action contact term β*S*I is the
# mean-field assumption again: every infectious individual is equally likely
# to contact every susceptible at every moment.

# Parameters -------------------------------------------------------------
sir_params <- c(
  beta  = 0.3,   # transmission rate: rate of S->I per S-I pair per unit time
  gamma = 0.1    # recovery rate: per-capita rate at which I -> R
)

# Initial conditions: 990 susceptible, 10 infectious, 0 recovered
sir_state <- c(S = 990, I = 10, R = 0)
N_total   <- sum(sir_state)   # total population (conserved — no births or deaths)


# Compute R0 -------------------------------------------------------------
# R0 is the expected number of secondary infections from one case in a
# fully susceptible population. Derived from the sign of dI/dt at t=0:
#   dI/dt = (beta * S - gamma) * I
# The epidemic grows iff beta * S > gamma, i.e., beta * N / gamma > 1.
# R0 = beta * N / gamma is not a numerical fact to memorize — it is visible
# in the algebra.

R0 <- sir_params["beta"] * N_total / sir_params["gamma"]
cat("Basic reproduction number R0 =", round(R0, 2), "\n")
cat("Epidemic threshold: R0 > 1 means the epidemic grows initially.\n")


# ODE function -----------------------------------------------------------
sir_ode <- function(t, state, params) {
  with(as.list(c(state, params)), {
    # dS/dt: susceptibles leave at rate proportional to S*I encounters
    dS <- -beta * S * I

    # dI/dt: infectious pool grows from S*I contacts, shrinks at recovery rate gamma
    dI <-  beta * S * I - gamma * I

    # dR/dt: recovered accumulate at the recovery rate
    dR <-  gamma * I

    list(c(dS, dI, dR))
  })
}


# Solve the ODE ----------------------------------------------------------
times_sir <- seq(0, 160, by = 0.5)   # 160 time units, output every 0.5

sir_out <- ode(
  y     = sir_state,
  times = times_sir,
  func  = sir_ode,
  parms = sir_params
)

# Tidy the output for ggplot2
sir_df <- as_tibble(as.data.frame(sir_out)) %>%
  pivot_longer(
    cols      = c(S, I, R),
    names_to  = "compartment",
    values_to = "count"
  ) %>%
  mutate(
    # Lowercase for palette lookup
    compartment = recode(compartment,
                         S = "susceptible",
                         I = "infectious",
                         R = "recovered"),
    # Ordered factor for legend ordering
    compartment = factor(compartment,
                         levels = c("susceptible", "infectious", "recovered"))
  )


# SIR time series plot ---------------------------------------------------
p_sir_time <- ggplot(sir_df, aes(x = time, y = count,
                                  color = compartment, linetype = compartment)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = palette_ch1[c("susceptible", "infectious", "recovered")],
    labels = c("Susceptible (S)", "Infectious (I)", "Recovered (R)")
  ) +
  scale_linetype_manual(
    values = c("solid", "dashed", "dotted"),
    labels = c("Susceptible (S)", "Infectious (I)", "Recovered (R)")
  ) +
  labs(
    title    = "SIR Epidemic Model",
    subtitle = paste0("β = ", sir_params["beta"],
                      ", γ = ", sir_params["gamma"],
                      ", R₀ = ", round(R0, 2)),
    x        = "Time",
    y        = "Number of individuals",
    color    = NULL,
    linetype = NULL
  )

print(p_sir_time)


# Parameter sweep over R0 ------------------------------------------------
# Fixing gamma and N, vary beta to trace out different epidemic trajectories.
# R0 = beta * N / gamma, so beta = R0 * gamma / N.
# This shows how the epidemic threshold shapes qualitative dynamics —
# below R0 = 1 there is no epidemic; above it the epidemic size grows
# continuously with R0.

set.seed(42)   # seed before any stochastic draws; none here, but good practice

r0_values <- c(0.8, 1.2, 1.8, 2.5, 3.5)   # spanning sub- and super-threshold

# For each R0, compute the corresponding beta, solve the ODE, label the run.
sir_sweep_df <- map_dfr(r0_values, function(r0_val) {
  beta_val  <- r0_val * sir_params["gamma"] / N_total
  params_i  <- c(beta = beta_val, gamma = sir_params["gamma"])

  out <- ode(
    y     = sir_state,
    times = times_sir,
    func  = sir_ode,
    parms = params_i
  )

  as_tibble(as.data.frame(out)) %>%
    # Keep only the I compartment for the sweep plot
    select(time, I) %>%
    mutate(R0 = paste0("R₀ = ", r0_val))
})

# Factor R0 label in order for a coherent legend
sir_sweep_df <- sir_sweep_df %>%
  mutate(R0 = factor(R0, levels = paste0("R₀ = ", r0_values)))

p_sir_sweep <- ggplot(sir_sweep_df, aes(x = time, y = I,
                                         color = R0, group = R0)) +
  geom_line(linewidth = 0.9) +
  scale_color_brewer(palette = "RdYlBu", direction = -1) +
  labs(
    title    = "SIR Parameter Sweep: Effect of R₀",
    subtitle = "Infectious count over time for varying basic reproduction numbers",
    x        = "Time",
    y        = "Infectious (I)",
    color    = NULL
  ) +
  # Vertical reference: R0 = 1 threshold
  geom_hline(yintercept = 0, color = "#DCDCD4", linewidth = 0.5)

print(p_sir_sweep)


# =============================================================================
# SECTION 1.4: WHAT THE LANGUAGE CAN SAY
# =============================================================================
# A brief catalogue of the shared assumptions embedded in both models:
#
#   - Populations are continuous: no individuals, no discreteness.
#   - Populations are homogeneous: every individual is equivalent.
#   - Mixing is perfect and instantaneous: mass action throughout.
#   - Space does not exist.
#   - History does not matter: state at t fully determines state at t+dt.
#
# These are not flaws. The Lotka-Volterra and SIR models have been
# scientifically productive for over a century. The point is that they
# constitute a language, and every language has an expressive boundary.
# The phenomena beyond that boundary are the subject of Chapter 2.


# =============================================================================
# SUMMARY FIGURE: all four panels together
# =============================================================================

p_summary <- (p_lv_time | p_lv_phase) / (p_sir_time | p_sir_sweep) +
  plot_annotation(
    title   = "Chapter 1: ODE Foundations",
    caption = paste0(
      "Top: Lotka-Volterra predator-prey system. ",
      "Bottom: SIR epidemic model. ",
      "All solutions via deSolve (lsoda)."
    )
  )

print(p_summary)

# =============================================================================
# END OF CHAPTER 1
# =============================================================================
