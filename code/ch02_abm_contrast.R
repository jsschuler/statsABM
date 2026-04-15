# =============================================================================
# Agent-Based Modeling for Statisticians
# JSM Short Course — Chapter 2: Agents, Networks, and the Geometry of Interaction
#
# This script implements agent-based versions of Lotka-Volterra and SIR,
# demonstrates the reduction result (ABM converges to ODE under random mixing
# and large population), and shows what lies outside the ODE's language:
# extinction events, network topology effects, spatial structure.
#
# Section 2.5 (spatial Lotka-Volterra with gganimate) is marked as a
# self-study extension — it is not run live during the course session but
# is fully implemented here for students to explore.
#
# Author: John Lynham
# Course: Agent-Based Modeling for Statisticians, JSM 2025
# =============================================================================


# =============================================================================
# SETUP
# =============================================================================

library(tidyverse)    # data manipulation, ggplot2, map_dfr, etc.
library(deSolve)      # ODE solver for comparison / reduction result
library(igraph)       # network generation and graph algorithms
library(tidygraph)    # tidy (dplyr-style) API for igraph objects
library(ggraph)       # ggplot2-based network visualization
library(gganimate)    # animation (used in Section 2.5 only)
library(patchwork)    # compositing multiple ggplot2 panels


# Custom theme and palette (same as Chapter 1 — each chapter is self-contained)
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

palette_ch2 <- c(
  prey        = "#2D5F8A",   # steel blue
  predator    = "#8A4A2D",   # warm rust
  susceptible = "#2D5F8A",
  infectious  = "#C0392B",
  recovered   = "#27AE60"
)


# =============================================================================
# SECTION 2.2: AGENT-BASED LOTKA-VOLTERRA (NON-SPATIAL)
# =============================================================================
# An agent is an entity with a state, a set of update rules, and a neighborhood.
# Here the neighborhood is the entire population (random pairing) — which is
# precisely the mean-field assumption. The ODE is what this model becomes when
# you take that assumption seriously and let N -> infinity.

# --- Initialization --------------------------------------------------------
# The population is a tibble: one row per agent, with type and alive status.
# We track every agent by id so births and deaths are unambiguous.

init_lv_abm <- function(n_prey, n_pred) {
  bind_rows(
    tibble(
      id    = seq_len(n_prey),
      type  = "prey",
      alive = TRUE
    ),
    tibble(
      id    = seq_len(n_pred) + n_prey,   # ids continue past prey ids
      type  = "predator",
      alive = TRUE
    )
  )
}


# --- One ABM timestep ------------------------------------------------------
# Rules:
#   1. Each living prey reproduces with probability p_birth.
#   2. Random predator-prey pairs are formed (min(n_prey, n_pred) pairs).
#      Each encounter kills the prey and produces a new predator with prob p_eat.
#   3. Each living predator dies with probability p_death.
#
# Parameters are calibrated to produce dynamics qualitatively similar to the
# ODE system in Chapter 1 (alpha ~ p_birth, gamma ~ p_death).

step_lv_abm <- function(pop,
                         p_birth = 0.15,    # prey reproduction probability per step
                         p_eat   = 0.20,    # predation success probability per encounter
                         p_death = 0.10) {  # predator natural death probability per step

  prey  <- filter(pop, type == "prey",     alive)
  preds <- filter(pop, type == "predator", alive)

  # -- Prey reproduction --
  # rbinom(1, n, p): draw one binomial to get the number of new prey born this step
  new_prey_n <- rbinom(1, nrow(prey), p_birth)
  max_id     <- max(pop$id, 0L)   # current maximum id, for generating new unique ids

  new_prey <- if (new_prey_n > 0) {
    tibble(
      id    = seq_len(new_prey_n) + max_id,
      type  = "prey",
      alive = TRUE
    )
  } else {
    tibble()   # empty tibble when no births occur
  }

  # -- Predator-prey encounters --
  # Number of encounters = min(prey alive, predators alive): each predator
  # can participate in at most one encounter per step, likewise each prey.
  n_enc         <- min(nrow(prey), nrow(preds))
  dead_prey_ids <- integer(0)
  new_pred_ids  <- integer(0)

  if (n_enc > 0) {
    # Random pairing: sample without replacement from each group
    prey_sample <- slice_sample(prey, n = n_enc)
    pred_sample <- slice_sample(preds, n = n_enc)   # not used by id, but for symmetry

    # Each encounter succeeds (kills prey, creates predator) with prob p_eat
    eaten <- rbinom(n_enc, 1, p_eat) == 1

    dead_prey_ids <- prey_sample$id[eaten]   # prey killed this step
    new_pred_n    <- sum(eaten)              # number of new predators born from hunts

    if (new_pred_n > 0) {
      # New id block: start after all existing ids plus any new prey just born
      max_id2      <- max(pop$id, 0L) + nrow(new_prey)
      new_pred_ids <- seq_len(new_pred_n) + max_id2
    }
  }

  # New predators from successful hunts
  new_preds <- if (length(new_pred_ids) > 0) {
    tibble(id = new_pred_ids, type = "predator", alive = TRUE)
  } else {
    tibble()
  }

  # -- Predator natural death --
  # Each predator alive this step dies independently with probability p_death
  dead_pred_ids <- preds$id[rbinom(nrow(preds), 1, p_death) == 1]

  # -- Update population --
  # Mark killed and dead agents; keep only alive agents; append newborns.
  pop_updated <- pop %>%
    mutate(alive = case_when(
      id %in% dead_prey_ids ~ FALSE,   # hunted prey
      id %in% dead_pred_ids ~ FALSE,   # died predators
      TRUE                  ~ alive    # unchanged
    )) %>%
    filter(alive)   # drop dead agents to keep tibble from growing unboundedly

  bind_rows(pop_updated, new_prey, new_preds)
}


# --- Run the ABM for T steps -----------------------------------------------
# Returns a tibble with prey and predator counts at each timestep.

run_lv_abm <- function(T      = 200,
                        n_prey = 100,
                        n_pred = 20,
                        ...) {          # ... passes extra args to step_lv_abm

  pop    <- init_lv_abm(n_prey, n_pred)
  counts <- tibble(t = 0L, prey = n_prey, predator = n_pred)

  for (tt in seq_len(T)) {
    pop <- step_lv_abm(pop, ...)

    n_prey_t <- sum(pop$type == "prey")
    n_pred_t <- sum(pop$type == "predator")

    counts <- bind_rows(
      counts,
      tibble(t = tt, prey = n_prey_t, predator = n_pred_t)
    )

    # If the population goes extinct, stop early
    if (nrow(pop) == 0) break
  }

  counts
}


# =============================================================================
# SECTION 2.2a: THE REDUCTION RESULT
# =============================================================================
# Run multiple ABM replications and overlay on the ODE solution.
# The ODE is the ABM under the mean-field limit: random mixing, large N.
# With 500 initial agents the convergence is visible, though stochastic
# fluctuations remain.

set.seed(42)   # reproducibility; change to explore different stochastic paths

N_REPS       <- 10     # number of ABM replications
T_SIM        <- 200    # simulation steps
N_PREY_LARGE <- 200    # large enough for convergence to ODE to be visible
N_PRED_LARGE <- 40

# Run all replications, labeling each with a rep number
abm_reps_df <- map_dfr(seq_len(N_REPS), function(rep_i) {
  run_lv_abm(T = T_SIM, n_prey = N_PREY_LARGE, n_pred = N_PRED_LARGE) %>%
    mutate(rep = rep_i)
})

# Tidy: pivot prey and predator counts to long format for ggplot2
abm_long_df <- abm_reps_df %>%
  pivot_longer(cols = c(prey, predator),
               names_to  = "species",
               values_to = "count")


# -- ODE solution for comparison --
# Use the same time horizon; scale parameters to match ABM rates.
# Here we use the same Lotka-Volterra ODE from Chapter 1 with parameters
# chosen to be qualitatively comparable to the ABM's p_birth/p_eat/p_death.

lv_params_red <- c(alpha = 0.05, beta = 0.0008, delta = 0.0006, gamma = 0.10)

lv_ode_fn <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dN <- alpha * N - beta * N * P
    dP <- delta * N * P - gamma * P
    list(c(dN, dP))
  })
}

ode_out_red <- ode(
  y     = c(N = N_PREY_LARGE, P = N_PRED_LARGE),
  times = seq(0, T_SIM, by = 1),
  func  = lv_ode_fn,
  parms = lv_params_red
)

ode_red_df <- as_tibble(as.data.frame(ode_out_red)) %>%
  rename(prey = N, predator = P, t = time) %>%
  pivot_longer(cols = c(prey, predator),
               names_to  = "species",
               values_to = "count")


# -- Plot: ABM ribbons over ODE line --
# Translucent ABM runs as the "cloud" of stochastic paths;
# the ODE solution as the solid spine through their center.

p_reduction <- ggplot() +
  # Translucent individual ABM runs
  geom_line(
    data    = abm_long_df,
    mapping = aes(x = t, y = count, group = interaction(rep, species),
                  color = species),
    alpha   = 0.18,
    linewidth = 0.5
  ) +
  # ODE solution — solid, full opacity
  geom_line(
    data      = ode_red_df,
    mapping   = aes(x = t, y = count, color = species),
    linewidth = 1.4
  ) +
  scale_color_manual(
    values = c(prey = palette_ch2["prey"], predator = palette_ch2["predator"]),
    labels = c("Prey", "Predator")
  ) +
  labs(
    title    = "The Reduction Result",
    subtitle = paste0(N_REPS, " ABM runs (translucent) vs. ODE solution (solid). ",
                      "N₀ = ", N_PREY_LARGE, " prey, ", N_PRED_LARGE, " predators."),
    x        = "Timestep",
    y        = "Population",
    color    = NULL
  )

print(p_reduction)


# =============================================================================
# SECTION 2.2b: WHAT THE ODE CANNOT SEE — EXTINCTION
# =============================================================================
# With small initial populations, stochastic fluctuations can drive prey or
# predators to zero. The ODE orbits forever. The ABM can fall to zero and stay.
# The ODE's language has no word for this.

set.seed(123)   # different seed to explore extinction-prone trajectories

N_REPS_SMALL <- 15
N_PREY_SMALL <- 20
N_PRED_SMALL <- 5

abm_small_df <- map_dfr(seq_len(N_REPS_SMALL), function(rep_i) {
  run_lv_abm(T = T_SIM, n_prey = N_PREY_SMALL, n_pred = N_PRED_SMALL) %>%
    mutate(rep = rep_i)
})

# Identify which runs go extinct (prey count reaches 0)
extinction_flag <- abm_small_df %>%
  group_by(rep) %>%
  summarise(extinct = any(prey == 0), .groups = "drop")

abm_small_long <- abm_small_df %>%
  left_join(extinction_flag, by = "rep") %>%
  pivot_longer(cols = c(prey, predator),
               names_to  = "species",
               values_to = "count") %>%
  filter(species == "prey")   # show prey only for clarity

p_extinction <- ggplot(abm_small_long,
                       aes(x = t, y = count,
                           group = rep,
                           color = extinct,
                           alpha = extinct)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(
    values = c("FALSE" = palette_ch2["prey"],
               "TRUE"  = "#C0392B"),
    labels = c("FALSE" = "Survived",
               "TRUE"  = "Went extinct")
  ) +
  scale_alpha_manual(
    values = c("FALSE" = 0.4, "TRUE" = 0.85),
    guide  = "none"
  ) +
  labs(
    title    = "What the ODE Cannot See: Extinction Events",
    subtitle = paste0("Small population (N₀ = ", N_PREY_SMALL,
                      " prey). Red runs go extinct. The ODE orbits forever."),
    x        = "Timestep",
    y        = "Prey population",
    color    = NULL
  )

print(p_extinction)


# =============================================================================
# SECTION 2.3: AGENT-BASED SIR
# =============================================================================
# Two versions:
#   Version 1 — Random mixing: same mean-field structure as the ODE.
#               Shows convergence to ODE under large N.
#   Version 2 — Network SIR: infection travels along contact network edges.
#               Same R0, different topology, different epidemic.


# --- Version 1: Random mixing ABM SIR ------------------------------------
# Each agent has a state: S, I, or R.
# Each step: infectious agents contact a random sample of the population;
# contact transmits infection with probability p_infect.
# Infectious agents recover with probability p_recover per step.

set.seed(99)

# Initialize: returns a tibble with id and compartment
init_sir_abm <- function(N = 500, n_initial_infected = 5) {
  tibble(
    id    = seq_len(N),
    state = c(rep("I", n_initial_infected),    # first n_initial_infected are infected
              rep("S", N - n_initial_infected)) # remainder susceptible
  )
}

# One timestep of the random-mixing SIR ABM
step_sir_abm <- function(pop,
                          k_contacts  = 5,     # number of contacts each I makes per step
                          p_infect    = 0.06,  # probability transmission per S-I contact
                          p_recover   = 0.10)  { # probability recovery per I per step

  infected    <- filter(pop, state == "I")
  susceptible <- filter(pop, state == "S")

  # New infections: each I agent contacts k_contacts random agents,
  # and infects each susceptible contact with probability p_infect.
  newly_infected_ids <- integer(0)

  if (nrow(infected) > 0 && nrow(susceptible) > 0) {
    # For each infected agent, sample contacts from the full population
    # (mean-field: everyone is reachable from everyone)
    contacts_per_I <- map(infected$id, function(inf_id) {
      # Sample k_contacts from the full population (excluding self)
      sample(pop$id[pop$id != inf_id],
             size    = min(k_contacts, nrow(pop) - 1),
             replace = FALSE)
    })

    # Collect all contact ids that are susceptible
    all_contacts <- unlist(contacts_per_I)
    susceptible_contacts <- all_contacts[all_contacts %in% susceptible$id]

    # Each susceptible contact becomes infected with probability p_infect
    if (length(susceptible_contacts) > 0) {
      infected_flag      <- rbinom(length(susceptible_contacts), 1, p_infect) == 1
      newly_infected_ids <- unique(susceptible_contacts[infected_flag])
    }
  }

  # Recovery: each infected agent recovers independently with prob p_recover
  newly_recovered_ids <- infected$id[rbinom(nrow(infected), 1, p_recover) == 1]

  # Update states in order: recoveries first, then new infections
  # (using mutate preserves immutability — we're not doing double-counting)
  pop %>%
    mutate(state = case_when(
      id %in% newly_recovered_ids ~ "R",       # infected -> recovered
      id %in% newly_infected_ids  ~ "I",       # susceptible -> infected
      TRUE                        ~ state      # unchanged
    ))
}

# Run the random-mixing ABM for T steps, recording compartment counts
run_sir_abm_random <- function(T = 200, N = 500, n_initial = 5, ...) {
  pop    <- init_sir_abm(N, n_initial)
  counts <- tibble(
    t = 0L,
    S = sum(pop$state == "S"),
    I = sum(pop$state == "I"),
    R = sum(pop$state == "R")
  )

  for (tt in seq_len(T)) {
    pop    <- step_sir_abm(pop, ...)
    counts <- bind_rows(counts, tibble(
      t = tt,
      S = sum(pop$state == "S"),
      I = sum(pop$state == "I"),
      R = sum(pop$state == "R")
    ))

    # Stop early if epidemic has burned out
    if (sum(pop$state == "I") == 0) break
  }

  counts
}

# Run 5 ABM replications to show stochastic variation around ODE
set.seed(77)
sir_abm_reps <- map_dfr(1:5, function(rep_i) {
  run_sir_abm_random(
    T         = 200,
    N         = 500,
    n_initial = 5,
    k_contacts  = 5,
    p_infect    = 0.06,
    p_recover   = 0.10
  ) %>%
    mutate(rep = rep_i)
})

sir_abm_long <- sir_abm_reps %>%
  pivot_longer(c(S, I, R), names_to = "compartment", values_to = "count") %>%
  mutate(compartment = recode(compartment,
                               S = "susceptible",
                               I = "infectious",
                               R = "recovered"))

# ODE comparison for SIR (same Chapter 1 ODE, scaled to N=500)
sir_ode_fn <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}

# Parameters matched to ABM: effective beta = k_contacts * p_infect / N
# effective gamma = p_recover
sir_ode_params <- c(beta = 5 * 0.06 / 500, gamma = 0.10)
sir_ode_state  <- c(S = 495, I = 5, R = 0)

sir_ode_out <- ode(
  y     = sir_ode_state,
  times = seq(0, 200, by = 1),
  func  = sir_ode_fn,
  parms = sir_ode_params
)

sir_ode_df <- as_tibble(as.data.frame(sir_ode_out)) %>%
  rename(t = time) %>%
  pivot_longer(c(S, I, R), names_to = "compartment", values_to = "count") %>%
  mutate(compartment = recode(compartment,
                               S = "susceptible",
                               I = "infectious",
                               R = "recovered"))

# Plot: ABM runs (translucent) over ODE (solid)
p_sir_reduction <- ggplot() +
  geom_line(
    data    = sir_abm_long,
    mapping = aes(x = t, y = count,
                  group = interaction(rep, compartment),
                  color = compartment),
    alpha   = 0.25,
    linewidth = 0.5
  ) +
  geom_line(
    data      = sir_ode_df,
    mapping   = aes(x = t, y = count, color = compartment),
    linewidth = 1.3
  ) +
  scale_color_manual(
    values = palette_ch2[c("susceptible", "infectious", "recovered")],
    labels = c("Recovered (R)", "Infectious (I)", "Susceptible (S)")
  ) +
  labs(
    title    = "SIR Reduction Result: ABM Converges to ODE",
    subtitle = "5 stochastic ABM runs (translucent) vs. ODE solution (solid). N = 500.",
    x        = "Timestep",
    y        = "Count",
    color    = NULL
  )

print(p_sir_reduction)


# =============================================================================
# SECTION 2.3b: NETWORK SIR
# =============================================================================
# Give agents an address — a position in a contact network. Infection can
# only travel along edges. The same R0, implemented on two different network
# topologies, produces qualitatively different epidemics.
#
# Erdos-Renyi (ER): random graph, roughly equal degree for all nodes.
# Barabasi-Albert (BA): scale-free graph with hubs — a few nodes have
# very high degree; most have low degree. Both have mean degree ~ 4.

set.seed(314)   # for reproducible network structure

N_NET    <- 500   # number of agents / nodes
MEAN_DEG <- 4     # target mean degree for both networks

# -- Generate networks --
# Erdos-Renyi: each pair of nodes connected with probability p = mean_deg / (N-1)
g_er <- igraph::erdos.renyi.game(
  n    = N_NET,
  p    = MEAN_DEG / (N_NET - 1),
  type = "gnp"
)

# Barabasi-Albert: preferential attachment — new nodes connect to existing
# nodes proportional to their current degree, producing power-law degree distribution.
# The 'm' parameter is the number of edges each new node adds; mean degree ~ 2*m.
g_ba <- igraph::barabasi.game(
  n            = N_NET,
  m            = MEAN_DEG / 2,   # each new node adds MEAN_DEG/2 edges
  directed     = FALSE
)

cat("ER network: mean degree =",
    round(mean(igraph::degree(g_er)), 2), "\n")
cat("BA network: mean degree =",
    round(mean(igraph::degree(g_ba)), 2), "\n")


# -- Network SIR simulation function --
# Infection spreads only along network edges (not globally).
# At each step, each I node infects each S neighbor with prob p_infect.
# Each I node recovers with prob p_recover.

run_sir_network <- function(graph,
                             n_initial = 5,
                             p_infect  = 0.15,   # per-edge transmission probability
                             p_recover = 0.10,
                             T         = 150) {

  N     <- igraph::vcount(graph)
  # Initialize: randomly select n_initial nodes as initially infected
  init_infected <- sample(seq_len(N), n_initial)
  state <- rep("S", N)
  state[init_infected] <- "I"

  # Store snapshot data for plotting
  snapshots <- tibble(
    t     = 0L,
    S     = sum(state == "S"),
    I     = sum(state == "I"),
    R     = sum(state == "R"),
    state = list(state)   # store full state vector for network visualization
  )

  # Adjacency list: precompute neighbors for speed
  adj <- igraph::as_adj_list(graph, mode = "all")

  for (tt in seq_len(T)) {
    new_state <- state   # work on a copy to avoid order-dependent updates

    for (node_i in which(state == "I")) {
      # Attempt to infect each susceptible neighbor
      neighbors_i <- as.integer(adj[[node_i]])
      s_neighbors <- neighbors_i[state[neighbors_i] == "S"]

      if (length(s_neighbors) > 0) {
        infected_flag <- rbinom(length(s_neighbors), 1, p_infect) == 1
        new_state[s_neighbors[infected_flag]] <- "I"
      }

      # Attempt recovery
      if (rbinom(1, 1, p_recover) == 1) {
        new_state[node_i] <- "R"
      }
    }

    state <- new_state

    snapshots <- bind_rows(snapshots, tibble(
      t     = tt,
      S     = sum(state == "S"),
      I     = sum(state == "I"),
      R     = sum(state == "R"),
      state = list(state)
    ))

    if (sum(state == "I") == 0) break
  }

  snapshots
}

# Run both network SIRs
set.seed(42)
sir_er_out <- run_sir_network(g_er, n_initial = 5)
set.seed(42)   # same seed for comparable initial conditions
sir_ba_out <- run_sir_network(g_ba, n_initial = 5)

# Tidy for plotting
sir_net_df <- bind_rows(
  sir_er_out %>% select(t, S, I, R) %>% mutate(network = "Erdos-Renyi"),
  sir_ba_out %>% select(t, S, I, R) %>% mutate(network = "Barabasi-Albert")
) %>%
  pivot_longer(c(S, I, R), names_to = "compartment", values_to = "count") %>%
  mutate(
    compartment = recode(compartment,
                          S = "susceptible", I = "infectious", R = "recovered"),
    network     = factor(network, levels = c("Erdos-Renyi", "Barabasi-Albert"))
  )

# -- Epidemic curves by network topology --
p_net_curves <- ggplot(
  filter(sir_net_df, compartment == "infectious"),
  aes(x = t, y = count, color = network, linetype = network)
) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(
    values = c("Erdos-Renyi" = "#2D5F8A", "Barabasi-Albert" = "#8A4A2D")
  ) +
  scale_linetype_manual(
    values = c("Erdos-Renyi" = "solid", "Barabasi-Albert" = "dashed")
  ) +
  labs(
    title    = "Network SIR: Same R₀, Different Topology",
    subtitle = paste0("ER vs. BA scale-free network, both N = ", N_NET,
                      ", mean degree ≈ ", MEAN_DEG,
                      ". Infectious count only."),
    x        = "Timestep",
    y        = "Infectious (I)",
    color    = NULL,
    linetype = NULL
  )

print(p_net_curves)


# -- Network visualization at a key timestep --
# Show node compartments on the network at roughly the epidemic peak.
# The visual is the argument: topology shapes who gets infected and when.

# Find the timestep of epidemic peak in the BA run (highest I count)
peak_t_ba <- sir_ba_out$t[which.max(sir_ba_out$I)]

# Extract node states at peak
state_at_peak_er <- sir_er_out %>%
  filter(t == peak_t_ba) %>%
  pull(state) %>%
  `[[`(1)   # unlist the stored state vector

state_at_peak_ba <- sir_ba_out %>%
  filter(t == peak_t_ba) %>%
  pull(state) %>%
  `[[`(1)

# Build tidygraph objects with state as node attribute
tg_er <- as_tbl_graph(g_er) %>%
  mutate(compartment = state_at_peak_er)

tg_ba <- as_tbl_graph(g_ba) %>%
  mutate(compartment = state_at_peak_ba)

# ggraph visualization — use FR layout for readability
# (Fruchterman-Reingold: force-directed layout that respects community structure)
p_net_er <- ggraph(tg_er, layout = "fr") +
  geom_edge_link(alpha = 0.05, color = "#AAAAAA") +
  geom_node_point(aes(color = compartment), size = 1.2, alpha = 0.8) +
  scale_color_manual(
    values = c(S = palette_ch2["susceptible"],
               I = palette_ch2["infectious"],
               R = palette_ch2["recovered"]),
    labels = c(S = "Susceptible", I = "Infectious", R = "Recovered")
  ) +
  labs(title = paste0("Erdos-Renyi at t = ", peak_t_ba), color = NULL) +
  theme_graph(base_family = "Source Serif 4") +
  theme(plot.title = element_text(family = "Raleway", size = 12))

p_net_ba <- ggraph(tg_ba, layout = "fr") +
  geom_edge_link(alpha = 0.05, color = "#AAAAAA") +
  geom_node_point(aes(color = compartment), size = 1.2, alpha = 0.8) +
  scale_color_manual(
    values = c(S = palette_ch2["susceptible"],
               I = palette_ch2["infectious"],
               R = palette_ch2["recovered"]),
    labels = c(S = "Susceptible", I = "Infectious", R = "Recovered")
  ) +
  labs(title = paste0("Barabasi-Albert at t = ", peak_t_ba), color = NULL) +
  theme_graph(base_family = "Source Serif 4") +
  theme(plot.title = element_text(family = "Raleway", size = 12))

p_net_combined <- p_net_er + p_net_ba +
  plot_annotation(
    title   = "Network Structure at Epidemic Peak",
    caption = paste0("Same parameters. Same R₀. The question 'what does topology do?' ",
                     "cannot be asked in the ODE's language.")
  )

print(p_net_combined)


# =============================================================================
# SECTION 2.4: THE REDUCTION THEOREM (SUMMARY)
# =============================================================================
# As population size grows, as mixing becomes increasingly random, and as
# individual heterogeneity averages out, the ABM converges in distribution
# to the ODE. The ODE is not wrong — it is a limiting case.
#
# This is also the answer to the "parameter proliferation" objection:
# the nominal parameter space of an ABM may be large, but the effective
# dimensionality of the calibrated structure is low. Most parameter
# combinations produce qualitatively identical dynamics. The interesting
# behavior lives on a low-dimensional manifold. This is regularization
# in a sense that anyone who has used LASSO already understands.


# =============================================================================
# SECTION 2.5: SPATIAL LOTKA-VOLTERRA (SELF-STUDY EXTENSION)
# =============================================================================
# NOTE: This section is NOT run during the live course session.
#       It is provided here for students to explore on their own.
#       The animation is shown as a demonstration; the code is the take-away.
#
# In the spatial version, agents occupy cells on a grid. Interactions are
# local — agents interact only with their Moore neighborhood (8 adjacent cells).
# This produces spiral wave patterns and parameter regimes where spatial
# structure sustains populations that the mean-field version drives to extinct.
#
# Spiral waves are simply inexpressible in the mean-field language.
# They require the spatial language to exist at all.
#
# To run this section, uncomment the code below and ensure gganimate,
# gifski, and transformr are installed.

# # Parameters
# GRID_SIZE  <- 50    # 50 x 50 grid of cells
# T_SPATIAL  <- 80   # number of timesteps to animate
# P_PREY_BIRTH  <- 0.12   # prey reproduces into empty neighbor with this prob
# P_PRED_EAT    <- 0.25   # predator eats prey neighbor with this prob
# P_PRED_DEATH  <- 0.08   # predator dies with this prob each step
#
# set.seed(1234)   # for reproducible spatial patterns
#
# # Initialize grid: 0 = empty, 1 = prey, 2 = predator
# # Random initialization: 40% prey, 10% predator, 50% empty
# init_grid <- matrix(
#   sample(c(0, 1, 2),
#          size    = GRID_SIZE^2,
#          replace = TRUE,
#          prob    = c(0.5, 0.4, 0.1)),
#   nrow = GRID_SIZE,
#   ncol = GRID_SIZE
# )
#
# # Get Moore neighborhood indices (8 adjacent cells, with wraparound)
# get_neighbors <- function(row, col, size) {
#   # Wraparound (toroidal grid): avoids boundary effects
#   rows <- ((c(row - 1, row, row + 1) - 1) %% size) + 1
#   cols <- ((c(col - 1, col, col + 1) - 1) %% size) + 1
#   # All combinations except (row, col) itself
#   expand.grid(r = rows, c = cols) %>%
#     filter(!(r == row & c == col))
# }
#
# # One spatial timestep
# step_spatial <- function(grid) {
#   new_grid <- grid
#   size     <- nrow(grid)
#
#   # Random update order to avoid sweep artifacts
#   cells <- sample(size^2)
#
#   for (cell_idx in cells) {
#     row <- ((cell_idx - 1) %% size) + 1
#     col <- ((cell_idx - 1) %/% size) + 1
#
#     nbrs <- get_neighbors(row, col, size)
#
#     if (grid[row, col] == 1) {   # prey cell
#       # Try to reproduce into a random empty neighbor
#       empty_nbrs <- nbrs[grid[cbind(nbrs$r, nbrs$c)] == 0, ]
#       if (nrow(empty_nbrs) > 0 && runif(1) < P_PREY_BIRTH) {
#         chosen <- empty_nbrs[sample.int(nrow(empty_nbrs), 1), ]
#         new_grid[chosen$r, chosen$c] <- 1
#       }
#
#     } else if (grid[row, col] == 2) {   # predator cell
#       # Try to eat a random prey neighbor
#       prey_nbrs <- nbrs[grid[cbind(nbrs$r, nbrs$c)] == 1, ]
#       if (nrow(prey_nbrs) > 0 && runif(1) < P_PRED_EAT) {
#         chosen <- prey_nbrs[sample.int(nrow(prey_nbrs), 1), ]
#         new_grid[chosen$r, chosen$c] <- 2   # predator moves to prey cell
#         new_grid[row, col]           <- 0   # old cell becomes empty
#       }
#       # Natural death
#       if (runif(1) < P_PRED_DEATH) {
#         new_grid[row, col] <- 0
#       }
#     }
#   }
#
#   new_grid
# }
#
# # Run simulation and collect frames
# grid_frames <- list()
# g           <- init_grid
#
# for (tt in seq_len(T_SPATIAL)) {
#   g <- step_spatial(g)
#
#   # Convert grid matrix to tidy tibble for ggplot2
#   grid_frames[[tt]] <- as_tibble(
#     expand.grid(row = seq_len(GRID_SIZE), col = seq_len(GRID_SIZE))
#   ) %>%
#     mutate(
#       cell_type = as.vector(g),
#       label     = recode(cell_type,
#                          `0` = "empty",
#                          `1` = "prey",
#                          `2` = "predator"),
#       frame     = tt
#     )
# }
#
# spatial_df <- bind_rows(grid_frames)
#
# # Build animated plot
# p_spatial <- ggplot(spatial_df,
#                     aes(x = col, y = row, fill = label)) +
#   geom_tile() +
#   scale_fill_manual(
#     values = c(empty    = "#FAFAF7",
#                prey     = palette_ch2["prey"],
#                predator = palette_ch2["predator"]),
#     name = NULL
#   ) +
#   coord_equal() +
#   labs(
#     title    = "Spatial Lotka-Volterra: Timestep {frame}",
#     subtitle = "Local interaction on a 50x50 toroidal grid"
#   ) +
#   theme_void(base_family = "Source Serif 4") +
#   theme(
#     plot.title      = element_text(family = "Raleway", size = 12),
#     legend.position = "bottom"
#   ) +
#   transition_manual(frame)
#
# # Render to gif (requires gifski package)
# # anim_save("spatial_lv.gif", p_spatial, nframes = T_SPATIAL, fps = 8)
# # To view in RStudio viewer: animate(p_spatial, nframes = T_SPATIAL, fps = 8)


# =============================================================================
# END OF CHAPTER 2
# =============================================================================
