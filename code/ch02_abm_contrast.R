# =============================================================================
# Agent-Based Modeling for Statisticians — JSM 2025
# Chapter 2: Agents, Networks, and the Geometry of Interaction
# =============================================================================
#
# This script contains all R code from Chapter 2 of the course book.
# It is self-contained: all required packages are loaded in the setup section.
#
# Sections:
#   1. Setup
#   2. ABM Lotka-Volterra: functions, reduction result, extinction
#   3. ABM SIR: random mixing vs ODE
#   4. Network SIR: Erdos-Renyi vs Barabasi-Albert
#   5. Regularization path: interpolating between structured and mean-field
#   6. Spatial Lotka-Volterra (gganimate, eval=FALSE — run separately)
#
# Packages required:
#   tidyverse, deSolve, igraph, tidygraph, ggraph, gganimate, patchwork
#
# Note: The spatial LV section (ch2-spatial-lv) is marked eval=FALSE in the
# book to avoid long render times. Run it interactively. It requires gifski.
#
# GitHub: [course repository]
# =============================================================================

## ----ch2-setup----------------------------------------------------------------
# Chapter 2 Setup
# Self-contained — does not depend on Chapter 1.

library(tidyverse)
library(deSolve)
library(igraph)
library(tidygraph)
library(ggraph)
library(gganimate)
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

palette_ch2 <- c(
  prey        = "#2D5F8A",
  predator    = "#8A4A2D",
  susceptible = "#2D5F8A",
  infectious  = "#C0392B",
  recovered   = "#27AE60"
)

# ODE parameters derived directly from the ABM mass-action rates:
#   alpha = p_birth = 0.15  (per-capita prey birth rate per step)
#   gamma = p_death = 0.10  (per-capita predator death rate per step)
#   beta  = k_enc   = 0.00375 (per-pair encounter-kill rate; matches ABM)
#   delta = k_enc * p_convert = 0.00375 * 0.267 = 0.001 (predator birth rate)
# Equilibrium: N* = gamma/delta = 100, P* = alpha/beta = 40.
# Starting at (N=200, P=40): N is above N*, P is at P*, so P rises first
# while N stays near 200, then both oscillate. Period ≈ 51 steps.
lv_params_ch2 <- c(alpha = 0.15, beta = 0.00375, delta = 0.001, gamma = 0.10)
lv_ode_ch2 <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dN <- alpha * N - beta * N * P
    dP <- delta * N * P - gamma * P
    list(c(dN, dP))
  })
}

sir_params_ch2 <- c(beta = 0.0003, gamma = 0.1)
sir_ode_ch2 <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}

#' 
#' ## What Is an Agent? {#sec-agents}
#' 
#' The ODE compresses a population of a hundred rabbits to the number 100. Not an approximation of the rabbits — the number, standing in for all of them. The rabbits have no locations, no ages, no histories of particular encounters. Each is identical to every other in every respect the model can represent, which is to say in every respect. The model works with their total, and the total is everything.
#' 
#' The agent-based model takes those hundred rabbits seriously as a hundred rabbits. Each has a state. Each has a neighborhood: the set of other agents it can actually interact with at this time step. The neighborhood is the structural innovation — the thing the ODE does not have and cannot have.
#' 
#' In the ODE, the effective neighborhood of every individual is the entire population, weighted by abundance, continuously. This is what the product term $\beta NP$ says: the encounter rate between any predator and any prey is proportional to both population sizes, equally, everywhere, always. The individual's position in space, its social relationships, its proximity to particular others — these do not exist as model objects. There is only the aggregate.
#' 
#' Give an agent a neighborhood smaller than the full population, and you give the model a new kind of sentence. The neighborhood can be spatial — the eight cells surrounding a position on a grid. It can be social — the edges of a contact network. Whatever its structure, the moment it differs from "the entire population, uniformly weighted," the mean-field assumption fails. And with it falls the constraint that has been keeping certain phenomena outside the model's language.
#' 
#' ## An Agent-Based Lotka-Volterra {#sec-abm-lv}
#' 
#' The biology is unchanged. Prey reproduce, predators hunt, predators die — the same rules as the differential equations. What changes is the substrate: a population of discrete individuals rather than a continuous quantity.
#' 
#' Each individual is a row in a `tibble` carrying an identifier, a type (prey or predator), and an alive status. At each discrete time step: each surviving prey reproduces with probability $p_\text{birth}$; each predator is matched with a random prey individual and, with probability $p_\text{eat}$, kills it and may produce an offspring; each predator dies with probability $p_\text{death}$. The parameters $p_\text{birth}$, $p_\text{eat}$, $p_\text{death}$ are the individual-level counterparts of $\alpha$, $\beta\delta$, and $\gamma$ — the same rates, acting on the same events, at a different level of description.
#' 
## ----ch2-abm-lv-functions-----------------------------------------------------
# Agent-based Lotka-Volterra
# Population represented as a tibble with one row per individual

init_lv_abm <- function(n_prey, n_pred) {
  # Initialize population with prey and predator agents
  bind_rows(
    tibble(id = seq_len(n_prey),           type = "prey",     alive = TRUE),
    tibble(id = seq_len(n_pred) + n_prey,  type = "predator", alive = TRUE)
  )
}

step_lv_abm <- function(pop, p_birth = 0.15, k_enc = 0.00375, p_convert = 0.267,
                        p_death = 0.10, max_pop = 5000L) {
  # One time step of the agent-based LV model.
  # Encounter mechanism is Poisson mass-action: the expected number of
  # predator-prey kills per step is k_enc * N * P, matching the beta*N*P term
  # in the LV ODE exactly. Each kill converts to a new predator with
  # probability p_convert, so delta = k_enc * p_convert in the ODE.
  prey  <- filter(pop, type == "prey",     alive)
  preds <- filter(pop, type == "predator", alive)
  N     <- nrow(prey)
  P     <- nrow(preds)
  max_id <- max(pop$id, 0L)

  # --- Prey reproduction ---
  # Each alive prey reproduces with probability p_birth.
  # max_pop is a safety cap: births are suppressed when prey exceed this
  # threshold, preventing runaway population growth in parameter regimes
  # where predation is temporarily low.
  n_born <- if (N >= max_pop) 0L else rbinom(1, N, p_birth)
  new_prey <- if (n_born > 0) {
    tibble(id = seq_len(n_born) + max_id, type = "prey", alive = TRUE)
  } else {
    tibble(id = integer(), type = character(), alive = logical())
  }
  max_id <- max_id + n_born

  # --- Predator-prey encounters (Poisson mass-action) ---
  # Number of kills ~ Poisson(k_enc * N * P), capped at N to avoid over-kill.
  # This is the discrete-time analogue of the beta*N*P term in the LV ODE.
  # Each kill also produces a new predator with probability p_convert.
  n_kills <- min(rpois(1L, k_enc * N * P), N)
  dead_prey_ids <- integer(0)
  new_pred_rows <- tibble(id = integer(), type = character(), alive = logical())

  if (n_kills > 0) {
    dead_prey_ids <- slice_sample(prey, n = n_kills)$id
    n_new_pred    <- rbinom(1L, n_kills, p_convert)

    if (n_new_pred > 0) {
      new_pred_rows <- tibble(
        id    = seq_len(n_new_pred) + max_id,
        type  = "predator",
        alive = TRUE
      )
      max_id <- max_id + n_new_pred
    }
  }

  # --- Predator natural death ---
  # Each alive predator dies with probability p_death
  dead_pred_ids <- preds$id[rbinom(nrow(preds), 1, p_death) == 1]

  # Update population: remove dead agents, add new ones
  pop_surviving <- pop %>%
    mutate(alive = alive & !(id %in% c(dead_prey_ids, dead_pred_ids))) %>%
    filter(alive)

  bind_rows(pop_surviving, new_prey, new_pred_rows)
}

run_lv_abm <- function(T = 200, n_prey = 100, n_pred = 20, seed = 42, ...) {
  # Run the ABM for T steps and return a tidy time series
  set.seed(seed)  # seed for reproducibility; change to explore stochastic variation
  pop <- init_lv_abm(n_prey, n_pred)

  map_dfr(0:T, function(tt) {
    if (tt > 0) pop <<- step_lv_abm(pop, ...)
    tibble(
      t        = tt,
      prey     = sum(pop$type == "prey"),
      predator = sum(pop$type == "predator")
    )
  })
}

#' 
## ----ch2-abm-lv-reduction, fig.cap="The reduction result: ten stochastic ABM runs (translucent ribbons) overlaid on the ODE solution (solid lines). Under large populations and random mixing, the ABM converges to the ODE. The ODE is not a competing model; it is what the ABM becomes when the mean-field assumption is exactly satisfied."----
# Run 10 ABM replications with a large population
# Each run uses a different seed to show stochastic variation

set.seed(2025)  # master seed for reproducibility

n_reps   <- 10
T_run    <- 120   # ~2-3 LV cycles; sufficient to show convergence
n_prey0  <- 200   # large enough for law-of-large-numbers effects
n_pred0  <- 40

abm_runs <- map_dfr(1:n_reps, function(rep) {
  run_lv_abm(T = T_run, n_prey = n_prey0, n_pred = n_pred0, seed = rep,
             p_birth = 0.15, k_enc = 0.00375, p_convert = 0.267,
             p_death = 0.10) %>%
    mutate(rep = rep)
})

# ODE solution for comparison — scaled to the same initial conditions
ode_state_ch2 <- c(N = n_prey0, P = n_pred0)
ode_out_ch2   <- ode(
  y     = ode_state_ch2,
  times = seq(0, T_run, by = 1),
  func  = lv_ode_ch2,
  parms = lv_params_ch2
)
ode_df_ch2 <- as_tibble(as.data.frame(ode_out_ch2)) %>%
  rename(t = time, prey = N, predator = P)

# Tidy ABM runs for plotting
abm_long <- abm_runs %>%
  pivot_longer(c(prey, predator), names_to = "species", values_to = "count")

ode_long <- ode_df_ch2 %>%
  pivot_longer(c(prey, predator), names_to = "species", values_to = "count")

ggplot() +
  geom_line(
    data = abm_long,
    aes(x = t, y = count, color = species, group = interaction(rep, species)),
    alpha = 0.2, linewidth = 0.5
  ) +
  geom_line(
    data = ode_long,
    aes(x = t, y = count, color = species),
    linewidth = 1.2
  ) +
  scale_color_manual(
    values = palette_ch2[c("prey", "predator")],
    labels = c("prey" = "Prey", "predator" = "Predator")
  ) +
  labs(
    title    = "ABM replicates vs. ODE solution",
    subtitle = "Translucent lines: 10 stochastic ABM runs. Solid lines: ODE. N = 200 prey, 40 predators.",
    x        = "Time step",
    y        = "Population count",
    color    = NULL
  )

#' 
#' With large populations and random mixing, the stochastic runs collapse onto the ODE solution. The ODE is a limiting case of the ABM — not a different theory, but the same theory under two conditions that the ODE treats as axioms: random mixing and large numbers. The mean-field assumption, stated precisely, is the claim that both hold exactly.
#' 
#' Now reduce the population.
#' 
## ----ch2-abm-extinction, fig.cap="The ODE cannot see extinction. With small populations, ABM runs frequently reach zero — a state the ODE cannot represent. The ODE orbits forever; the ABM can, and often does, fall to zero and stay there."----
# Small-population ABM: extinction is possible
set.seed(2025)  # reproducibility

n_reps_small <- 15
T_small      <- 150
n_prey_small <- 20   # small enough for demographic stochasticity to matter
n_pred_small <- 6

abm_small <- map_dfr(1:n_reps_small, function(rep) {
  run_lv_abm(
    T         = T_small,
    n_prey    = n_prey_small,
    n_pred    = n_pred_small,
    seed      = rep + 100,
    p_birth   = 0.15,
    k_enc     = 0.00375,
    p_convert = 0.267,
    p_death   = 0.10
  ) %>%
    mutate(rep = rep)
})

# ODE solution with the same (small) initial conditions
ode_small_state <- c(N = n_prey_small, P = n_pred_small)
ode_small_out   <- ode(
  y     = ode_small_state,
  times = seq(0, T_small, by = 1),
  func  = lv_ode_ch2,
  parms = lv_params_ch2
)
ode_small_df <- as_tibble(as.data.frame(ode_small_out)) %>%
  rename(t = time, prey = N, predator = P) %>%
  pivot_longer(c(prey, predator), names_to = "species", values_to = "count")

abm_small_long <- abm_small %>%
  pivot_longer(c(prey, predator), names_to = "species", values_to = "count")

# Count extinctions
extinction_count <- abm_small %>%
  group_by(rep) %>%
  summarise(
    prey_extinct     = any(prey == 0),
    predator_extinct = any(predator == 0)
  ) %>%
  summarise(
    n_any_extinct = sum(prey_extinct | predator_extinct)
  ) %>%
  pull(n_any_extinct)

ggplot() +
  geom_line(
    data = abm_small_long,
    aes(x = t, y = count, color = species, group = interaction(rep, species)),
    alpha = 0.25, linewidth = 0.5
  ) +
  geom_line(
    data = ode_small_df,
    aes(x = t, y = count, color = species),
    linewidth = 1.2, linetype = "dashed"
  ) +
  scale_color_manual(
    values = palette_ch2[c("prey", "predator")],
    labels = c("prey" = "Prey", "predator" = "Predator")
  ) +
  labs(
    title    = "Small-population ABM: extinction events",
    subtitle = paste0(
      extinction_count, " of ", n_reps_small,
      " runs reach extinction. Dashed: ODE (cycles forever). N\u2080 = ",
      n_prey_small, " prey, ", n_pred_small, " predators."
    ),
    x     = "Time step",
    y     = "Population count",
    color = NULL
  )

#' 
#' The ODE orbits forever. For a continuous quantity that obeys a smooth differential equation, there is no path to zero unless you engineer one by hand. The ABM can fall to zero because it represents discrete individuals: when the last prey individual dies, the population is extinguished, and that is a state from which there is no return. The phenomenon of extinction — one of the most consequential events in population biology — is inexpressible in the ODE's language. Not unlikely. Not improbable. Grammatically forbidden.
#' 
#' ## An Agent-Based SIR {#sec-abm-sir}
#' 
#' ### Random Mixing {#sec-abm-sir-random}
#' 
#' The mean-field assumption can survive the move to discrete individuals. Each infectious agent contacts random members of the population at each step — the contact structure is still fully mixed, still mass-action — but the population is now a collection of persons rather than a continuous quantity. This is the reduction result for epidemics: show that the individual-based model recovers the ODE when the assumption holds, then ask what happens when it doesn't.
#' 
## ----ch2-abm-sir-functions----------------------------------------------------
# Agent-based SIR with random mixing
# Each infectious agent contacts a random subset of the population at each step

run_sir_abm_random <- function(N = 500, I0 = 5, p_infect = 0.05,
                                p_recover = 0.1, contacts_per_step = 5,
                                T = 150, seed = 42) {
  set.seed(seed)  # seed for reproducibility; change to explore variation

  agents <- tibble(
    id    = seq_len(N),
    state = c(rep("I", I0), rep("S", N - I0))
  )

  map_dfr(0:T, function(tt) {
    if (tt > 0) {
      infected_ids <- agents$id[agents$state == "I"]

      # Each infected agent makes contacts with random agents
      new_infected <- map(infected_ids, function(inf_id) {
        contacts           <- sample(setdiff(agents$id, inf_id),
                                     min(contacts_per_step, N - 1),
                                     replace = FALSE)
        suscept_contacts   <- contacts[agents$state[contacts] == "S"]
        if (length(suscept_contacts) == 0) return(integer(0))
        suscept_contacts[rbinom(length(suscept_contacts), 1, p_infect) == 1]
      }) %>%
        unlist() %>%
        unique()

      # Recovery
      recovering <- infected_ids[rbinom(length(infected_ids), 1, p_recover) == 1]

      agents <<- agents %>%
        mutate(state = case_when(
          id %in% new_infected & state == "S" ~ "I",
          id %in% recovering                  ~ "R",
          TRUE                                ~ state
        ))
    }

    tibble(
      t = tt,
      S = sum(agents$state == "S"),
      I = sum(agents$state == "I"),
      R = sum(agents$state == "R")
    )
  })
}

#' 
## ----ch2-sir-random-vs-ode, fig.cap="Agent-based SIR under random mixing (N = 500, 10 replicates shown as translucent lines) vs. ODE solution (solid lines). Convergence to the ODE is visible: the individual-based model recovers the mean-field dynamics when contacts are drawn uniformly from the population."----
set.seed(2025)

N_sir_compare <- 500
I0_compare    <- 5

# Run ABM replicates
sir_abm_runs <- map_dfr(1:10, function(rep) {
  run_sir_abm_random(
    N = N_sir_compare, I0 = I0_compare,
    p_infect = 0.05, p_recover = 0.1, contacts_per_step = 5,
    T = 150, seed = rep + 200
  ) %>%
    mutate(rep = rep)
})

# ODE comparison — match approximate R0
# R0 ≈ (p_infect * contacts_per_step) / p_recover  (rough approximation)
approx_R0  <- (0.05 * 5) / 0.1
beta_match <- approx_R0 * 0.1 / N_sir_compare

sir_ode_state <- c(S = N_sir_compare - I0_compare, I = I0_compare, R = 0)
sir_ode_out   <- ode(
  y     = sir_ode_state,
  times = seq(0, 150, by = 0.5),
  func  = sir_ode_ch2,
  parms = c(beta = beta_match, gamma = 0.1)
)
sir_ode_compare <- as_tibble(as.data.frame(sir_ode_out)) %>%
  rename(t = time)

sir_abm_long <- sir_abm_runs %>%
  pivot_longer(c(S, I, R), names_to = "compartment", values_to = "count") %>%
  mutate(compartment = factor(compartment, levels = c("S", "I", "R"),
                              labels = c("susceptible", "infectious", "recovered")))

sir_ode_long <- sir_ode_compare %>%
  pivot_longer(c(S, I, R), names_to = "compartment", values_to = "count") %>%
  mutate(compartment = factor(compartment, levels = c("S", "I", "R"),
                              labels = c("susceptible", "infectious", "recovered")))

ggplot() +
  geom_line(
    data = sir_abm_long,
    aes(x = t, y = count, color = compartment,
        group = interaction(rep, compartment)),
    alpha = 0.2, linewidth = 0.4
  ) +
  geom_line(
    data = sir_ode_long,
    aes(x = t, y = count, color = compartment),
    linewidth = 1.1
  ) +
  scale_color_manual(
    values = palette_ch2[c("susceptible", "infectious", "recovered")]
  ) +
  labs(
    title    = "ABM SIR (random mixing) vs. ODE",
    subtitle = paste0("N = ", N_sir_compare, "; 10 stochastic replicates (faint) vs. ODE (solid)"),
    x        = "Time step",
    y        = "Count",
    color    = NULL
  )

#' 
#' ### Network SIR {#sec-abm-sir-network}
#' 
#' Now give the agents an address.
#' 
## ----ch2-networks-generate----------------------------------------------------
set.seed(2025)  # reproducibility for network generation

N_net   <- 500
avg_deg <- 6   # target mean degree

# Erdos-Renyi: edges placed independently with fixed probability
# Mean degree = p * (N - 1) ≈ avg_deg
g_er <- igraph::erdos.renyi.game(N_net, p = avg_deg / (N_net - 1))

# Barabasi-Albert: preferential attachment produces a heavy-tailed degree distribution
# m = edges added per new node; mean degree ≈ 2m
g_ba <- igraph::sample_pa(N_net, m = avg_deg / 2, directed = FALSE)

# Verify mean degrees are comparable
mean_deg_er <- mean(igraph::degree(g_er))
mean_deg_ba <- mean(igraph::degree(g_ba))

#' 
#' The two networks have comparable mean degree — each individual is connected to roughly the same number of others on average. What differs is the distribution of that degree. The Erdős-Rényi graph is the random graph: each pair of nodes is connected independently with fixed probability, and the degree distribution is approximately Poisson. In a Poisson degree distribution, most nodes have degree close to the mean; there are no hubs. The Barabási-Albert graph grows by preferential attachment — new nodes attach to existing nodes in proportion to their current degree, and rich nodes get richer. The result is a scale-free degree distribution: most nodes have few connections, but a small number of hubs have very many. These hubs do not appear in an Erdős-Rényi network with the same mean degree.
#' 
## ----ch2-network-sir-function-------------------------------------------------
# Network SIR: infection spreads along edges
# Nodes are infected by each infected neighbor independently with probability p_infect

run_network_sir <- function(g, I0 = 5, p_infect = 0.05, p_recover = 0.1,
                              T = 100, seed = 42) {
  set.seed(seed)  # seed for reproducibility

  N     <- igraph::vcount(g)
  state <- rep("S", N)
  state[sample(N, I0)] <- "I"

  # Precompute adjacency list for fast neighbor lookup
  adj <- igraph::as_adj_list(g)

  map_dfr(0:T, function(tt) {
    if (tt > 0) {
      infected_ids <- which(state == "I")

      # Collect newly infected nodes
      new_I <- integer(0)
      for (v in infected_ids) {
        nbrs        <- as.integer(adj[[v]])
        suscept_nbrs <- nbrs[state[nbrs] == "S"]
        if (length(suscept_nbrs) > 0) {
          newly  <- suscept_nbrs[rbinom(length(suscept_nbrs), 1, p_infect) == 1]
          new_I  <- c(new_I, newly)
        }
      }

      # Recovery
      recovering <- infected_ids[rbinom(length(infected_ids), 1, p_recover) == 1]

      # Update states — order matters: infections before recoveries
      state[unique(new_I)] <<- "I"
      state[recovering]    <<- "R"
    }

    tibble(
      t = tt,
      S = sum(state == "S"),
      I = sum(state == "I"),
      R = sum(state == "R")
    )
  })
}

#' 
## ----ch2-network-sir-run------------------------------------------------------
set.seed(2025)

# Run SIR on both networks with identical parameters
sir_er <- run_network_sir(g_er, I0 = 5, p_infect = 0.05, p_recover = 0.1,
                           T = 100, seed = 301)
sir_ba <- run_network_sir(g_ba, I0 = 5, p_infect = 0.05, p_recover = 0.1,
                           T = 100, seed = 302)

sir_network_df <- bind_rows(
  mutate(sir_er, network = "Erdős-Rényi"),
  mutate(sir_ba, network = "Barabási-Albert")
)

#' 
## ----ch2-network-sir-plot, fig.cap="SIR epidemic curves on two networks with the same mean degree. The Barabási-Albert scale-free network (right) produces faster initial spread through hubs, different peak timing, and a different final epidemic size than the structurally simpler Erdős-Rényi graph (left). The same transmission parameters; different interaction structures; different epidemics."----
sir_network_long <- sir_network_df %>%
  pivot_longer(c(S, I, R), names_to = "compartment", values_to = "count") %>%
  mutate(
    compartment = factor(compartment, levels = c("S", "I", "R"),
                         labels = c("susceptible", "infectious", "recovered"))
  )

ggplot(sir_network_long, aes(x = t, y = count, color = compartment)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ network, labeller = label_both) +
  scale_color_manual(values = palette_ch2[c("susceptible", "infectious", "recovered")]) +
  labs(
    title    = "Network SIR: topology changes the epidemic",
    subtitle = paste0("Mean degree ≈ ", round(mean_deg_er, 1),
                      " (ER) and ≈ ", round(mean_deg_ba, 1), " (BA). Same parameters; different structure."),
    x        = "Time step",
    y        = "Count",
    color    = NULL
  )

#' 
## ----ch2-network-viz, fig.cap="Contact network structures at the peak of the epidemic. Node color indicates compartment status: blue = susceptible, red = infectious, green = recovered. Left: Erdős-Rényi graph, with roughly uniform degree distribution. Right: Barabási-Albert graph, with visible hub nodes carrying many connections. The hub structure changes the epidemic's geometry."----
set.seed(2025)

# Find peak time for each network
t_peak_er <- sir_er %>% filter(I == max(I)) %>% slice(1) %>% pull(t)
t_peak_ba <- sir_ba %>% filter(I == max(I)) %>% slice(1) %>% pull(t)

# Helper: get node states at a given time step
get_states_at_t <- function(g, t_target, I0 = 5, p_infect = 0.05,
                              p_recover = 0.1, seed = 42) {
  set.seed(seed)
  N     <- igraph::vcount(g)
  state <- rep("S", N)
  state[sample(N, I0)] <- "I"
  adj   <- igraph::as_adj_list(g)

  for (tt in seq_len(t_target)) {
    infected_ids <- which(state == "I")
    new_I <- integer(0)
    for (v in infected_ids) {
      nbrs         <- as.integer(adj[[v]])
      suscept_nbrs <- nbrs[state[nbrs] == "S"]
      if (length(suscept_nbrs) > 0) {
        newly <- suscept_nbrs[rbinom(length(suscept_nbrs), 1, p_infect) == 1]
        new_I <- c(new_I, newly)
      }
    }
    recovering       <- infected_ids[rbinom(length(infected_ids), 1, p_recover) == 1]
    state[unique(new_I)] <- "I"
    state[recovering]    <- "R"
  }
  state
}

states_er <- get_states_at_t(g_er, t_peak_er, seed = 301)
states_ba <- get_states_at_t(g_ba, t_peak_ba, seed = 302)

# Tidygraph objects for ggraph
tg_er <- as_tbl_graph(g_er) %>%
  mutate(compartment = states_er,
         network     = "Erdős-Rényi")

tg_ba <- as_tbl_graph(g_ba) %>%
  mutate(compartment = states_ba,
         network     = "Barabási-Albert")

plot_network <- function(tg, title_str, t_peak) {
  ggraph(tg, layout = "fr") +
    geom_edge_link(alpha = 0.05, color = "#888888") +
    geom_node_point(aes(color = compartment), size = 1.2, alpha = 0.8) +
    scale_color_manual(
      values = c(S = "#2D5F8A", I = "#C0392B", R = "#27AE60"),
      labels = c(S = "susceptible", I = "infectious", R = "recovered"),
      name   = NULL
    ) +
    labs(title = title_str,
         subtitle = paste0("At peak infectious (t = ", t_peak, ")")) +
    theme_void(base_family = "Source Serif 4") +
    theme(legend.position = "bottom",
          plot.title = element_text(family = "Raleway", size = 12),
          plot.background = element_rect(fill = "#FAFAF7", color = NA))
}

p_er_net <- plot_network(tg_er, "Erdős-Rényi", t_peak_er)
p_ba_net <- plot_network(tg_ba, "Barabási-Albert", t_peak_ba)

p_er_net + p_ba_net

#' 
#' The same $\beta$. The same $\gamma$. A different world.
#' 
#' The ODE cannot express this difference. Not because it has the wrong parameter values — there are no parameter values of the ODE that would reproduce the difference between these two epidemics, because the ODE has no parameter that represents contact network topology. The contact structure is not a parameter of the ODE. It is excluded from its language. The question "what does network topology do to epidemic dynamics?" cannot be asked in the ODE's vocabulary. The ODE is not giving the wrong answer to that question. It is incapable of posing it.
#' 
#' ## The Reduction Theorem {#sec-reduction}
#' 
#' The ODE is the ABM under two independent structural restrictions. The first is agent homogeneity: write each agent's property vector as $\theta_i = \bar\theta + \varepsilon_i$, and set $\varepsilon_i = 0$ for all $i$ — every agent is identical to the population mean. The second is contact structure: write the weighted contact matrix as $W = \bar{W} + \Delta W$ where $\bar{W}$ is the uniform complete graph (the maximum entropy contact structure under fixed mean contact rate), and set $\Delta W = 0$ — every agent contacts every other at the same rate. As population size grows, stochastic fluctuations suppress around the mean-field trajectory — but only given that both restrictions hold. Population size alone says nothing about contact structure. A large population interacting on a lattice, or within households, or across a scale-free network, does not approach the ODE as it grows. It approaches a denser version of the same structured system. Convergence to the ODE requires $\Delta W = 0$ as an independent structural condition, not as a consequence of large $n$.
#' 
#' When data are recorded at coarse temporal resolution — weekly, quarterly, annually — the sequence of individual interactions within each observation interval is unobservable. Contacts that occurred in a specific order between specific agents are collapsed into a single time step and treated as simultaneous. The mean-field model is the maximum entropy response to this constraint: it makes the least informative claim about within-interval interaction structure consistent with what the data can support. Below the observation frequency, no model can be preferred over the mean-field on statistical grounds, because the data do not contain the information needed to distinguish them. This is a hard epistemological limit in the spirit of the Nyquist theorem. The bias this introduces is the price of statistical stability, and it is principled in exactly the maximum entropy sense.
#' 
#' The data constraint and the structural claim are not the same argument. The data constraint says: at this observation frequency, the data cannot distinguish mean-field from structured interaction. The structural claim says: the phenomenon's natural time scale is slow enough that interactions genuinely randomize between observations — that $\Delta W \approx 0$ is warranted by the system's dynamics, not merely forced by the data. The first is imposed by the observation design. The second is a philosophical contention about the phenomenon. A researcher with high-frequency contact data who nevertheless invokes the mean-field is making the structural claim. A researcher with only low-frequency data who invokes it may be making either — and should say which, because the two have different implications for what could, in principle, falsify the model.
#' 
#' The theorem: as $n \to \infty$, as $\varepsilon_i \to 0$ for all $i$, and as $\Delta W \to 0$, the ABM converges in distribution to the ODE. This is a conditional result. It says what the ABM becomes when both restrictions are imposed simultaneously; it does not say that those restrictions are warranted. The ODE is not wrong — it is the correct model when both conditions hold. The question is always whether they hold for the phenomenon at hand.
#' 
#' System dynamics and agent-based models are not competing theories. They are a richer language and one of its sublanguages, related by two named restrictions rather than by a vague appeal to complexity or realism. Every ABM that includes random mixing and agent homogeneity as special cases contains the ODE as a theorem. The choice between them is not philosophical. It is a question about which restrictions are warranted for the phenomenon and the data.
#' 
#' The ABM has more nominal parameters than the Lotka-Volterra ODE: $p_\text{birth}$, $p_\text{eat}$, $p_\text{death}$, the population size, the pairing mechanism. The ODE has four: $\alpha$, $\beta$, $\delta$, $\gamma$. Nominal dimensionality is not effective dimensionality. A regression with a thousand predictors may have a sparse solution concentrated on ten. The statistician who counts predictors before asking whether the signal is sparse has the analysis backwards. The critic who counts ABM parameters before asking how many degrees of freedom the dynamics of interest actually require has made the same mistake. Many regions of the ABM's parameter space produce qualitatively identical behavior — cycling statistically indistinguishable from the ODE output. The consequential structure — the boundary between cycling and extinction, the dependence of extinction probability on population size — is low-dimensional relative to the nominal parameter count. The ODE is the ABM under maximal regularization of both $\varepsilon_i$ and $\Delta W$: the restriction that produces its analytical tractability is the same restriction that removes certain phenomena from its vocabulary, and no amount of careful parameter estimation recovers what that regularization has discarded.
#' 
#' Parameterize the contact matrix as a mixture: with probability $p$, each infected agent contacts a random individual drawn uniformly from the population; with probability $1-p$, it contacts a neighbor from the scale-free network. At $p = 1$, $\Delta W = 0$ exactly — the mean-field. At $p = 0$, $\Delta W$ carries the full network structure. Every intermediate value is a point on the regularization path.
#' 
## ----ch2-regularization-path, fig.cap="The regularization path from structured network contact ($p = 0$, full $\\Delta W$) to random mixing ($p = 1$, $\\Delta W = 0$). Each curve shows $I(t)$ for a different value of the mixing probability on the Barabási-Albert network. The ODE (dashed) is the $p = 1$ limit. Dialing $p$ from 0 to 1 shrinks $\\Delta W$ toward zero — the regularization path between the structured ABM and the differential equation."----
set.seed(2025)  # reproducibility

# Mixed contact SIR: each infected agent makes contacts_per_step contacts per step.
# Each contact is drawn from the global population with probability p_random,
# or from the network neighborhood with probability 1 - p_random.
# Total contact rate is held constant across p_random values, so differences
# in dynamics are purely structural — a change in Delta W, not in contact rate.
run_mixed_sir <- function(g, p_random = 0.5, I0 = 5, p_infect = 0.05,
                           p_recover = 0.1, contacts_per_step = 6,
                           T = 100, seed = 42) {
  set.seed(seed)
  N     <- igraph::vcount(g)
  state <- rep("S", N)
  state[sample(N, I0)] <- "I"
  adj   <- igraph::as_adj_list(g)

  map_dfr(0:T, function(tt) {
    if (tt > 0) {
      infected_ids <- which(state == "I")
      new_I <- integer(0)

      for (v in infected_ids) {
        # Binomial split: n_rand contacts from global pool, remainder from network
        n_rand <- rbinom(1, contacts_per_step, p_random)
        n_net  <- contacts_per_step - n_rand

        contacts <- integer(0)
        if (n_rand > 0)
          contacts <- c(contacts,
                        sample(setdiff(seq_len(N), v), min(n_rand, N - 1),
                               replace = FALSE))
        if (n_net > 0) {
          nbrs <- as.integer(adj[[v]])
          if (length(nbrs) > 0)
            contacts <- c(contacts,
                          sample(nbrs, min(n_net, length(nbrs)), replace = FALSE))
        }

        suscept <- contacts[state[contacts] == "S"]
        if (length(suscept) > 0)
          new_I <- c(new_I, suscept[rbinom(length(suscept), 1, p_infect) == 1])
      }

      recovering           <- infected_ids[rbinom(length(infected_ids), 1, p_recover) == 1]
      state[unique(new_I)] <<- "I"
      state[recovering]    <<- "R"
    }
    tibble(t = tt, S = sum(state == "S"), I = sum(state == "I"), R = sum(state == "R"))
  })
}

# Five points on the regularization path
p_values <- c(0, 0.25, 0.5, 0.75, 1.0)

mixed_runs <- map_dfr(seq_along(p_values), function(i) {
  run_mixed_sir(g_ba, p_random = p_values[i], I0 = 5,
                p_infect = 0.05, p_recover = 0.1, contacts_per_step = avg_deg,
                T = 100, seed = 400 + i) %>%
    mutate(p_random = p_values[i])
})

# ODE overlay: beta = p_infect * contacts_per_step / N
# Each infected makes avg_deg contacts per step; each susceptible contact infected
# with probability p_infect. Expected new infections = I * avg_deg * (S/N) * p_infect
# = beta * S * I, giving beta = p_infect * avg_deg / N. R0 = beta * N / gamma = 3.
beta_reg <- (0.05 * avg_deg) / N_net
ode_reg  <- as_tibble(as.data.frame(
  ode(y     = c(S = N_net - 5, I = 5, R = 0),
      times = seq(0, 100, by = 0.5),
      func  = sir_ode_ch2,
      parms = c(beta = beta_reg, gamma = 0.1))
)) %>% rename(t = time)

ggplot() +
  geom_line(
    data = mixed_runs,
    aes(x = t, y = I, color = p_random, group = p_random),
    linewidth = 0.9
  ) +
  geom_line(
    data = ode_reg,
    aes(x = t, y = I),
    linetype = "dashed", color = "#1C1C1E", linewidth = 1.0
  ) +
  scale_color_gradient(
    low    = "#8A4A2D",
    high   = "#2D5F8A",
    name   = expression(p),
    breaks = p_values,
    labels = p_values
  ) +
  annotate("text", x = 80, y = max(ode_reg$I) * 1.05,
           label = "ODE (p = 1 limit)", family = "Source Serif 4",
           size = 3.2, color = "#1C1C1E", hjust = 0) +
  labs(
    title    = "Regularization path: structured contact to mean-field",
    subtitle = paste0("Barabási-Albert network, N = ", N_net,
                      ". Each curve: one value of p. ",
                      "p = 1 is random mixing; p = 0 is network-only contact."),
    x        = "Time step",
    y        = "Infectious individuals (I)"
  )

#' 
#' ## Spatial Extension (Reference Module) {#sec-spatial}
#' 
#' > **Note for readers:** This section is provided for self-study and is not covered in the live session. The code below implements a full spatial Lotka-Volterra model on a grid. In the live session, the output animation is shown as a demonstration; the code walkthrough is skipped because the principle has already been established. Students who wish to explore the spatial extension are encouraged to run this code and experiment with the parameters.
#' 
#' The spatial extension moves the Lotka-Volterra dynamics onto a discrete grid. Each cell of the grid can hold prey and predators. Interactions are local: agents interact only with their Moore neighborhood (the eight adjacent cells). The mean-field assumption is replaced by a spatial interaction structure — every individual's neighborhood is its eight immediate neighbors, not the entire population.
#' 
#' The consequence is qualitative, not merely quantitative. Spatial structure can sustain populations that the mean-field dynamics would drive to extinction. It can produce spatial pattern formation — spiral waves of prey and predator density rotating through the grid. These patterns are not present in the ODE and cannot be approximated by the ODE, because they are spatial phenomena and the ODE has no spatial dimension. They require the spatial language to exist at all.
#' 
## ----ch2-spatial-lv, eval=FALSE-----------------------------------------------
# # SELF-STUDY EXTENSION: Spatial Lotka-Volterra on a grid
# # This code is included in the distributed materials for student exploration.
# # The animation requires gifski; rendering may take several minutes.
# # The live session shows a pre-rendered animation; this code is not run live.
# 
# set.seed(2025)  # seed for reproducibility; different seeds produce different spiral patterns
# 
# GRID_SIZE <- 50
# T_SPATIAL <- 120
# 
# # Initialize grid: each cell is a row in a tibble with prey and predator counts
# init_spatial_grid <- function(size = GRID_SIZE, n_prey = 400, n_pred = 80) {
#   grid <- expand.grid(x = 1:size, y = 1:size) %>%
#     as_tibble() %>%
#     mutate(prey = 0L, predator = 0L)
# 
#   # Distribute initial agents randomly across cells
#   prey_cells <- sample(nrow(grid), n_prey,  replace = TRUE)
#   pred_cells <- sample(nrow(grid), n_pred, replace = TRUE)
# 
#   for (cell in prey_cells) grid$prey[cell]     <- grid$prey[cell]     + 1L
#   for (cell in pred_cells)  grid$predator[cell] <- grid$predator[cell] + 1L
# 
#   grid
# }
# 
# # Moore neighborhood indices on a toroidal grid (wraps at boundaries)
# moore_nbr_indices <- function(x, y, size, grid_df) {
#   dx <- c(-1L, 0L, 1L, -1L, 1L, -1L, 0L, 1L)
#   dy <- c(-1L, -1L, -1L, 0L, 0L, 1L, 1L, 1L)
#   nx <- ((x + dx - 1L) %% size) + 1L
#   ny <- ((y + dy - 1L) %% size) + 1L
#   # Return row indices into the grid tibble
#   (ny - 1L) * size + nx
# }
# 
# # One step of the spatial LV model
# step_spatial_lv <- function(grid, size = GRID_SIZE,
#                               p_birth = 0.25, p_eat = 0.35, p_death = 0.12,
#                               p_move = 0.4) {
#   new_grid <- grid
# 
#   for (i in seq_len(nrow(grid))) {
#     x <- grid$x[i]
#     y <- grid$y[i]
#     nbr_idx <- moore_nbr_indices(x, y, size, grid)
# 
#     # --- Prey reproduction with offspring dispersal to random neighbor ---
#     if (grid$prey[i] > 0L) {
#       born <- rbinom(1L, grid$prey[i], p_birth)
#       if (born > 0L) {
#         # Offspring land in random neighboring cells
#         targets <- sample(nbr_idx, born, replace = TRUE)
#         for (target in targets) {
#           new_grid$prey[target] <- new_grid$prey[target] + 1L
#         }
#       }
#     }
# 
#     # --- Predator-prey encounter: local predation ---
#     if (grid$prey[i] > 0L && grid$predator[i] > 0L) {
#       encounters <- min(grid$prey[i], grid$predator[i])
#       eaten      <- rbinom(1L, encounters, p_eat)
#       new_grid$prey[i] <- max(0L, new_grid$prey[i] - eaten)
#       # Each kill produces a new predator with probability 0.7
#       offspring <- rbinom(1L, eaten, 0.7)
#       new_grid$predator[i] <- new_grid$predator[i] + offspring
#     }
# 
#     # --- Predator natural death ---
#     if (grid$predator[i] > 0L) {
#       dead <- rbinom(1L, grid$predator[i], p_death)
#       new_grid$predator[i] <- max(0L, new_grid$predator[i] - dead)
#     }
# 
#     # --- Predator movement toward most prey-dense neighbor ---
#     if (new_grid$predator[i] > 0L && runif(1L) < p_move) {
#       # Find neighbor with most prey
#       nbr_prey     <- grid$prey[nbr_idx]
#       best_nbr_pos <- which.max(nbr_prey)
#       target_idx   <- nbr_idx[best_nbr_pos]
# 
#       # Move a binomial fraction of predators to that neighbor
#       movers <- rbinom(1L, new_grid$predator[i], p_move)
#       new_grid$predator[i]          <- new_grid$predator[i] - movers
#       new_grid$predator[target_idx] <- new_grid$predator[target_idx] + movers
#     }
#   }
# 
#   new_grid
# }
# 
# # Run the spatial model and collect frames for animation
# spatial_grid   <- init_spatial_grid()
# spatial_frames <- vector("list", T_SPATIAL)
# spatial_frames[[1]] <- mutate(spatial_grid, t = 1L)
# 
# for (tt in 2:T_SPATIAL) {
#   spatial_grid       <- step_spatial_lv(spatial_grid)
#   spatial_frames[[tt]] <- mutate(spatial_grid, t = as.integer(tt))
#   # Early termination if total population crashes to zero
#   if (sum(spatial_grid$prey) == 0 || sum(spatial_grid$predator) == 0) break
# }
# 
# spatial_df <- bind_rows(spatial_frames)
# 
# # Prey density animation
# spatial_anim <- ggplot(spatial_df, aes(x = x, y = y, fill = prey)) +
#   geom_tile() +
#   scale_fill_gradient(
#     low  = "#FAFAF7",
#     high = "#2D5F8A",
#     name = "Prey count"
#   ) +
#   coord_equal() +
#   labs(
#     title    = "Spatial Lotka-Volterra — prey density",
#     subtitle = "Time: {as.integer(frame_time)}"
#   ) +
#   theme_void(base_family = "Source Serif 4") +
#   theme(
#     plot.background = element_rect(fill = "#FAFAF7", color = NA),
#     plot.title      = element_text(family = "Raleway", size = 12),
#     legend.position = "right"
#   ) +
#   transition_time(t) +
#   ease_aes("linear")
# 
# # Uncomment to save the animation (requires gifski package):
# # anim_save(
# #   "spatial_lv.gif",
# #   animate(spatial_anim, nframes = T_SPATIAL, fps = 10, width = 600, height = 600)
# # )

#' 
#' The spiral wave patterns that emerge from the spatial model are a dynamical regime without a name in the ODE vocabulary. They require spatial coordinates to exist. The ODE's world has no place for them, not because they are exotic or unusual, but because the ODE is a language in which spatial position has no representation. The patterns are grammatically inexpressible — not as an approximation error, but as a structural fact about the model's language.
#' 
#' ---
#' 
#' # The Size of Firms, and What It Tells Us {#ch3}
#' 
