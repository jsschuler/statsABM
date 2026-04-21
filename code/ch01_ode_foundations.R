# =============================================================================
# Agent-Based Modeling for Statisticians — JSM 2025
# Chapter 1: The Differential Equations of Living Systems
# =============================================================================
#
# This script contains all R code from Chapter 1 of the course book.
# It is self-contained: all required packages are loaded in the setup section.
#
# Sections:
#   1. Setup
#   2. Lotka-Volterra: time series and phase portrait
#   3. SIR: base epidemic and R0 sweep
#
# Packages required:
#   tidyverse, deSolve, patchwork
#
# GitHub: [course repository]
# =============================================================================

## ----ch1-setup----------------------------------------------------------------
# Chapter 1 Setup
# All packages and global settings for this chapter.
# This chunk is self-contained — it does not depend on any prior chapter.

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

palette_ch1 <- c(
  prey        = "#2D5F8A",
  predator    = "#8A4A2D",
  susceptible = "#2D5F8A",
  infectious  = "#C0392B",
  recovered   = "#27AE60"
)

#' 
#' The simplest living systems defeat the intuitions we bring to them. Not because their rules are complicated — the rules governing a population of rabbits and foxes are not complicated — but because feedback does something to time that our unaided intuition cannot follow. We expect growth to continue or stop; we do not expect it to reverse, gather again, and reverse once more in endless repetition. We expect an epidemic to burn through everyone it can reach. These expectations are wrong, and they are wrong in the same way: we have failed to account for what happens when a process feeds back on the conditions of its own continuation.
#' 
#' A population of rabbits with unlimited food grows exponentially. Everyone knows this. What is less obvious is that the fox population grows with it — and then the rabbits decline — and then the foxes, deprived of prey, decline in turn — and then the rabbits recover. The system cycles. Not approximately, not with damping toward some equilibrium — it cycles exactly, the same orbit traversed indefinitely, a clock that does not run down. This is genuinely strange. It is the kind of strangeness that deserves an instrument adequate to it.
#' 
#' The differential equation is that instrument. It does not merely describe change over time; it describes the rate of change as a function of the current state, which is precisely the structure feedback requires. The state determines the rate, the rate changes the state, the new state determines a new rate — the equation captures this self-referential logic and makes it tractable.
#' 
#' Familiar equations accumulate invisible assumptions. The predator-prey model and the epidemic model are a century old; the choices embedded in their functional forms have had that long to become invisible. What is hiding in those forms — and what it costs to have those things hidden — is what the rest of the day is about.
#' 
#' ## Predators, Prey, and the Geometry of Cycles {#sec-lv}
#' 
#' Begin with the prey. Absent predators, a prey population grows at a rate proportional to its size — each individual produces offspring at the same per-capita rate $\alpha$, and the population is not resource-constrained. This gives the exponential growth term $\alpha N$. Predators, absent prey, decline at a per-capita rate $\gamma$ — they die without food. These are the uncoupled dynamics: rabbits grow, foxes die.
#' 
#' Coupling enters through encounters. A predator that encounters a prey individual kills it with some probability; the encounter benefits the predator. The rate at which these encounters occur is proportional to the product $NP$. This deserves a moment's attention before the equation appears. The product $NP$ says that every predator is equally likely to encounter every prey individual: if there are twice as many prey, encounters double; if there are twice as many predators, encounters double. The encounter rate is the product of the two abundances, multiplied by a rate constant $\beta$ that converts encounters into kills.
#' 
#' $$\frac{dN}{dt} = \alpha N - \beta N P$$
#' 
#' $$\frac{dP}{dt} = \delta N P - \gamma P$$
#' 
#' The parameter $\alpha$ is the prey birth rate; $\beta$ is the predation rate coefficient — the probability per unit time per predator-prey pair that an encounter results in a kill; $\delta$ is the conversion efficiency, the rate at which prey consumed is converted into predator offspring; $\gamma$ is the predator death rate. Four parameters. Two equations. A complete description of a world with two species and one interaction.
#' 
#' The nullclines — the curves along which each population is momentarily stationary — divide the phase plane into four regions. The prey nullcline $dN/dt = 0$ requires either $N = 0$ or $P = \alpha/\beta$: the prey population is constant when the predator population is exactly $\alpha/\beta$, regardless of how many prey there are. The predator nullcline $dP/dt = 0$ requires either $P = 0$ or $N = \gamma/\delta$: predators are stationary when prey are at exactly $\gamma/\delta$. The nontrivial equilibrium is the crossing point: $N^* = \gamma/\delta$, $P^* = \alpha/\beta$.
#' 
#' The trajectories are closed orbits around this equilibrium. Not spiraling inward, not spiraling outward — closed. The system cycles forever without damping or growth. This is the pendulum that never loses energy, a machine that returns to any given state without ever simplifying toward rest. It is geometrically beautiful and physically strange. A physicist would say the system is conservative — it has a constant of motion, a quantity that is preserved along every trajectory. Volterra showed that $\delta N - \gamma \ln N + \beta P - \alpha \ln P$ is constant along solutions of the system. The orbits are the level curves of this conserved quantity.
#' 
#' Each initial condition $(N_0, P_0)$ selects a particular orbit, and the system follows that orbit indefinitely. The interior of each orbit is another orbit; the exterior is another. The equilibrium point is not a fixed point in the attracting sense — it is surrounded by a continuous family of periodic orbits, each one a different amplitude of the same cycle. The phase plane is therefore not a diagram of what the system tends toward; it is a diagram of what the system always does.
#' 
## ----ch1-lv-timeseries, fig.cap="Lotka-Volterra predator-prey dynamics. Prey (blue) and predator (brown) populations cycle indefinitely. Parameters: $\\alpha = 0.8$, $\\beta = 0.4$, $\\delta = 0.3$, $\\gamma = 0.6$."----
# Lotka-Volterra ODE system
# set.seed not required here — deSolve is deterministic
lv_params <- c(alpha = 0.8, beta = 0.4, delta = 0.3, gamma = 0.6)
lv_state  <- c(N = 10, P = 5)

lv_ode <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dN <- alpha * N - beta * N * P
    dP <- delta * N * P - gamma * P
    list(c(dN, dP))
  })
}

times_lv <- seq(0, 50, by = 0.1)
lv_out   <- ode(y = lv_state, times = times_lv, func = lv_ode, parms = lv_params)

lv_df <- as_tibble(as.data.frame(lv_out)) %>%
  pivot_longer(c(N, P), names_to = "species", values_to = "population") %>%
  mutate(species = recode(species, N = "prey", P = "predator"))

ggplot(lv_df, aes(x = time, y = population, color = species)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = palette_ch1[c("prey", "predator")],
    labels = c("prey" = "Prey (N)", "predator" = "Predator (P)")
  ) +
  labs(
    title    = "Lotka-Volterra: population time series",
    subtitle = expression(alpha == 0.8 ~ "," ~ beta == 0.4 ~ "," ~ delta == 0.3 ~ "," ~ gamma == 0.6),
    x        = "Time",
    y        = "Population",
    color    = NULL
  )

#' 
## ----ch1-lv-phase, fig.cap="Phase portrait of Lotka-Volterra dynamics. Each curve is a closed orbit corresponding to a different initial condition. The system returns to each point on a given orbit with exact periodicity."----
# Phase portrait: multiple initial conditions
# The system is deterministic; no seed needed
initial_conditions <- list(
  c(N = 10, P = 5),
  c(N = 15, P = 4),
  c(N = 8,  P = 7),
  c(N = 20, P = 3),
  c(N = 6,  P = 9)
)

orbit_df <- map_dfr(
  seq_along(initial_conditions),
  function(i) {
    out <- ode(
      y     = initial_conditions[[i]],
      times = seq(0, 80, by = 0.05),
      func  = lv_ode,
      parms = lv_params
    )
    as_tibble(as.data.frame(out)) %>%
      mutate(orbit = factor(i))
  }
)

# Equilibrium point
N_star <- lv_params["gamma"] / lv_params["delta"]
P_star <- lv_params["alpha"] / lv_params["beta"]

ggplot(orbit_df, aes(x = N, y = P, color = orbit)) +
  geom_path(linewidth = 0.7, alpha = 0.8) +
  annotate("point", x = N_star, y = P_star, size = 3, color = "#1C1C1E") +
  annotate("text", x = N_star + 0.4, y = P_star + 0.3,
           label = "Equilibrium", family = "Source Serif 4", size = 3.5) +
  scale_color_manual(values = colorRampPalette(c("#2D5F8A", "#8A4A2D"))(5)) +
  guides(color = "none") +
  labs(
    title = "Lotka-Volterra: phase portrait",
    subtitle = "Five initial conditions, each tracing a closed orbit",
    x = "Prey population (N)",
    y = "Predator population (P)"
  )

#' 
#' ## The Mean-Field Assumption {#sec-meanfield}
#' 
#' Something has been hiding in the product term $\beta NP$, and it has been hiding there in plain sight.
#' 
#' The expression says that the rate of predator-prey encounters is proportional to the product of population sizes. For this to hold, every predator must be equally likely to encounter every prey individual at every moment. The population is perfectly mixed. Space does not exist. Individuals have no locations, no territories, no persistent relationships. There are no dense patches and no empty expanses. There is no structure at all to the spatial arrangement of individuals — or rather, the assumption is that the spatial arrangement is irrelevant because mixing is instantaneous and complete.
#' 
#' This is the mean-field assumption. Its name comes from physics, where it appears in the analysis of magnetic systems: the effective field experienced by any one particle is the mean field produced by all the others, rather than the specific field produced by its actual neighbors. In population ecology it enters through the functional form of the interaction term. It is not stated as an assumption in most presentations of Lotka-Volterra. It is embedded in the mathematics. The product $NP$ is not the only way to model predator-prey encounters; it is the way that assumes perfect mixing.
#' 
#' The mean-field assumption is a parametric family choice: it selects the family of models governed by mass-action kinetics, excluding all others — network models, spatial models, individual-based models — before a single observation is consulted. The product $NP$ is not a parameter to be estimated. It is the choice of family, and that choice determines what phenomena the family can express. Structured populations, spatial aggregations, social networks among individuals, heterogeneous encounter rates — these are not poorly approximated within the family. They are not in the family.
#' 
#' That family choice is simultaneously a choice about temporal resolution. The ODE family is defined in continuous time — formally, the limit as the time step shrinks to zero. But the interaction term $\beta NP$ is appropriate only when the observation interval is coarse enough for the law of large numbers to operate across interactions within each step. At fine enough temporal resolution the family is misspecified: in any given instant most individuals are not contacting most others, and the product term misrepresents the actual contact process. The mean-field ODE is the right family at temporal resolutions coarse enough for spatial averaging to hold within each step. Whether that condition is satisfied is an empirical question about the system's contact dynamics — not a modeling convenience.
#' 
#' The closed-orbit result depends on this assumption. The conserved quantity depends on it. When real predator-prey systems fail to cycle as neatly as the model predicts — and they often do — the mean-field assumption is frequently where the discrepancy originates. Its most important consequences, however, will not come into view until the contrast with agent-based models is drawn.
#' 
#' ## Epidemics and the Threshold Phenomenon {#sec-sir}
#' 
#' The same assumption appears in epidemic modeling, in precisely the same place, governing a different interaction.
#' 
#' A closed population of $N$ individuals. Each is in one of three states: Susceptible, Infectious, or Recovered (and immune). Infectious individuals shed pathogen; susceptible individuals become infected upon exposure. The rate at which susceptible individuals encounter infectious ones is proportional to $SI$ — the number of susceptibles times the number of infecteds, multiplied by a rate constant $\beta$. Recovery occurs at per-capita rate $\gamma$.
#' 
#' $$\frac{dS}{dt} = -\beta S I$$
#' 
#' $$\frac{dI}{dt} = \beta S I - \gamma I$$
#' 
#' $$\frac{dR}{dt} = \gamma I$$
#' 
#' The mean-field assumption is here again: the contact rate is proportional to the product $SI$, which assumes that every susceptible individual is equally likely to contact every infectious individual. Human contact, of course, is not structured this way. People have households, workplaces, social networks — structures that make some contacts far more probable than others. The SIR model, like Lotka-Volterra, treats this structure as if it does not exist.
#' 
#' From the sign of $dI/dt$ at the start of an epidemic, when $S \approx N$:
#' 
#' $$\frac{dI}{dt}\bigg|_{t=0} \approx I(\beta N - \gamma) = I \cdot \gamma\left(\frac{\beta N}{\gamma} - 1\right)$$
#' 
#' The epidemic grows if $\beta N / \gamma > 1$ and dies out if $\beta N / \gamma < 1$. The quantity $\mathcal{R}_0 = \beta N / \gamma$ is the basic reproduction number: the expected number of secondary infections produced by a single infectious individual in a fully susceptible population. It is not a numerical fact to memorize. It is a structural consequence of the model — a ratio between the transmission term and the recovery term, visible directly in the algebra. The epidemic threshold at $\mathcal{R}_0 = 1$ is where the production of new infections exactly balances the removal of current ones.
#' 
#' The years since 2020 have given most people in the room considerably more opinions about this model than they might have anticipated forming. The herd immunity threshold, the effect of contact reduction, the role of $\mathcal{R}_0$ in policy discussions — these became household concepts in a span of weeks. What remained, for the most part, unstated in those discussions was the mean-field assumption underlying the standard SIR framework: that the contact structure of the population could be adequately represented by the product $SI$, ignoring the network topology that governs who actually contacts whom.
#' 
## ----ch1-sir-base, fig.cap="SIR epidemic dynamics. The epidemic rises, peaks when the susceptible pool is depleted sufficiently, and declines. The herd immunity threshold is reached when enough individuals have recovered that $\\mathcal{R}_0 S/N < 1$."----
# SIR ODE implementation
sir_params <- c(beta = 0.0003, gamma = 0.1)
sir_state  <- c(S = 990, I = 10, R = 0)
N_pop      <- sum(sir_state)

# R0 under these parameters
R0_base <- sir_params["beta"] * N_pop / sir_params["gamma"]

sir_ode <- function(t, state, params) {
  with(as.list(c(state, params)), {
    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-  gamma * I
    list(c(dS, dI, dR))
  })
}

times_sir <- seq(0, 200, by = 0.5)
sir_out   <- ode(y = sir_state, times = times_sir, func = sir_ode, parms = sir_params)

sir_df <- as_tibble(as.data.frame(sir_out)) %>%
  pivot_longer(c(S, I, R), names_to = "compartment", values_to = "count") %>%
  mutate(
    compartment = factor(
      compartment,
      levels = c("S", "I", "R"),
      labels = c("susceptible", "infectious", "recovered")
    )
  )

ggplot(sir_df, aes(x = time, y = count, color = compartment)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(
    values = palette_ch1[c("susceptible", "infectious", "recovered")]
  ) +
  labs(
    title    = "SIR epidemic dynamics",
    subtitle = bquote(beta == .(sir_params["beta"]) ~ "," ~
                      gamma == .(sir_params["gamma"]) ~ "," ~
                      R[0] == .(round(R0_base, 2))),
    x        = "Time",
    y        = "Count",
    color    = NULL
  )

#' 
## ----ch1-sir-sweep, fig.cap="SIR epidemic curves across values of $\\mathcal{R}_0$. Each curve shows the infectious compartment $I(t)$ for a different transmission rate. The epidemic threshold at $\\mathcal{R}_0 = 1$ is visible: below it, no outbreak occurs; above it, epidemic size grows with $\\mathcal{R}_0$."----
# Parameter sweep over R0
# Vary beta to produce different R0 values; gamma and N fixed

set.seed(2025)  # seed for reproducibility; results are deterministic here, but good practice

R0_values <- c(0.8, 1.2, 1.5, 2.0, 3.0, 5.0)
gamma_fixed <- 0.1
N_fixed     <- 1000

sweep_df <- map_dfr(R0_values, function(r0) {
  beta_val <- r0 * gamma_fixed / N_fixed
  params_i <- c(beta = beta_val, gamma = gamma_fixed)
  state_i  <- c(S = N_fixed - 10, I = 10, R = 0)
  out_i    <- ode(y = state_i, times = seq(0, 250, by = 0.5),
                  func = sir_ode, parms = params_i)
  as_tibble(as.data.frame(out_i)) %>%
    mutate(R0 = r0) %>%
    select(time, I, R0)
})

ggplot(sweep_df, aes(x = time, y = I, color = factor(R0), group = factor(R0))) +
  geom_line(linewidth = 0.85) +
  scale_color_manual(
    values = colorRampPalette(c("#AED6F1", "#C0392B"))(length(R0_values)),
    name   = expression(R[0])
  ) +
  labs(
    title = expression("SIR epidemic curves across values of" ~ R[0]),
    x     = "Time",
    y     = "Infectious individuals (I)"
  )

#' 
#' ## What the Language Can Say {#sec-ch1-language}
#' 
#' The ODE is a machine for processing continuous quantities. The prey population $N = 3.7$ is as valid a model state as $N = 37$ or $N = 370$. This is not an approximation of discrete reality — it is a different ontology. The formalism requires it: a differential equation governing $dN/dt$ must treat $N$ as something smooth enough to differentiate, which means a real number rather than a count. Individuals are absent. Only totals exist.
#' 
#' Once populations are continuous, they must be homogeneous. The ODE tracks totals; it has no machinery for tracking distributions within compartments. If prey individuals differ in their predation risk, the ODE has nowhere to put this fact — it has only $N$. The model therefore treats all individuals of the same type as identical: same transmission probability, same predation risk, same birth rate. The distribution of individual characteristics collapses to a point mass. A real fox population contains young foxes and old foxes, healthy and sick, bold hunters and timid ones. The ODE knows none of this, because a total is not a distribution.
#' 
#' Identical individuals interacting through their totals: this is already the mean-field assumption, arrived at from the inside. If all prey individuals are equivalent, the rate at which any predator encounters prey is proportional to $N$, independently of where it is or what it has done before. If all predators are likewise equivalent, encounters occur at rate $\beta NP$. The product term is the consequence of the prior two choices, not a third independent assumption. And with it follows everything else: space does not exist because individuals have no positions; history does not matter because the state at $t$ — two numbers — is all there is. There is nothing to accumulate.
#' 
#' These are not flaws. They are choices, and they have been scientifically productive for over a century. Lotka-Volterra was formulated in the 1920s; SIR was published in 1927 by Kermack and McKendrick. The models have generated real insight into predator-prey dynamics, epidemic thresholds, herd immunity. The point is not that the choices are wrong. It is that they are a chain — continuity requires homogeneity, homogeneity implies the mean-field, the mean-field excludes position and history — and every link was forged before the modeler wrote the first equation.
#' 
#' The product term $\beta NP$ is not a neutral functional form. It is a commitment — to uniformity of access, to the irrelevance of position, relationship, and history. When the commitment holds, the model is not merely adequate. It is the right instrument, not an approximation of one. When it fails, the model does not give imprecise answers. It gives precise answers about a world in which individuals have no addresses and no neighbors except everyone.
#' 
#' ---
#' 
#' # Agents, Networks, and the Geometry of Interaction {#ch2}
#' 
