library(tidyverse)
library(deSolve)
library(showtext)

font_add_google("Raleway", "Raleway")
font_add_google("Source Serif 4", "Source Serif 4")
showtext_auto()
showtext_opts(dpi = 300)

set.seed(2025)

# ---- System parameters ----
ALPHA <- 0.8; BETA <- 0.04; DELTA <- 0.02; GAMMA <- 0.4
EQ_N  <- GAMMA / DELTA   # 20
EQ_P  <- ALPHA / BETA    # 20

lv_ode <- function(t, state, parms) {
  N <- state[["N"]]; P <- state[["P"]]
  list(c(
    N = ALPHA * N - BETA  * N * P,
    P = DELTA * N * P - GAMMA * P
  ))
}

# ---- ODE orbits ----
# Innermost orbit (ic=2.8) is where ABMs actually live — the Ito drift
# E[dH/dt] = (σ²/2)(γ/N + α/P) > 0 means SDE trajectories escape the
# tiny inner loops (H-gap ≈ 0.025) in under one period and settle in
# the mid-to-outer range starting around ic≈2.8–3.0.
ic_factors <- c(2.8, 3.5, 4.3, 5.2, 6.5)

ode_df <- map_dfr(seq_along(ic_factors), function(i) {
  ic  <- c(N = EQ_N * ic_factors[i], P = EQ_P)
  out <- ode(y = ic, times = seq(0, 800, by = 0.1),
             func = lv_ode, parms = NULL)
  as_tibble(as.data.frame(out)) %>%
    transmute(N, P, orbit_id = i)
})

# ---- SDE trajectories (ABM proxy) ----
run_sde <- function(ic_f, sigma, T_max, dt, seed) {
  set.seed(seed)
  N <- EQ_N * ic_f; P <- EQ_P
  n_steps <- as.integer(T_max / dt)
  N_v <- numeric(n_steps + 1); P_v <- numeric(n_steps + 1)
  N_v[1] <- N; P_v[1] <- P
  last <- 1
  for (i in seq_len(n_steps)) {
    n <- N_v[i]; p <- P_v[i]
    if (n < 0.25 || p < 0.25) break
    dN <- (ALPHA*n - BETA*n*p)*dt + sigma * sqrt(n * dt) * rnorm(1)
    dP <- (DELTA*n*p - GAMMA*p)*dt + sigma * sqrt(p * dt) * rnorm(1)
    N_v[i+1] <- max(0, n + dN)
    P_v[i+1] <- max(0, p + dP)
    last <- i + 1
  }
  tibble(N = N_v[1:last], P = P_v[1:last], rep = seed)
}

# Start ABMs at the innermost ODE orbit; no outer-orbit filter.
# Trajectories that drift beyond the outermost ODE orbit clip at the
# frame boundary — their lower-left arc (small P, small N) remains
# visible in the bottom-left corner.
sde_df <- map_dfr(1:25, ~run_sde(
  ic_f  = ic_factors[1],
  sigma = 0.22,
  T_max = 200,
  dt    = 0.08,
  seed  = .x
)) %>% filter(N > 0, P > 0)

# ---- Visual framing ----
# Frame is set by the outermost ODE orbit (ic=6.5).
# ABMs that escape beyond it clip cleanly at the frame edges.
# x = P (predators), y = N (prey) — portrait aspect fits naturally.

outer_orbit <- ode_df %>% filter(orbit_id == length(ic_factors))

P_lim <- c(min(outer_orbit$P) - 0.3,  max(outer_orbit$P) + 1.5)
N_lim <- c(min(outer_orbit$N) - 0.3,  max(outer_orbit$N) + 2.0)

bg_col   <- "#0D1B2A"
line_col <- "#B8D0E8"

p_cover <- ggplot() +
  # SDE/ABM trajectories drawn first (behind ODE orbits)
  geom_path(
    data = sde_df,
    aes(x = P, y = N, group = rep),
    color = line_col, alpha = 0.28, linewidth = 0.30
  ) +
  # ODE orbits: fully opaque — innermost matches ABM starting level
  geom_path(
    data = ode_df,
    aes(x = P, y = N, group = orbit_id),
    color = line_col, alpha = 1.0, linewidth = 0.85
  ) +
  # Equilibrium point
  annotate("point", x = EQ_P, y = EQ_N,
           color = "#FFFFFF", size = 2.2, shape = 16) +
  # Title and author: upper-right, between outermost ODE orbit and frame corner.
  # At P=55 the outer orbit's upper arc sits at N≈94; frame top is N=132,
  # leaving ~38 units of clear dark space — centred here for a natural placement.
  annotate("text",
           x     = 83,
           y     = 121,
           label = "Agent-Based Modeling\nfor Statisticians",
           color = "#FFFFFF", family = "Raleway",
           size  = 8.5, fontface = "bold",
           hjust = 1, lineheight = 1.25) +
  annotate("text",
           x     = 83,
           y     = 110,
           label = "John S. Schuler",
           color = line_col, family = "Source Serif 4",
           size  = 5.5, hjust = 1, alpha = 0.90) +
  coord_cartesian(xlim = P_lim, ylim = N_lim, expand = FALSE, clip = "on") +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = bg_col, color = NA),
    panel.background = element_rect(fill = bg_col, color = NA),
    plot.margin      = margin(0, 0, 0, 0)
  )

ggsave("book/cover.pdf", p_cover,
       width = 8.5, height = 11, units = "in",
       bg = bg_col)

cat("Cover saved to book/cover.pdf\n")
cat("SDE reps:", n_distinct(sde_df$rep), "| P range:",
    round(P_lim, 1), "| N range:", round(N_lim, 1), "\n")
