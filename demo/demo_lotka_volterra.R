## demo_lotka_volterra.R
## Compares the Lotka-Volterra ODE with the predator-prey ABM.
## Figures: side-by-side time series + phase portrait.
##
## Run from the package root:
##   Rscript demo/demo_lotka_volterra.R

pkgload::load_all(".", quiet = TRUE)

if (!dir.exists("figures")) dir.create("figures")

# ── 1. ODE: Lotka-Volterra ────────────────────────────────────────────────────
cli::cli_h1("Lotka-Volterra ODE")

lv <- run_lv(params = LV_DEFAULTS)

cli::cli_alert_info(
  "Equilibrium: prey* = {round(lv$equilibrium['prey'], 2)}, \\
   predators* = {round(lv$equilibrium['predators'], 2)}"
)

# Colour map: prey in sd-blue, predators in abm-plum
lv_colours <- c(prey = COURSE_PALETTE[["sd"]], predators = COURSE_PALETTE[["abm"]])

p_lv_ts <- plot_time_series(
  lv$results,
  title      = "Lotka-Volterra ODE",
  colour_map = lv_colours
)

# Phase portrait requires wide format with columns x, y
phase_wide <- tidyr::pivot_wider(lv$results,
                                  names_from  = "variable",
                                  values_from = "value")
p_lv_phase <- plot_phase_portrait(
  data.frame(x = phase_wide$prey, y = phase_wide$predators),
  xlabel = "Prey",
  ylabel = "Predators",
  title  = "ODE phase portrait"
)

ggplot2::ggsave("figures/lv_ode_timeseries.png", p_lv_ts,
                width = 7, height = 4, dpi = 150)
ggplot2::ggsave("figures/lv_ode_phase.png", p_lv_phase,
                width = 5, height = 5, dpi = 150)

# ── 2. ABM: predator-prey ─────────────────────────────────────────────────────
cli::cli_h1("Predator-Prey ABM")

# seed = 18 on a 20x20 grid shows two clear boom-bust cycles without going
# extinct within 300 steps -- chosen deliberately so the figure is readable.
# Other seeds and longer runs frequently end in wolf extinction, which is the
# pedagogical point of Activity 1 and Section 4.5 (see Caddena.md for details).
abm_model <- PredatorPreyModel$new(seed = 18L, width = 20L, height = 20L)
abm_out   <- abm_model$run(steps = 300L)

cli::cli_alert_info("ABM: extinct = {abm_out$extinct}")
cli::cli_alert_info(
  "Note: running to step 400 with this seed shows wolf extinction (~step 341). \\
   Try seed = 42 or reduce width/height to 10 to observe extinction more quickly."
)

# Use the same colour convention as ODE (sheep ≈ prey, wolves ≈ predators)
abm_colours <- c(sheep = COURSE_PALETTE[["sd"]], wolves = COURSE_PALETTE[["abm"]])

p_abm_ts <- plot_time_series(
  abm_out$results,
  title      = "Predator-Prey ABM",
  colour_map = abm_colours
)

# ABM phase portrait
abm_wide  <- tidyr::pivot_wider(abm_out$results,
                                names_from  = "variable",
                                values_from = "value")
p_abm_phase <- plot_phase_portrait(
  data.frame(x = abm_wide$sheep, y = abm_wide$wolves),
  xlabel = "Sheep",
  ylabel = "Wolves",
  title  = "ABM phase portrait"
)

# ── 3. Side-by-side comparison ────────────────────────────────────────────────
cli::cli_h1("Comparison figures")

fig_ts    <- plot_side_by_side(p_lv_ts,    p_abm_ts,
                                title = "ODE vs ABM: population dynamics")
fig_phase <- plot_side_by_side(p_lv_phase, p_abm_phase,
                                title = "ODE vs ABM: phase portraits")

ggplot2::ggsave("figures/lv_comparison_timeseries.png", fig_ts,
                width = 12, height = 4, dpi = 150)
ggplot2::ggsave("figures/lv_comparison_phase.png", fig_phase,
                width = 10, height = 5, dpi = 150)

cli::cli_alert_success("Figures saved to figures/")


## ---- ACTIVITY 1: ODE vs ABM extinction (Section 2.6) ----------------------
## Instructions: course document Section 2.6. Expected time: ~20 minutes.
##
## Teaching point: The ODE predicts survival forever at any positive parameter
## value. The ABM reveals finite extinction probability. This is the mean-field
## fragility argument made directly observable.

## Part A — ODE behavior: Reduce prey birth rate
params_low_alpha <- list(alpha = 0.02, beta = 0.02, gamma = 0.3, delta = 0.01)
results_ode <- run_lv(params = params_low_alpha)
plot_time_series(results_ode$results,
                 title      = "ODE: low alpha",
                 colour_map = c(prey = COURSE_PALETTE[["sd"]],
                                predators = COURSE_PALETTE[["abm"]]))
# Q: Does the ODE ever reach zero? What does it predict long-run?

## Part B — ABM behavior with the same ecological regime
# Parameters to modify:
n_runs <- 20L   # try 5, 10, 20 for speed vs precision
steps  <- 500L
width  <- 20L   # try 10, 50
height <- 20L

extinction_probs <- find_abm_extinction_probability(
  params = params_low_alpha,
  n_runs = n_runs,
  steps  = steps,
  width  = width,
  height = height
)
print(extinction_probs$runs)
cli::cli_alert_info(
  "Extinction probability: {round(extinction_probs$extinction_prob * 100, 1)}% \\
   ({sum(extinction_probs$runs$extinct)} / {n_runs} runs)"
)
cli::cli_alert_info(
  "Mean time to extinction: {round(extinction_probs$mean_time_to_extinction, 1)} steps"
)
# Q: What fraction of ABM runs go extinct?
#    Why does the ODE not predict this?

# Extension (fast finishers): Vary alpha from 0.02 to 0.2 in steps of 0.02.
# For each value, run 10 ABM replicates and plot extinction probability vs alpha.
# Is there a sharp threshold, or a gradual transition?
# YOUR CODE HERE
