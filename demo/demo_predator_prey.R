## demo_predator_prey.R
## Compares the baseline predator-prey ABM with the grass-extension ABM,
## and shows both alongside the Lotka-Volterra ODE.
## Figures: population time series; baseline vs grass comparison.
##
## Run from the package root:
##   Rscript demo/demo_predator_prey.R

pkgload::load_all(".", quiet = TRUE)

if (!dir.exists("figures")) dir.create("figures")

# ABM runs use a 20x20 grid with seed = 18 (baseline) / seed = 10 (grass) to
# show clear oscillation dynamics within 300 steps without hitting extinction
# mid-figure. Other seeds and longer runs frequently end in wolf extinction --
# this is the key ABM vs ODE contrast and the topic of Activities 2 and 5.
# See Caddena.md for the full seed-search rationale.
ABM_STEPS  <- 300L
ABM_WIDTH  <- 20L
ABM_HEIGHT <- 20L

# ── 1. ODE: Lotka-Volterra (reference) ───────────────────────────────────────
cli::cli_h1("Lotka-Volterra ODE (reference)")

lv     <- run_lv(params = LV_DEFAULTS)
lv_eq  <- lv$equilibrium
lv_colours <- c(prey = COURSE_PALETTE[["sd"]], predators = COURSE_PALETTE[["abm"]])

p_ode <- plot_time_series(
  lv$results,
  title      = "ODE: prey and predators",
  colour_map = lv_colours
)

cli::cli_alert_info(
  "ODE equilibrium: prey* = {round(lv_eq['prey'], 1)}, \\
   predators* = {round(lv_eq['predators'], 1)}"
)

# ── 2. ABM: baseline (no grass) ───────────────────────────────────────────────
cli::cli_h1("Predator-Prey ABM (baseline)")

baseline <- PredatorPreyModel$new(
  seed = 18L, width = ABM_WIDTH, height = ABM_HEIGHT
)
base_out <- baseline$run(steps = ABM_STEPS)

abm_colours <- c(sheep = COURSE_PALETTE[["sd"]], wolves = COURSE_PALETTE[["abm"]])

p_base <- plot_time_series(
  base_out$results,
  title      = sprintf("ABM baseline: sheep and wolves (%dx%d, seed=18)", ABM_WIDTH, ABM_HEIGHT),
  colour_map = abm_colours
)

final_sheep  <- base_out$results$value[base_out$results$step == ABM_STEPS &
                                         base_out$results$variable == "sheep"]
final_wolves <- base_out$results$value[base_out$results$step == ABM_STEPS &
                                         base_out$results$variable == "wolves"]
cli::cli_alert_info(
  "Baseline ABM: extinct = {base_out$extinct}, \\
   step {ABM_STEPS}: sheep = {final_sheep}, wolves = {final_wolves}"
)
cli::cli_alert_info(
  "Note: wolves go extinct around step 341 with this seed. \\
   The ODE predicts stable cycles indefinitely -- this contrast is Activity 2."
)

# ODE vs baseline ABM side by side
fig_ode_vs_abm <- plot_side_by_side(
  p_ode, p_base,
  title = "Lotka-Volterra ODE vs predator-prey ABM"
)

ggplot2::ggsave("figures/pp_ode_vs_abm.png", fig_ode_vs_abm,
                width = 12, height = 4, dpi = 150)


## ---- ACTIVITY 2: Spatial density and finite-population effects (Section 4.6)
## Instructions: course document Section 4.6. Expected time: ~20 minutes.
##
## Teaching point: The mean-field assumption is a density assumption — infinite
## dilution, uniform mixing. Holding population constant and varying grid size
## isolates spatial density as the variable and shows how the ODE approximation
## degrades as density increases.

# Parameters to modify:
n_sheep  <- 100L   # fixed population across all three grids
n_wolves <- 25L

results_sparse <- PredatorPreyModel$new(
  width = 100L, height = 100L,
  n_sheep = n_sheep, n_wolves = n_wolves, seed = 42L
)$run(steps = 300L)

results_medium <- PredatorPreyModel$new(
  width = 50L, height = 50L,
  n_sheep = n_sheep, n_wolves = n_wolves, seed = 42L
)$run(steps = 300L)

results_dense <- PredatorPreyModel$new(
  width = 20L, height = 20L,
  n_sheep = n_sheep, n_wolves = n_wolves, seed = 42L
)$run(steps = 300L)

results_ode <- run_lv()

plot_abm_ode_comparison(
  list(sparse  = results_sparse,
       medium  = results_medium,
       dense   = results_dense),
  ode = results_ode
)
# Q: Which grid size produces dynamics closest to the ODE?
#    What happens to extinction risk as density increases?

# Extension (fast finishers): Run each grid size 10 times with seeds 1:10.
# At which grid size does extinction probability first exceed 10%?
# Does the ODE give any warning that this is coming?
# YOUR CODE HERE


# ── 3. ABM: grass extension ───────────────────────────────────────────────────
cli::cli_h1("Predator-Prey ABM with grass")

# seed = 10 on 20x20: wolves linger to step ~230, showing how grass carrying
# capacity changes the collapse mechanism relative to the baseline.
grass_model <- PredatorPreyGrassModel$new(
  seed = 10L, width = ABM_WIDTH, height = ABM_HEIGHT
)
grass_out   <- grass_model$run(steps = ABM_STEPS)

# Separate animal populations from grass fraction for clarity
animal_results <- grass_out$results[grass_out$results$variable != "grass_fraction", ]
grass_results  <- grass_out$results[grass_out$results$variable == "grass_fraction", ]

p_grass_animals <- plot_time_series(
  animal_results,
  title      = sprintf("ABM grass: sheep and wolves (%dx%d, seed=10)", ABM_WIDTH, ABM_HEIGHT),
  colour_map = abm_colours
)

p_grass_frac <- plot_time_series(
  grass_results,
  title      = "ABM grass: grass cover fraction",
  colour_map = c(grass_fraction = COURSE_PALETTE[["extended"]])
)

final_grass <- round(grass_results$value[nrow(grass_results)], 2)
cli::cli_alert_info(
  "Grass ABM: extinct = {grass_out$extinct}, \\
   final grass fraction = {final_grass}"
)
cli::cli_alert_info(
  "Grass model: wolves exhaust food and die around step 230. \\
   Sheep stabilise at a grass-limited carrying capacity (~25 animals). \\
   Compare: baseline wolves die from prey crash; grass wolves die from sparse prey."
)

# Grass-extension: animals + grass fraction
fig_grass <- plot_side_by_side(
  p_grass_animals, p_grass_frac,
  title = "ABM grass extension: animals and grass cover"
)

# Baseline vs grass comparison
fig_base_vs_grass <- plot_side_by_side(
  p_base, p_grass_animals,
  title = "ABM: baseline vs grass extension"
)

ggplot2::ggsave("figures/pp_grass_extension.png", fig_grass,
                width = 12, height = 4, dpi = 150)
ggplot2::ggsave("figures/pp_base_vs_grass.png", fig_base_vs_grass,
                width = 12, height = 4, dpi = 150)

cli::cli_alert_success("Figures saved to figures/")


## ---- ACTIVITY 5: Grass as a buffer and the extinction optimum (Section 7.4)
## Instructions: course document Section 7.4. Expected time: ~20 minutes.
##
## Teaching point: Adding a third trophic level changes not just cycle amplitude
## but extinction risk — and there is an optimal grass regrowth rate that
## minimises extinction probability. This is a non-obvious optimum that no ODE
## would predict without restructuring.

## Part A — Direct comparison: baseline vs grass
results_baseline <- PredatorPreyModel$new(
  seed = 18L, width = ABM_WIDTH, height = ABM_HEIGHT
)$run(steps = ABM_STEPS)
results_grass_a5 <- PredatorPreyGrassModel$new(
  seed = 10L, width = ABM_WIDTH, height = ABM_HEIGHT
)$run(steps = ABM_STEPS)

animal_res <- results_grass_a5$results[
  results_grass_a5$results$variable != "grass_fraction", ]

plot_side_by_side(
  plot_time_series(results_baseline$results,
                   title      = "Baseline: no grass",
                   colour_map = c(sheep = COURSE_PALETTE[["sd"]],
                                  wolves = COURSE_PALETTE[["abm"]])),
  plot_time_series(animal_res,
                   title      = "With grass",
                   colour_map = c(sheep = COURSE_PALETTE[["sd"]],
                                  wolves = COURSE_PALETTE[["abm"]]))
)
# Q: How does cycle amplitude change?
#    How does extinction mechanism differ (baseline: wolf overshoot;
#    grass: wolf food starvation)?

## Part B — Regrowth rate optimum
# Parameters to modify:
regrowth_times <- c(5L, 15L, 30L, 60L, 120L)   # try adding 10L, 45L, 90L
n_runs_a5      <- 20L                            # try 5L for speed

extinction_by_regrowth <- find_grass_extinction_curve(
  regrowth_times = regrowth_times,
  n_runs         = n_runs_a5,
  steps          = ABM_STEPS,
  width          = ABM_WIDTH,
  height         = ABM_HEIGHT
)
print(extinction_by_regrowth)
plot_extinction_curve(extinction_by_regrowth)
# Q: At which regrowth time is extinction probability lowest?
#    Why does very fast regrowth not eliminate extinction risk?
#    Why does very slow regrowth increase it?

# Extension (fast finishers): Reduce initial sheep to 50 and wolves to 12.
# Does the optimal regrowth time shift? What does this suggest about
# conservation policy in small populations?
# YOUR CODE HERE
