# Addendum — Activity Redesigns

This file supersedes the activity specifications in `CLAUDE.md` and the
activity table in the course schedule section. Implement these designs
in the demo scripts, course document activity subsections, and slide
activity frames. The minimal-code principle applies throughout: students
call provided functions and modify parameter values only. They do not
write logic, loops, or custom functions.

---

## General principle

Every activity script section should be structured as:

1. A named parameter block students can modify
2. Pre-written function calls that use those parameters
3. A comparison or summary output that makes the result immediately
   visible

Students change one or two numbers, re-run the block, and interpret
the output. The insight comes from observation and interpretation,
not from writing code.

---

## Activity 1: ODE vs ABM extinction (Section 2.6)

**Replaces:** "Modify LV parameters; find a parameterization that causes
extinction"

**Teaching point:** The ODE predicts survival forever at any positive
parameter value; the ABM reveals finite extinction probability. This is
the mean-field fragility argument made directly observable.

**Structure:**

Part A — ODE behavior:
```r
# Reduce prey birth rate progressively
params_low_alpha <- list(alpha = 0.02, beta = 0.02, gamma = 0.3, delta = 0.01)
results_ode <- run_lv(params = params_low_alpha)
plot_time_series(results_ode$results, title = "ODE: low alpha")
# Q: Does the ODE ever reach zero? What does it predict long-run?
```

Part B — ABM behavior with the same parameters:
```r
extinction_probs <- find_abm_extinction_probability(
  params = params_low_alpha,
  n_runs = 20,
  steps  = 500L
)
print(extinction_probs)
# Q: What fraction of ABM runs go extinct?
#    Why does the ODE not predict this?
```

**Required helper function** (add to `R/utils_plotting.R` or a new
`R/utils_activities.R`):

```r
#' Estimate ABM extinction probability across replicate runs
#'
#' @param params Named list of LV parameters.
#' @param n_runs Number of independent replications.
#' @param steps Number of steps per run.
#' @param width,height Grid dimensions.
#' @param n_sheep,n_wolves Initial population sizes.
#' @return Named list:
#'   - extinction_prob: fraction of runs where either population hit 0
#'   - mean_time_to_extinction: mean step at extinction (NA if no extinction)
#'   - runs: tibble with one row per run, columns seed, extinct, extinct_step
find_abm_extinction_probability <- function(
  params,
  n_runs   = 20L,
  steps    = 500L,
  width    = 50L,
  height   = 50L,
  n_sheep  = 100L,
  n_wolves = 25L
)
```

**Extension question for fast finishers:** At what value of `alpha` does
extinction probability exceed 50% across 20 runs? Is there a sharp
threshold or a gradual transition?

**Course document subsection:** Section 2.6. Include a figure showing
the ODE trajectory (never reaching zero) alongside a histogram of
ABM extinction steps across 20 runs.

---

## Activity 2: Spatial density and finite-population effects (Section 4.6)

**Replaces:** "Compare ABM and ODE outputs; change grid size and observe
finite-population effects"

**Teaching point:** The mean-field assumption is fundamentally a density
assumption — infinite dilution, uniform mixing. Holding population
constant and varying grid size isolates spatial density as the variable
and shows how the ODE approximation degrades as density increases.

**Structure:**

```r
# Fixed population, three grid sizes
n_sheep  <- 100L
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

# Compare all three against the ODE
results_ode <- run_lv()
plot_abm_ode_comparison(
  list(sparse = results_sparse,
       medium = results_medium,
       dense  = results_dense),
  ode = results_ode
)
# Q: Which grid size produces dynamics closest to the ODE?
#    What happens to extinction risk as density increases?
```

**Required helper function:**

```r
#' Plot multiple ABM runs against an ODE trajectory
#'
#' @param abm_list Named list of ABM run results (from PredatorPreyModel$run())
#' @param ode Result from run_lv()
#' @param theme_fn ggplot2 theme function
#' @return A patchwork ggplot object
plot_abm_ode_comparison <- function(abm_list, ode, theme_fn = theme_course_light)
```

**Extension question for fast finishers:** Run each grid size 10 times
with different seeds. At which grid size does extinction probability
first exceed 10%? Does the ODE give any warning that this is coming?

**Course document subsection:** Section 4.6. The key figure shows three
ABM trajectories (sparse, medium, dense) alongside the ODE on the same
axes, making the density-dependence of the mean-field approximation
visually explicit.

---

## Activity 3: Stochastic variation in epidemic outcomes (Section 5.6)

**Replaces:** "Run 10 epidemic simulations; compute empirical R₀ and
compare to theoretical"

**Teaching point:** The ODE gives one epidemic curve; the ABM gives a
distribution. Peak infected fraction varies across runs in ways the ODE
cannot represent. Statistical thinking — summarising a distribution,
not reading off a point — is the right framework for ABM output.

**Structure:**

```r
# Run the pandemic model 10 times
seeds <- 1:10
runs  <- lapply(seeds, function(s) {
  PandemicModel$new(seed = s)$run(steps = 200L)
})

# Extract peak infected fraction from each run
peak_I_distribution <- summarise_runs(runs)
print(peak_I_distribution)

# Compare mean ABM trajectory to ODE
results_ode <- run_isr()
plot_epidemic_comparison(runs, ode = results_ode)

# Q: How much does peak_I vary across runs?
#    Does the ODE peak fall within the ABM distribution?
#    What does this imply for policy decisions based on the ODE peak?
```

**Required helper functions:**

```r
#' Summarise key statistics across a list of pandemic model runs
#'
#' @param runs List of results from PandemicModel$run()
#' @return Tibble with columns: run, peak_I, peak_step, final_R,
#'   R0_empirical. Includes mean and SD rows.
summarise_runs <- function(runs)

#' Plot mean ABM epidemic curve with +/- 1 SD band against ODE
#'
#' @param runs List of results from PandemicModel$run()
#' @param ode Result from run_isr()
#' @param theme_fn ggplot2 theme function
plot_epidemic_comparison <- function(runs, ode, theme_fn = theme_course_light)
```

**Extension question for fast finishers:** Increase `initial_infected_fraction`
from 0.01 to 0.05. Does the variance across runs increase or decrease?
Why? What does this imply about epidemic prediction early vs late in an
outbreak?

**Course document subsection:** Section 5.6. The key figure is the mean
ABM trajectory with ±1 SD band overlaid on the ODE curve — the same
figure used in `demo_isr.R`.

---

## Activity 4: Hub seeding and epidemic shape (Section 6.7)

**Replaces:** "Vary n_edges_per_new_node; observe effect on epidemic
curve shape"

**Teaching point:** On a heterogeneous network, *who* gets infected first
matters as much as *how many*. Seeding the epidemic in high-degree hub
nodes versus peripheral low-degree nodes produces dramatically different
epidemic curves even with identical parameters — making the super-spreader
mechanism viscerally observable.

**Required model modification:** Add `seed_hubs` parameter to
`NetworkPandemicModel`:

```r
initialize = function(
  n_agents                  = 500L,
  n_edges_per_new_node      = 3L,
  initial_infected_fraction = 0.01,
  beta                      = 0.3,
  gamma                     = 0.05,
  seed_hubs                 = FALSE,   # NEW: if TRUE, infect highest-degree
                                       # nodes first rather than at random
  seed                      = NULL
)
```

When `seed_hubs = TRUE`, the initially infected agents are drawn from
the top `ceiling(initial_infected_fraction * n_agents)` nodes by degree
rather than uniformly at random.

**Structure:**

```r
# Same parameters, different seeding strategy
results_random <- NetworkPandemicModel$new(
  seed_hubs = FALSE, seed = 42L
)$run(steps = 200L)

results_hubs <- NetworkPandemicModel$new(
  seed_hubs = TRUE, seed = 42L
)$run(steps = 200L)

# Compare epidemic curves
plot_side_by_side(
  plot_time_series(results_random$results,
                   title = "Random seeding"),
  plot_time_series(results_hubs$results,
                   title = "Hub seeding")
)

# Compare attack rates by node type
print(results_random$super_spreader_attack_rate)
print(results_hubs$super_spreader_attack_rate)

# Q: Which seeding strategy produces a faster-rising epidemic?
#    Which produces a higher peak?
#    What does this imply for early containment policy?
```

**Extension question for fast finishers:** Run both seeding strategies
10 times each. Does hub seeding always produce a faster epidemic, or
does stochastic variation sometimes reverse the result? At what network
density (vary `n_edges_per_new_node`) does the seeding strategy matter
most?

**Course document subsection:** Section 6.7. The key figure shows the
two epidemic curves side by side; a second figure shows the
degree distribution with the seeded nodes highlighted.

---

## Activity 5: Grass as a buffer and the extinction optimum (Section 7.4)

**Replaces:** "Add grass; compare cycle amplitude and extinction
probability to baseline"

**Teaching point:** Adding a third trophic level (grass) changes not
just cycle amplitude but extinction risk — and there is an optimal
grass regrowth rate that minimises extinction probability. This
demonstrates the payoff of ABM extensibility: a new ecological
mechanism is one parameter away, and it produces a non-obvious
optimum that no ODE would predict without restructuring.

**Structure:**

Part A — Direct comparison:
```r
results_baseline <- PredatorPreyModel$new(seed = 42L)$run(steps = 500L)
results_grass    <- PredatorPreyGrassModel$new(seed = 42L)$run(steps = 500L)

plot_side_by_side(
  plot_time_series(results_baseline$results,
                   title = "Baseline: no grass"),
  plot_time_series(results_grass$results,
                   title = "With grass")
)
# Q: How does cycle amplitude change?
#    How does extinction risk appear to change?
```

Part B — Regrowth rate optimum:
```r
# Vary grass regrowth time and measure extinction probability
regrowth_times <- c(5L, 15L, 30L, 60L, 120L)
extinction_by_regrowth <- find_grass_extinction_curve(
  regrowth_times = regrowth_times,
  n_runs         = 20L,
  steps          = 500L
)
plot_extinction_curve(extinction_by_regrowth)
# Q: At which regrowth time is extinction probability lowest?
#    Why does very fast regrowth not eliminate extinction risk?
#    Why does very slow regrowth increase it?
```

**Required helper functions:**

```r
#' Compute extinction probability across grass regrowth times
#'
#' @param regrowth_times Integer vector of grass_regrowth_time values to try.
#' @param n_runs Number of replications per regrowth time.
#' @param steps Steps per run.
#' @return Tibble with columns: regrowth_time, extinction_prob, se.
find_grass_extinction_curve <- function(
  regrowth_times,
  n_runs = 20L,
  steps  = 500L
)

#' Plot extinction probability as a function of grass regrowth time
#'
#' @param results Tibble from find_grass_extinction_curve().
#' @param theme_fn ggplot2 theme function.
plot_extinction_curve <- function(results, theme_fn = theme_course_light)
```

**Extension question for fast finishers:** Does the optimal regrowth
time change when you start with a smaller initial population
(`n_sheep = 50`, `n_wolves = 12`)? What does this suggest about
conservation policy in small populations?

**Course document subsection:** Section 7.4. Two figures: the
baseline vs. grass comparison time series, and the extinction
probability curve across regrowth times with the optimum marked.

---

## Summary of helper functions to add

All helper functions below should be added to a new file
`R/utils_activities.R`. They are exported functions, fully documented
with roxygen2, and follow all package conventions.

| Function | Used in |
|---|---|
| `find_abm_extinction_probability()` | Activity 1 |
| `plot_abm_ode_comparison()` | Activity 2 |
| `summarise_runs()` | Activity 3 |
| `plot_epidemic_comparison()` | Activity 3 |
| `find_grass_extinction_curve()` | Activity 5 |
| `plot_extinction_curve()` | Activity 5 |

Activity 4 requires only the `seed_hubs` parameter addition to
`NetworkPandemicModel` — no new helper functions beyond those already
specified.

Add `R/utils_activities.R` to the repository layout and to the build
order (after step 8, before step 9 in `CLAUDE.md`).

---

## Revised activity arc summary

| Activity | Core manipulation | Key insight |
|---|---|---|
| 1 | Reduce alpha; compare ODE survival to ABM extinction probability | Mean-field fragility: ODE predicts forever, ABM reveals hitting-time |
| 2 | Fix N, vary grid size; compare to ODE | Mean-field is a density assumption; approximation degrades with density |
| 3 | Run 10 epidemic replicates; examine peak_I distribution | ABM output is a distribution, not a point; statistical thinking required |
| 4 | Seed hubs vs random; compare epidemic curves | Who gets infected first matters; super-spreader mechanism made visible |
| 5 | Compare baseline to grass; find optimal regrowth rate | ABM extensibility payoff; non-obvious optimum from local interaction |