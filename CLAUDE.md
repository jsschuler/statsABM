# ABM for Statisticians — Build Instructions for Claude Code

## Project purpose

This codebase supports a day-long short course titled **Agent-Based Modeling for
Statisticians**. It demonstrates each model in two paradigms — system dynamics (ODE)
and agent-based — so that students can compare them directly. The code must be clean
enough to run live during the course and readable enough to serve as reference
material afterward.

The audience is practicing statisticians fluent in R. Code should feel idiomatic —
tidyverse-style data handling, ggplot2 for all visualization, R6 classes for model
objects.

---

## Technology stack

| Layer | Choice | Rationale |
|---|---|---|
| Language | R 4.3+ | Native to the audience |
| ODE solver | `deSolve` | Standard, well-documented, familiar interface |
| ABM framework | R6 classes (`R6` package) | Pythonic-style OOP; transferable pattern |
| Networks | `igraph` | Power-law graph generation, degree analysis |
| Visualization | `ggplot2` + `patchwork` | Publication-quality, composable |
| Data wrangling | `dplyr`, `tidyr` | Idiomatic R |
| Power law fitting | `poweRlaw` | Clauset et al. 2009 method |
| Progress reporting | `cli` | Clean console output during long runs |
| Testing | `testthat` | Standard R package testing |
| Package structure | Standard R package (`DESCRIPTION`, `NAMESPACE`) | `devtools::load_all()` workflow |

**ABM framework note:** R does not have a dominant ABM framework equivalent to Mesa.
Use **R6 classes** from the `R6` package for agents and models. This gives students
a transferable OOP pattern without requiring a specialized ABM package. If a student
asks about dedicated ABM packages, mention `RNetLogo` (interface to NetLogo) and
`ABM` (lightweight), but do not use them here.

Do **not** introduce dependencies beyond those listed without explicit approval.
The package must install cleanly via `devtools::install()`.

---

## Repository layout

Build this exact structure. Do not add files or directories not listed here without
asking first.

```
abmcourse/
├── DESCRIPTION
├── NAMESPACE
├── README.md
├── CLAUDE.md                        <- this file (copy into repo root)
├── .Rbuildignore
├── .gitignore                       <- must include: figures/, *.tex, *.pdf
|
├── R/
│   ├── utils_plotting.R             <- shared ggplot2 helpers + both themes
│   ├── utils_stats.R                <- power law fit, Gini, KS test wrappers
│   ├── utils_axtell.R               <- load + plot Axtell's firm-size results
│   │
│   ├── sd_lotka_volterra.R          <- ODE predator-prey
│   ├── sd_isr_pandemic.R            <- ODE ISR compartmental model
│   │
│   ├── abm_predator_prey.R          <- ABM wolves + sheep (baseline)
│   ├── abm_predator_prey_grass.R    <- ABM wolves + sheep + grass (extension)
│   ├── abm_pandemic.R               <- ABM ISR on well-mixed grid (baseline)
│   ├── abm_pandemic_network.R       <- ABM ISR on social network + super-spreaders
│   └── utils_axtell.R               <- load + plot Axtell's firm-size results
│   ├── lv.R                         <- Lotka-Volterra chunks (light + dark variants)
│   ├── isr.R                        <- ISR pandemic chunks
│   ├── abm_predator_prey.R          <- predator-prey ABM chunks
│   ├── abm_pandemic.R               <- pandemic ABM chunks
│   ├── abm_network.R                <- network extension chunks
│   ├── abm_grass.R                  <- grass extension chunks
│   └── axtell.R                     <- Axtell presentation chunks
│
├── demo/
│   ├── demo_lotka_volterra.R
│   ├── demo_isr.R
│   ├── demo_predator_prey.R
│   ├── demo_pandemic.R
│   └── demo_axtell.R
│
├── vignettes/
│   ├── abm_for_statisticians.Rnw    <- main course document (LaTeX + knitr)
│   ├── abm_slides.Rnw               <- beamer slide deck (LaTeX + knitr)
│   └── references.bib               <- BibTeX references (shared by both)
│
├── data-raw/
│   └── census_firm_sizes.R          <- script to fetch/clean Census firm data
│
├── data/
│   └── census_firm_sizes.rda        <- cleaned Census firm-size data (usethis::use_data)
│
└── tests/
    └── testthat/
        ├── test-sd.R
        ├── test-abm-predator-prey.R
        └── test-abm-pandemic.R
```

The `.gitignore` must include at minimum:
```
figures/
*.tex
*.pdf
*.aux
*.log
```
Generated figures and compiled LaTeX artifacts are not committed.

The `README.md` must include:
1. One-paragraph description of the course and codebase.
2. Installation instructions: `devtools::install_github(...)` or
   `devtools::install()` from a local clone.
3. How to run a demo: `Rscript demo/demo_lotka_volterra.R`
4. How to find activity starting points: search for `ACTIVITY` in any
   demo script, or see the course document for instructions.
5. How to compile the course document and slides (the two `knitr::knit()`
   + `pdflatex` sequences).
6. A note that Axtell's data files are not included in the repository and
   must be obtained separately.

```
Package: abmcourse
Title: Agent-Based Modeling for Statisticians
Version: 0.1.0
Authors@R: person("First", "Last", role = c("aut", "cre"),
    email = "you@example.com")
Description: Course materials comparing system dynamics and agent-based
    models for predator-prey, epidemic, and firm-size applications.
License: MIT + file LICENSE
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
RoxygenNote: 7.3.1
Imports:
    R6,
    deSolve,
    igraph,
    ggplot2,
    patchwork,
    dplyr,
    tidyr,
    poweRlaw,
    cli
Suggests:
    testthat (>= 3.0.0),
    devtools,
    pkgload,
    here
Config/testthat/edition: 3
```

---

## Conventions applying to all R code

- **Roxygen2 documentation** on every exported function. Include `@param`, `@return`,
  `@examples`.
- **No global state.** Model objects carry all state. No assignments to `.GlobalEnv`
  from library code.
- **Reproducibility.** Every stochastic function accepts a `seed = NULL` parameter.
  Set it with `set.seed(seed)` at the top of the function when non-NULL.
- **Return values are named lists** (or R6 objects with a `$results` field). Never
  return bare vectors from top-level model functions.
- **tidyverse-style data frames** for time series output — long format, suitable for
  direct piping into ggplot2. Model run functions should return a tibble with a
  `step` or `time` column and `variable` / `value` columns where multiple series
  are present.
- **Chunk file package loading**: chunk files load the package via
  `pkgload::load_all(here::here())` at the top of the first chunk in each
  file. Do not use `library(abmcourse)` in chunk files — this requires an
  installed version and will silently use stale code during development.
  Add `pkgload` and `here` to `Suggests` in `DESCRIPTION`.
- **Activity scaffolding in demo scripts**: the activity block in each demo
  script must define all modifiable parameters as named variables immediately
  above the `ACTIVITY` comment, followed by `# YOUR CODE HERE` stubs for
  each numbered step. Students see the full demo code above and learn from
  it; the activity section gives them a clean entry point. Example pattern:

```r
## ---- ACTIVITY 1: Finding extinction boundaries ------------------------------
## Instructions: course document Section 2.6. Expected time: ~20 minutes.

# Parameters to modify:
alpha <- 0.1   # try reducing this toward zero
beta  <- 0.02
gamma <- 0.3
delta <- 0.01

# Step 1: Run the model with the default parameters and note the trajectory.
# YOUR CODE HERE

# Step 2: Reduce alpha until you observe extinction. What is the threshold?
# YOUR CODE HERE

# Extension (fast finishers): How does extinction probability change with
# initial population size? Vary n_sheep and n_wolves and run 10 replicates.
# YOUR CODE HERE
```

---

## Module specifications

Build modules in the order listed in the Build Order section. Each specification
gives the public interface; implementation details are left to you unless
constrained.

---

### 1. `R/utils_plotting.R`

All plots use ggplot2. Define package-level palette and theme constants:

```r
COURSE_PALETTE <- c(
  sd       = "#2E86AB",   # blue  -- system dynamics
  abm      = "#A23B72",   # plum  -- ABM baseline
  extended = "#F18F01",   # amber -- ABM extension
  data     = "#4B8B3B"    # green -- empirical data
)

# Slide background and accent colours (shared with beamer theme)
SLIDE_BG      <- "#1B2A4A"   # dark navy
SLIDE_ACCENT  <- "#F18F01"   # amber -- frame titles, block borders
SLIDE_TEXT    <- "#E8E8E8"   # off-white body text
SLIDE_PANEL   <- "#1E3A5F"   # slightly lighter navy for code panels
```

Two ggplot2 theme functions are required. Both must produce visually identical
layouts — axis structure, legend position, font sizes — differing only in
background and text colours. This ensures figures are comparable across the
course document and slides.

```r
#' Light ggplot2 theme for the course document
#' @return A ggplot2 theme object.
theme_course_light <- function()
# Based on theme_minimal(). White panel background, dark axis text,
# light gray grid lines. Used in all course document chunks.

#' Dark ggplot2 theme for the beamer slides
#' @return A ggplot2 theme object.
theme_course_dark <- function()
# Panel background: SLIDE_BG. Grid lines: slightly lighter navy.
# Axis text, legend text, strip text: SLIDE_TEXT.
# Used in all slide chunks. Must produce figures that are legible
# on a projected dark background.
```

Required functions (all return a `ggplot` object):

```r
#' Plot time series from a model results tibble
#'
#' @param results A long-format tibble with columns: time (or step),
#'   variable, value.
#' @param title Plot title string.
#' @param colour_map Named character vector mapping variable names to hex
#'   colours. Defaults to COURSE_PALETTE.
#' @param log_y Logical; use log scale on y-axis.
#' @return A ggplot object.
plot_time_series <- function(results, title, colour_map = NULL, log_y = FALSE)

#' Plot a phase portrait (x vs y trajectory)
#'
#' @param results Tibble with columns x and y (e.g., prey and predators).
#' @param xlabel,ylabel Axis label strings.
#' @param title Plot title.
plot_phase_portrait <- function(results, xlabel, ylabel, title)

#' Plot an empirical distribution, optionally on log-log axes
#'
#' @param values Numeric vector of observed values.
#' @param title Plot title.
#' @param log_log Logical; plot on log-log axes.
#' @param fit Optional list with elements slope and intercept for a
#'   reference power-law line.
plot_distribution <- function(values, title, log_log = FALSE, fit = NULL)

#' Combine two ggplot objects side by side using patchwork
#'
#' @param left,right ggplot objects.
#' @param title Overall title string (patchwork annotation).
plot_side_by_side <- function(left, right, title = NULL)
```

---

### 2. `R/utils_stats.R`

```r
#' Fit a discrete power law using the poweRlaw package (Clauset et al. 2009)
#'
#' @param x Integer or numeric vector of positive values.
#' @return Named list: alpha, xmin, ks_statistic, ks_p_value, n.
fit_power_law <- function(x)

#' Compute the Gini coefficient
#'
#' @param x Non-negative numeric vector.
#' @return Scalar in [0, 1].
gini <- function(x)

#' Compare two distributions via KS test and summary statistics
#'
#' @param simulated,empirical Numeric vectors.
#' @return Named list: ks_statistic, ks_p_value, simulated_mean,
#'   empirical_mean, simulated_gini, empirical_gini.
compare_distributions <- function(simulated, empirical)
```

---

### 3. `R/sd_lotka_volterra.R`

Use `deSolve::ode()` to integrate the Lotka-Volterra system.

```r
# Default parameters (document ecological meaning in roxygen)
LV_DEFAULTS <- list(
  alpha = 0.1,    # prey birth rate
  beta  = 0.02,   # predation rate (prey removed per predator per prey)
  gamma = 0.3,    # predator death rate
  delta = 0.01    # predator birth rate per prey eaten
)

#' Run the Lotka-Volterra ODE system
#'
#' @param params Named list of parameters. Defaults to LV_DEFAULTS.
#' @param y0 Named numeric vector c(prey = 40, predators = 9).
#' @param t_span Numeric vector c(start, end). Default c(0, 200).
#' @param n_points Number of output time points. Default 2000.
#' @return Named list:
#'   - results: long-format tibble with columns time, variable, value
#'       (variable is "prey" or "predators")
#'   - equilibrium: named vector c(prey = ..., predators = ...)
#'   - params: the params list used
run_lv <- function(params = LV_DEFAULTS, y0 = c(prey=40, predators=9),
                   t_span = c(0, 200), n_points = 2000)

#' Compute the non-trivial Lotka-Volterra equilibrium
#'
#' @param params Named list with alpha, beta, gamma, delta.
#' @return Named vector c(prey = gamma/delta, predators = alpha/beta).
lv_equilibrium <- function(params)
```

---

### 4. `R/sd_isr_pandemic.R`

Note: **ISR** ordering (Infected, Susceptible, Recovered) — do not reorder to SIR.

```r
ISR_DEFAULTS <- list(
  beta  = 0.3,    # transmission rate
  gamma = 0.05    # recovery rate
  # R0 = beta / gamma = 6 (measles-like; produces a dramatic curve)
)

#' Run the ISR compartmental ODE model
#'
#' @param params Named list. Defaults to ISR_DEFAULTS.
#' @param y0 Named vector c(I=0.01, S=0.98, R=0.01) of fractions.
#' @param t_span Numeric vector c(0, 160).
#' @param n_points Integer. Default 1600.
#' @return Named list:
#'   - results: long tibble with columns time, variable, value
#'       (variable in c("I","S","R"))
#'   - R0: scalar
#'   - peak_I: peak infected fraction
#'   - peak_time: time of peak
#'   - final_R: fraction recovered at end of simulation
#'   - params: params list used
run_isr <- function(params = ISR_DEFAULTS,
                    y0 = c(I=0.01, S=0.98, R=0.01),
                    t_span = c(0, 160),
                    n_points = 1600)

#' Herd immunity threshold
#'
#' @param params Named list with beta and gamma.
#' @return Scalar 1 - 1/R0.
herd_immunity_threshold <- function(params)
```

---

### 5. `R/abm_predator_prey.R`

R6-based wolf-sheep predator-prey ABM. This is the **baseline** ABM.

```r
#' Sheep agent
#'
#' @description Grazes on a plain grid. Reproduces probabilistically.
#'   Dies when energy reaches zero.
Sheep <- R6::R6Class("Sheep",
  public = list(
    id        = NULL,
    energy    = NULL,
    x         = NULL,   # grid column (1-indexed)
    y         = NULL,   # grid row (1-indexed)
    alive     = TRUE,

    initialize = function(id, x, y, energy),
    step = function(model)
    # step() logic:
    #   1. Move to a random Moore neighbor (wrap at grid edges -- torus).
    #   2. Lose 1 energy.
    #   3. If energy > sheep_reproduce_threshold: reproduce with
    #      probability sheep_reproduce (spawn offspring at same cell,
    #      each gets half of parent energy).
    #   4. Set alive = FALSE if energy <= 0.
  )
)

#' Wolf agent
Wolf <- R6::R6Class("Wolf",
  public = list(
    id        = NULL,
    energy    = NULL,
    x         = NULL,
    y         = NULL,
    alive     = TRUE,

    initialize = function(id, x, y, energy),
    step = function(model)
    # step() logic:
    #   1. Move to random Moore neighbor (torus).
    #   2. Lose 1 energy.
    #   3. If any Sheep at current cell: eat one (remove it, gain
    #      wolf_gain_from_food energy).
    #   4. Reproduce with probability wolf_reproduce if energy >
    #      wolf_reproduce_threshold.
    #   5. Set alive = FALSE if energy <= 0.
  )
)

#' Predator-prey ABM
#'
#' @description Baseline wolf-sheep model on a toroidal grid.
PredatorPreyModel <- R6::R6Class("PredatorPreyModel",
  public = list(
    width                    = NULL,
    height                   = NULL,
    params                   = NULL,
    sheep                    = NULL,   # list of Sheep R6 objects
    wolves                   = NULL,   # list of Wolf R6 objects
    step_count               = 0L,
    history                  = NULL,   # tibble built up by run()

    initialize = function(
      width                     = 50L,
      height                    = 50L,
      n_sheep                   = 100L,
      n_wolves                  = 25L,
      sheep_reproduce           = 0.04,
      wolf_reproduce            = 0.05,
      wolf_gain_from_food       = 20.0,
      sheep_gain_from_food      = 4.0,
      sheep_reproduce_threshold = 10.0,
      wolf_reproduce_threshold  = 20.0,
      seed                      = NULL
    ),

    step = function(),    # advance model one tick; purge dead agents

    #' @return Named list:
    #'   - results: long tibble, columns step, variable, value
    #'       (variable in c("sheep","wolves"))
    #'   - extinct: logical, TRUE if either pop hit 0
    #'   - params: params list
    run = function(steps = 500L)
  )
)
```

**Implementation notes:**
- Store agents in plain R lists. Use `Filter()` to purge dead agents after each
  step.
- Agent stepping order must be **randomized each tick** (shuffle the combined
  agent list before iterating). This is important for fairness and matches
  standard ABM convention.
- Do not use a formal spatial grid object. A data frame of
  `(x, y, agent_id, type)` for lookups is fine. Ensure Moore-neighborhood
  lookup is efficient enough for a 50x50 grid with ~125 agents.

---

### 6. `R/abm_predator_prey_grass.R`

Extension of module 5. Adds a grass layer. Inherit from `PredatorPreyModel`.

```r
#' Predator-prey ABM with grass dynamics
#'
#' @description Adds a GrassPatch layer. Sheep gain energy only from
#'   grown cells. Eaten grass regrows after grass_regrowth_time steps.
PredatorPreyGrassModel <- R6::R6Class("PredatorPreyGrassModel",
  inherit = PredatorPreyModel,

  public = list(
    grass               = NULL,   # matrix(logical, nrow=height, ncol=width)
    grass_countdown     = NULL,   # matrix(integer) regrowth timers
    grass_regrowth_time = NULL,

    initialize = function(
      ...,                          # pass through all PredatorPreyModel args
      grass_regrowth_time    = 30L,
      initial_grass_fraction = 0.5,
      seed                   = NULL
    ),

    step = function(),   # override: tick grass countdowns; call super$step()

    run = function(steps = 500L)
    # Returns same as PredatorPreyModel$run() plus:
    #   results tibble gains rows where variable == "grass_fraction"
  )
)
```

Override only `initialize()`, `step()`, and `run()`. Reuse all wolf/sheep logic
from the parent class.

---

### 7. `R/abm_pandemic.R`

ISR model on a well-mixed grid. Baseline for comparison with the ODE model.

```r
Person <- R6::R6Class("Person",
  public = list(
    id     = NULL,
    state  = NULL,   # character: "I", "S", or "R"
    x      = NULL,
    y      = NULL,

    initialize = function(id, x, y, state),
    step = function(model)
    # step() logic (ISR ordering):
    #   - If "S": for each "I" agent in Moore neighborhood (radius 1),
    #     become "I" with probability beta.
    #   - If "I": become "R" with probability gamma.
    #   - If "R": stay "R".
  )
)

#' Pandemic ABM on well-mixed grid
PandemicModel <- R6::R6Class("PandemicModel",
  public = list(
    width      = NULL,
    height     = NULL,
    params     = NULL,
    agents     = NULL,
    step_count = 0L,

    initialize = function(
      width                     = 50L,
      height                    = 50L,
      n_agents                  = 2500L,
      initial_infected_fraction = 0.01,
      beta                      = 0.3,
      gamma                     = 0.05,
      seed                      = NULL
    ),

    step = function(),

    #' @return Named list:
    #'   - results: long tibble, columns step, variable, value
    #'       (variable in c("I","S","R") as fractions)
    #'   - R0_empirical: estimated from early exponential growth rate
    #'   - peak_I: peak infected fraction
    #'   - peak_step: step at peak
    #'   - params: params list
    run = function(steps = 200L)
  )
)
```

---

### 8. `R/abm_pandemic_network.R`

Extension: replace spatial grid with a Barabasi-Albert social network.
This is the key teaching moment — the ODE's well-mixed assumption becomes
explicit and removable.

```r
#' Network pandemic ABM
#'
#' @description ISR model on a Barabasi-Albert graph (igraph).
#'   Power-law degree distribution produces natural super-spreader structure.
#'   Super-spreaders are agents in the top decile of degree.
NetworkPandemicModel <- R6::R6Class("NetworkPandemicModel",
  public = list(
    n_agents   = NULL,
    params     = NULL,
    graph      = NULL,    # igraph object
    agents     = NULL,    # data frame: id, state, degree, is_super_spreader
    step_count = 0L,

    initialize = function(
      n_agents                  = 500L,
      n_edges_per_new_node      = 3L,     # m in sample_pa()
      initial_infected_fraction = 0.01,
      beta                      = 0.3,
      gamma                     = 0.05,
      seed                      = NULL
    ),

    step = function(),
    # Each step: for each infected agent, attempt transmission to each
    # network neighbor with probability beta. Then apply recovery.

    #' @return Same keys as PandemicModel$run(), plus:
    #'   - super_spreader_attack_rate: fraction of super-spreaders ever infected
    #'   - non_super_spreader_attack_rate: same for non-super-spreaders
    #'   - degree_distribution: integer vector of node degrees
    run = function(steps = 200L)
  )
)
```

Use `igraph::sample_pa(n_agents, m = n_edges_per_new_node, directed = FALSE)`.
Super-spreaders = agents with degree above the 90th percentile of the degree
distribution.

---

### 9. `R/utils_axtell.R`

> **No ABM implementation.** The Axtell firm-size model is presented using
> materials supplied directly by Rob Axtell. This module provides only the
> statistical and plotting support needed to display and discuss his results
> in the course.

```r
#' Plot a firm-size distribution on log-log axes with power-law fit
#'
#' @param firm_sizes Integer vector of firm sizes (one element per firm).
#' @param fit Optional list from fit_power_law(); if supplied, overlays
#'   the fitted line.
#' @param title Plot title.
#' @return A ggplot object.
plot_firm_size_distribution <- function(firm_sizes, fit = NULL,
                                        title = "Firm size distribution")

#' Load Axtell firm-size data from a CSV or RDA file
#'
#' @param path Path to file. CSV must have a column named `size`.
#' @return Integer vector of firm sizes.
load_firm_sizes <- function(path)
```

The demo script (`demo/demo_axtell.R`) loads Axtell's data, fits a power law
via `fit_power_law()`, plots the log-log distribution, and prints the estimated
exponent and KS statistic. The discussion — local interaction, heavy tails,
Zipf's law, contrast with log-normal — is handled verbally using Axtell's slides.

---

### 10. Demo scripts (`demo/`)

Each demo is a standalone R script with a clearly marked section at the top that
users can run. Scripts load the package via `devtools::load_all()` or
`library(abmcourse)`.

Every demo must:
1. Run both the SD and ABM versions (or ABM baseline and extension).
2. Produce a side-by-side comparison figure via `plot_side_by_side()`.
3. Print a brief statistical summary using `cli::cli_alert_info()`.
4. Save figures to `figures/` (create if missing) as `.png` at 150 dpi.

```r
# demo_lotka_volterra.R
# Compares ODE and ABM predator-prey.
# Figures: side-by-side time series + phase portrait overlay.

# demo_isr.R
# Compares ODE and ABM ISR pandemic.
# Figures: ODE curve overlaid with mean +/- 1 SD band from 10 ABM runs.

# demo_predator_prey.R
# Compares baseline ABM vs grass-extension ABM.
# Figures: population time series + grass fraction panel.

# demo_pandemic.R
# Compares grid ABM vs network ABM.
# Figures: epidemic curves + BA degree distribution.

# demo_axtell.R
# Runs firm-size model. Plots log-log distribution with power-law fit line.
# Overlays US Census firm-size data from data/census_firm_sizes.rda.
```

Scripts must be runnable via `Rscript demo/demo_lotka_volterra.R` from the
package root.

---

## Course schedule

The schedule below is authoritative for all timing decisions in the demo
scripts, slide deck, and course document. Activities are guided R exercises
in which students run and modify the package's demo scripts directly.

```
8:30  Morning session 1 (105 min)
      8:30  Introduction and framing — central epistemological
            tension, Newton, hitting-time reframing, roadmap    30 min
      9:00  SD: Lotka-Volterra — model sketch, equilibrium
            result (no full derivation), phase portrait         25 min
      9:25  SD: Lotka-Volterra — R implementation, figures      20 min
      9:45  ACTIVITY 1: Modify LV parameters; find a
            parameterization that causes extinction              20 min
      10:05 SD: ISR — compartmental structure, R0 derivation,
            herd immunity threshold                             10 min

10:15 Break (15 min)

10:30 Morning session 2 (90 min)
      10:30 SD: ISR — R implementation, epidemic curve           20 min
      10:50 ABM: predator-prey — from ODEs to agents,
            rules, R implementation                              30 min
      11:20 ABM: predator-prey — ODE vs ABM comparison,
            stochastic variation                                 10 min
      11:30 ACTIVITY 2: Compare ABM and ODE outputs; change
            grid size and observe finite-population effects      20 min
      11:50 Transition remarks: what the ABM buys you           10 min

12:00 Lunch (60 min)

1:00  Afternoon session 1 (120 min)
      1:00  ABM: ISR pandemic — agent rules, well-mixed
            grid, R implementation                              25 min
      1:25  ABM: ISR — ODE vs ABM, variance across runs         15 min
      1:40  ACTIVITY 3: Run 10 epidemic simulations; compute
            empirical R0 and compare to theoretical             20 min
      2:00  Extension: network + super-spreaders — BA graph,
            igraph, model rules and implementation              30 min
      2:30  Extension: network — epidemic curves,
            super-spreader attack rates                         15 min
      2:45  ACTIVITY 4: Rewire the network (vary m); observe
            effect on epidemic curve shape                      15 min

3:00  Break (15 min)

3:15  Afternoon session 2 (105 min)
      3:15  Extension: grass — motivation, rules, R
            implementation, effect on population cycles         25 min
      3:40  ACTIVITY 5: Add grass; compare cycle amplitude
            and extinction probability to baseline              20 min
      4:00  Axtell: firm size — empirical distribution,
            power law, local interaction as mechanism           35 min
      4:35  Discussion: ABM as a statistical modeling
            paradigm, calibration, when to use it               20 min
      4:55  Closing remarks and further reading                  5 min

5:00  End
```

### Activity specifications

Each activity corresponds to a guided exercise in the course document
(a dedicated subsection with instructions and questions) and a matching
block comment in the relevant demo script marking the starting point.

| Activity | Demo script | What students do |
|---|---|---|
| 1 | `demo_lotka_volterra.R` | Call `run_lv()` with modified params; find extinction boundary |
| 2 | `demo_predator_prey.R` | Run ABM and ODE side by side; vary `width`/`height`; note stochastic extinction |
| 3 | `demo_isr.R` | Run `PandemicModel` 10 times; summarise `R0_empirical` distribution |
| 4 | `demo_pandemic.R` | Vary `n_edges_per_new_node`; plot resulting epidemic curves |
| 5 | `demo_predator_prey.R` | Switch to `PredatorPreyGrassModel`; compare outputs |

### Implications for demo scripts

Each demo script must contain a clearly marked activity block comment:

```r
## ---- ACTIVITY [n]: [title] --------------------------------------------------
## Students start here. Instructions are in the course document Section [x.y].
## Expected run time: ~5 minutes.
```

The activity block should be self-contained: all parameters students are
expected to modify are defined as named variables immediately above the
comment, not buried in function calls.

### Implications for the slide deck

Each session block maps to a beamer `\section{}`. Activity slides use a
distinct frame template:

```latex
\begin{frame}{Activity N: Title}
  \begin{block}{Instructions}
    \begin{enumerate}
      \item ...
    \end{enumerate}
  \end{block}
  \textit{20 minutes — open \texttt{demo\_xxx.R} and start at the
  \texttt{ACTIVITY N} comment.}
\end{frame}
```

### Implications for the course document

Each activity gets a dedicated subsection immediately following the
relevant content section, structured as:

```
\subsection{Activity N: Title}
Instructions paragraph.
\begin{enumerate}
  \item Step one.
  \item Step two (question to answer and record).
  \item Extension: harder question for fast finishers.
\end{enumerate}
```

Include an extension question for fast finishers in every activity.

---

## Knitr course document (`vignettes/abm_for_statisticians.Rnw`)

A single self-contained `.Rnw` document (LaTeX + knitr) covering the full course
curriculum. This is the primary take-home reference for students. It is **not** a
vignette in the R package sense — place it in `vignettes/` for convenience but it
is compiled independently via `knitr::knit()` + `pdflatex`, not by
`R CMD build`.

The `chunks/` directory must be listed in `.Rbuildignore` so that
`devtools::load_all()` and `R CMD build` do not attempt to source chunk
files as package functions. Add this line to `.Rbuildignore`:

```
^chunks$
```

### Compilation

```bash
Rscript -e "knitr::knit('vignettes/abm_for_statisticians.Rnw')"
pdflatex abm_for_statisticians.tex
pdflatex abm_for_statisticians.tex   # second pass for references
```

Document must compile cleanly to PDF with no warnings from `pdflatex`.

### External chunk files

All R code is stored in `chunks/` and loaded via `knitr::read_chunk()`.
Neither `.Rnw` file contains inline R code beyond the `read_chunk()` calls
and chunk references. This ensures a single source of truth for all
executable code.

At the top of `abm_for_statisticians.Rnw`, after the global options chunk:

```r
knitr::read_chunk("../chunks/lv.R")
knitr::read_chunk("../chunks/isr.R")
knitr::read_chunk("../chunks/abm_predator_prey.R")
knitr::read_chunk("../chunks/abm_pandemic.R")
knitr::read_chunk("../chunks/abm_network.R")
knitr::read_chunk("../chunks/abm_grass.R")
knitr::read_chunk("../chunks/axtell.R")
```

Chunks are then referenced by label only:

```latex
<<lv-run-model>>=
@

<<lv-time-series>>=
@
```

Each chunk file uses the standard knitr named-chunk comment convention:

```r
## ---- lv-run-model ----------------------------------------------------------
params <- LV_DEFAULTS
results <- run_lv(params = params)

## ---- lv-time-series --------------------------------------------------------
plot_time_series(results$results, title = "Lotka-Volterra: prey and predators",
                 theme_fn = theme_course_light)

## ---- lv-phase-portrait -----------------------------------------------------
plot_phase_portrait(
  results$results |> tidyr::pivot_wider(names_from = variable,
                                         values_from = value),
  xlabel = "Prey", ylabel = "Predators",
  title  = "Phase portrait",
  theme_fn = theme_course_light
)
```

**The `theme_fn` parameter**: all plotting functions accept a `theme_fn`
argument (default `theme_course_light`) that is applied as the ggplot2 theme.
Slide chunks pass `theme_fn = theme_course_dark`. This is the only difference
between a document chunk and its slide counterpart — the chunk label gets a
`-dark` suffix in the slide deck (e.g. `lv-time-series-dark`) and the chunk
body is identical except for `theme_fn`.

### LaTeX preamble requirements

```latex
\documentclass[11pt]{article}
\usepackage{amsmath, amsthm, amssymb}
\usepackage{booktabs}
\usepackage{hyperref}
\usepackage{natbib}
\usepackage{geometry}[margin=1in]
\usepackage{float}      % for H figure placement
\usepackage{listings}   % fallback; knitr handles most code display

\newtheorem{definition}{Definition}
\newtheorem{proposition}{Proposition}
\newtheorem{remark}{Remark}
```

### Global knitr chunk options

Set once at the top of the document:

```r
knitr::opts_chunk$set(
  echo    = TRUE,       # always display code
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  fig.pos   = "H",      # place figures exactly here
  fig.width = 8,
  fig.height = 4,
  out.width = "\\textwidth"
)
```

### Document structure

The document follows the course arc exactly. Each section introduces the model
formally (mathematics), shows the R implementation running, displays the output
figure, and closes with a statistical interpretation paragraph.

```
1  Introduction
   1.1  What is agent-based modeling?
   1.2  Why should statisticians care?
   1.3  The central epistemological tension: false equilibrium vs apocalypse
        extrapolation
        - Any deterministic trend, extrapolated far enough, crosses a
          critical threshold
        - Any equilibrium model, extended far enough, ignores drift toward
          a threshold
        - Newton as the paradigm case: extrapolation works when structural
          conditions (conservation, closure, linearity) are satisfied;
          ecological and epidemiological systems conspicuously lack these
        - The ABM resolution: replace point predictions with distributions
          over futures; reframe policy questions as hitting-time problems
        - This tension is a thread running through the entire course
   1.4  System dynamics vs agent-based models: a roadmap

2  System Dynamics: the Lotka-Volterra predator-prey model
   2.1  Model formulation (ODEs, parameters, ecological interpretation)
   2.2  Equilibrium analysis — full derivation in document;
        sketch only on slides (state the fixed point, cite the
        result, move on)
   2.3  R implementation with deSolve
   2.4  Phase portrait and time series
   2.5  Mean-field foundations and time-scale limitations
        - LV as the mean-field limit of a stochastic birth-death process
        - Demographic stochasticity: variance scales as 1/N; deviations
          are of order 1/sqrt(N)
        - The ODE equilibrium is neutrally stable, not asymptotically
          stable: cycles neither grow nor decay, but the stochastic version
          drifts
        - Recurrence theorem (state without proof, cite Karlin & Taylor):
          a one-dimensional random walk is recurrent — it visits every
          state including zero. The ODE cannot represent this; the ABM
          reveals it as finite extinction probability
        - Time-scale argument: ODE is a reliable approximation over short
          horizons; error accumulates as T grows, as N shrinks, and as the
          trajectory approaches a boundary
        - Policy consequence: conservation management built on ODE cycles
          is implicitly assuming the mean-field regime persists
          indefinitely. The ABM reframes this as a hitting-time problem:
          what is the probability of extinction before time T?
   2.6  Activity 1: Modifying parameters and finding extinction boundaries

3  System Dynamics: the ISR epidemic model
   3.1  Model formulation (compartments, flow diagram, assumptions)
   3.2  Basic reproduction number R0 (derive from next-generation method)
   3.3  Herd immunity threshold
   3.4  R implementation with deSolve
   3.5  Epidemic curve and final size
   3.6  Mean-field foundations and time-scale limitations
        - ISR ODE as the N -> inf limit of a stochastic SIR process on a
          complete graph: the homogeneous mixing assumption stated precisely
        - Early epidemic phase: mean-field is most accurate here;
          R0 estimated in this phase is reliable
        - Around the peak and late epidemic: mean-field degrades as
          susceptible pool is depleted heterogeneously across the network
        - Stochastic fadeout: probability of extinction when R0 > 1 in a
          finite population (branching process approximation, cite
          Bartlett 1955 or Andersson & Britton 2000); the ODE cannot
          represent this
        - Time-scale argument: ODE final-size equation is reliable for
          large homogeneous populations; breaks down for small populations,
          heterogeneous contact structure, and long endemic time horizons
        - Policy consequence: intervention timing (lockdowns, vaccination
          rollout) depends critically on where you are on the epidemic
          curve; ODE and network ABM can disagree substantially about that
          timing, especially in the presence of super-spreaders

4  Agent-Based Models: predator-prey
   4.1  From ODEs to agents: what changes and what does not
   4.2  Model rules (Sheep, Wolf, toroidal grid)
   4.3  R implementation with R6
   4.4  Comparison with ODE: time series overlay, stochastic variation
   4.5  Finite-population effects and extinction risk
   4.6  Activity 2: Grid size and finite-population effects

5  Agent-Based Models: ISR epidemic
   5.1  From compartments to individuals
   5.2  Model rules (Person, well-mixed grid)
   5.3  R implementation with R6
   5.4  Comparison with ODE: mean trajectory and variance across runs
   5.5  What the well-mixed assumption is really doing
   5.6  Activity 3: Empirical R0 across stochastic runs

6  Extension: social networks and super-spreaders
   6.1  The well-mixed assumption revisited
   6.2  Barabasi-Albert networks and degree heterogeneity
   6.3  R implementation with igraph
   6.4  Epidemic curves on the network vs the grid
   6.5  Super-spreader identification and attack rates
   6.6  Statistical implications: why R0 is not enough
        - On a heterogeneous network, the threshold quantity is the
          spectral radius of the next-generation matrix, not R0
        - State the result (cite Diekmann, Heesterbeek & Metz 1990):
          the epidemic threshold on a network is determined by the largest
          eigenvalue of the next-generation matrix; reduces to R0 = beta/gamma
          on a complete graph
        - Super-spreader removal: targeting hubs is highly effective in a
          static network model — but this is an extrapolation into an
          intervention regime the model was never fit on; real networks
          adapt, behavior changes, and the model's structural assumptions
          break down precisely when the policy is most aggressive
        - The apocalypse extrapolation failure mode: network models can
          predict near-certain epidemic explosion for modest changes in
          connectivity; this is the trend-extrapolation trap applied to
          network structure
   6.7  Activity 4: Varying network connectivity

7  Extension: grass and carrying capacity
   7.1  Adding a third trophic level (motivation and rules)
   7.2  R implementation
   7.3  Effect on population cycles: amplitude, period, extinction
        probability
   7.4  Activity 5: Comparing baseline and grass-extension ABM

8  Firm Size and Heavy-Tailed Distributions (Axtell)
   8.1  The empirical regularity: US firm-size distribution
   8.2  Power laws, Zipf's law, and the Pareto distribution
   8.3  Why top-down models fail: the time-scale argument applied to
        economic dynamics
        - Firm-size dynamics operate on decade-long time scales; no
          mean-field approximation is credible at these horizons
        - The equilibrium trap: standard industrial organization models
          predict stable size distributions; the empirical distribution
          is heavy-tailed and non-Gaussian, which no equilibrium model
          generates convincingly
   8.4  Local interaction as a generative mechanism
   8.5  Fitting and interpreting the power-law exponent
   8.6  [Figures and results from Axtell's materials]

9  Discussion: ABM as a statistical modeling paradigm
   9.1  The epistemological thread: revisited
        - Restate the central tension: false equilibrium vs apocalypse
          extrapolation
        - Newton's laws as the paradigm of successful extrapolation:
          what made it work (conservation, closure, linearity) and why
          those conditions rarely hold in social and biological systems
          (cite Newton Principia; discuss why physics succeeded where
          other sciences struggle)
        - The recurrence theorem as the mathematical statement of the
          long-run fragility of equilibrium: cite Karlin & Taylor (1975)
          A First Course in Stochastic Processes
        - The hitting-time reframing: policy questions should be stated
          as "what is the probability of crossing threshold X before
          time T" — a question ABM with stochastic replication can
          answer and ODE models cannot
        - The Lucas critique as an instance of the same structure
          (for audiences with economics background)
   9.2  Strengths and limitations of ABM
   9.3  Calibration and identifiability
   9.4  When to use ABM vs ODE vs statistical models
   9.5  Further reading and key references
```

### Content standards

- **Mathematics**: use `align` environments for multi-line derivations. Number
  only equations that are referenced later. Use `\begin{proposition}` /
  `\begin{proof}` for analytical results (equilibria, R0 derivation, herd
  immunity threshold).
- **R code**: every chunk that produces a figure or printed result must be
  shown (`echo = TRUE`). Chunks that are purely setup (library loading,
  palette definition) may use `echo = FALSE` if they add no instructional
  value, but this should be rare.
- **Figures**: every figure gets a `\caption{}` with a full sentence of
  interpretation, not just a label. Figures are produced by calling the
  package's plotting functions (`plot_time_series()`, `plot_phase_portrait()`,
  etc.) so the document and the demo scripts produce visually consistent output.
- **Interpretation paragraphs**: every model section closes with a paragraph
  explicitly connecting the result to statistical concepts the audience already
  knows — likelihood, variance, distributional assumptions, model
  misspecification. This is the bridge that justifies the course to a
  skeptical statistician.
- **Section 8**: leave a clearly marked placeholder comment where Axtell's
  figures and results will be inserted. The surrounding prose (power law
  motivation, fitting methodology, interpretation) should be written in full.
- **References**: use `natbib` with `\citep{}` / `\citet{}`. Required
  entries in `references.bib` (to be populated when instructor provides
  full bibliography):
  - Lotka (1925) — LV prey equation
  - Volterra (1926) — LV predator equation
  - Kermack & McKendrick (1927) — SIR model
  - Karlin & Taylor (1975) *A First Course in Stochastic Processes* —
    recurrence theorem
  - Bartlett (1955) or Andersson & Britton (2000) — stochastic fadeout /
    branching process approximation for epidemic extinction
  - Diekmann, Heesterbeek & Metz (1990) — next-generation matrix /
    spectral radius epidemic threshold
  - Barabasi & Albert (1999) — BA preferential attachment network
  - Axtell (2001) — firm-size power law
  - Clauset, Shalizi & Newman (2009) — power law fitting methodology
  - Newton, *Principia Mathematica* (1687) — paradigm case of
    successful extrapolation (Section 9 discussion)
  - Additional references to be added from instructor's list

### Build order for the document

Write the `.Rnw` in section order. After each section is drafted, verify that:
1. `knitr::knit()` runs without error on the section's chunks in isolation.
2. All figures render and are correctly captioned.
3. All mathematical environments compile without `pdflatex` errors.

Do not write the full document in one pass. Build and verify section by section.

---

## Beamer slide deck (`vignettes/abm_slides.Rnw`)

A single `.Rnw` beamer presentation covering the full course. Used by the
instructor during delivery. Compiled independently from the course document
but shares `references.bib` and calls the same package functions to produce
identical figures.

### Compilation

```bash
Rscript -e "knitr::knit('vignettes/abm_slides.Rnw')"
pdflatex abm_slides.tex
pdflatex abm_slides.tex
```

### LaTeX preamble

```latex
\documentclass{beamer}
\usetheme{Madrid}
\usecolortheme{default}
\usepackage{amsmath, amssymb}
\usepackage{booktabs}
\usepackage{natbib}

% Dark navy theme
\setbeamercolor{normal text}{fg=SlideText, bg=SlideBG}
\setbeamercolor{frametitle}{fg=SlideAccent, bg=SlideBG}
\setbeamercolor{title}{fg=SlideAccent}
\setbeamercolor{block title}{fg=SlideBG, bg=SlideAccent}
\setbeamercolor{block body}{fg=SlideText, bg=SlidePanel}
\setbeamercolor{itemize item}{fg=SlideAccent}
\setbeamercolor{itemize subitem}{fg=SlideText}
\setbeamercolor{section in toc}{fg=SlideAccent}

\definecolor{SlideBG}{HTML}{1B2A4A}
\definecolor{SlideAccent}{HTML}{F18F01}
\definecolor{SlideText}{HTML}{E8E8E8}
\definecolor{SlidePanel}{HTML}{1E3A5F}

\setbeamerfont{frametitle}{series=\bfseries}
\setbeamertemplate{itemize item}{\textbullet}

\title{Agent-Based Modeling for Statisticians}
\author{}
\date{}
```

### External chunk files

`abm_slides.Rnw` loads the same chunk files as the course document.
At the top of the file, after the global options chunk:

```r
knitr::read_chunk("../chunks/lv.R")
knitr::read_chunk("../chunks/isr.R")
knitr::read_chunk("../chunks/abm_predator_prey.R")
knitr::read_chunk("../chunks/abm_pandemic.R")
knitr::read_chunk("../chunks/abm_network.R")
knitr::read_chunk("../chunks/abm_grass.R")
knitr::read_chunk("../chunks/axtell.R")
```

Figure chunks on slides use the `-dark` suffix convention and pass
`theme_fn = theme_course_dark`:

```latex
<<lv-time-series-dark>>=
@
```

The `-dark` variants are defined at the bottom of each chunk file:

```r
## ---- lv-time-series-dark ---------------------------------------------------
plot_time_series(results$results, title = "Lotka-Volterra: prey and predators",
                 theme_fn = theme_course_dark)
```

This is the **only** substantive difference between a document chunk and its
slide counterpart. No other code duplication is permitted.

### Global knitr chunk options for slides

```r
knitr::opts_chunk$set(
  echo      = FALSE,      # code hidden by default on slides
  warning   = FALSE,
  message   = FALSE,
  fig.align = "center",
  fig.width = 7,
  fig.height = 3.5,
  out.width = "\\textwidth"
)
```

Code is hidden by default. Use `echo = TRUE` only on slides explicitly
intended to show code — mark these with a comment `% SHOW CODE SLIDE` so
they are easy to find and toggle later.

### Relationship to the course document

The slides and the course document cover identical content in identical order.
The mapping is strict:

- Every **section** in the course document corresponds to a beamer `\section{}`
  in the slides.
- Every **figure** in the course document appears on a dedicated slide, produced
  by the **same function call with the same arguments**. Do not produce separate
  figures for slides.
- Every **proposition or analytical result** appears on the slides as a result
  box (`\begin{block}{Result}`) showing only the final statement. The derivation
  and proof live in the course document only.
- Every **interpretation paragraph** in the course document is condensed to
  2--3 bullet points on the corresponding slide.

### Slide structure

```
Section 1  Introduction
  - What is ABM? (1 slide)
  - Why statisticians should care (1 slide)
  - The central tension: false equilibrium vs apocalypse
    extrapolation (1 slide — sets up the entire day)
  - Newton: when extrapolation works and why (1 slide)
  - The ABM resolution: distributions over futures,
    hitting-time problems (1 slide)
  - Roadmap: SD vs ABM (1 slide)

Section 2  System dynamics: Lotka-Volterra
  - Model equations — write down the system, label
    parameters ecologically (1 slide)
  - Equilibrium result block: state prey* and predator*,
    no derivation, note it is neutrally stable not
    asymptotically stable (1 slide)
  - Time series + phase portrait (1 slide per figure)
  - Mean-field foundations: LV as limit of stochastic
    process (1 slide)
  - Recurrence theorem statement + citation (1 slide)
  - Time-scale limitations and policy consequence (1 slide)

Section 3  System dynamics: ISR epidemic
  - Model equations and flow diagram (1 slide)
  - R0 result and herd immunity threshold block (1 slide)
  - Epidemic curve (1 slide)
  - Mean-field foundations: complete graph assumption
    stated precisely (1 slide)
  - Stochastic fadeout: branching process result +
    citation (1 slide)
  - Time-scale limitations and policy consequence (1 slide)

Section 4  ABM: predator-prey
  - From ODEs to agents: what changes (1 slide)
  - Agent rules: Sheep and Wolf (1 slide each, concise)
  - ODE vs ABM comparison figure (1 slide)
  - Finite-population effects and extinction risk (1 slide)

Section 5  ABM: ISR epidemic
  - From compartments to individuals (1 slide)
  - Agent rules: Person (1 slide)
  - ODE vs ABM mean + variance figure (1 slide)
  - The well-mixed assumption unpacked (1 slide)

Section 6  Extension: social networks and super-spreaders
  - The well-mixed assumption as a complete-graph
    assumption (1 slide)
  - BA network: degree distribution (1 slide)
  - Epidemic curves: grid vs network (1 slide)
  - Super-spreader attack rates (1 slide)
  - Spectral radius result + citation; reduces to R0
    on complete graph (1 slide)
  - The extrapolation trap: why aggressive network
    interventions break the model (1 slide)

Section 7  Extension: grass and carrying capacity
  - Motivation and rules (1 slide)
  - Before/after comparison figure (1 slide)
  - Effect on population cycles (1 slide)

Section 8  Firm size and heavy tails (Axtell)
  - The empirical regularity (1 slide)
  - Power laws and Zipf's law (1 slide)
  - Why equilibrium models fail at long time scales (1 slide)
  - Local interaction as generative mechanism (1 slide)
  - [Placeholder slides for Axtell's figures]

Section 9  Discussion
  - The epistemological thread revisited: false equilibrium
    vs apocalypse extrapolation (1 slide)
  - Newton and the conditions for successful extrapolation
    (1 slide)
  - Hitting-time reframing of policy questions (1 slide)
  - Strengths and limitations of ABM (1 slide)
  - When to use ABM vs ODE vs statistical models (1 slide)
  - Further reading (1 slide)
```

Target: approximately 50--55 slides. Each slide makes exactly one point.

### Content standards for slides

- **No derivations.** Proofs and algebraic steps belong in the course document
  only. Slides show named results in `\begin{block}{}` environments.
- **Bullets**: maximum 4 per slide, maximum 10 words each. If a point needs
  more words it belongs in the course document.
- **Figures**: same ggplot2 output as the course document. Adjust
  `fig.width` / `fig.height` per chunk if needed for beamer's aspect ratio,
  but do not alter the underlying function calls.
- **Section 8 placeholder**: use `% AXTELL FIGURE PLACEHOLDER` comments on
  slides that will receive his materials.
- **Citations**: `\citep{}` on the slide where a result is first stated. No
  bibliography slide needed; the course document carries full references.

---

## Build order

Build modules in this exact sequence. Each step should pass its tests before
proceeding.

1. `DESCRIPTION`, `NAMESPACE`, `.Rbuildignore`, package skeleton
2. `R/utils_plotting.R` (including `theme_course_light`, `theme_course_dark`,
   `COURSE_PALETTE`, `SLIDE_*` constants) + `R/utils_stats.R`
3. `R/sd_lotka_volterra.R` → `tests/testthat/test-sd.R` (LV portion)
4. `R/sd_isr_pandemic.R` → `tests/testthat/test-sd.R` (ISR portion)
5. `R/abm_predator_prey.R` → `tests/testthat/test-abm-predator-prey.R`
6. `R/abm_predator_prey_grass.R` → extend tests
7. `R/abm_pandemic.R` → `tests/testthat/test-abm-pandemic.R`
8. `R/abm_pandemic_network.R` → extend tests
9. `R/utils_axtell.R` + `demo/demo_axtell.R` (no model, no simulation tests)
10. `demo/` scripts (no tests required, but must run without error)
11. `chunks/*.R` — one chunk file per module, light and dark variants;
    verify each chunk runs in isolation before proceeding
12. `vignettes/abm_for_statisticians.Rnw` — build section by section in
    curriculum order; verify compilation after each section
13. `vignettes/abm_slides.Rnw` — build section by section; verify figures
    match course document and dark theme renders correctly
14. `data-raw/census_firm_sizes.R` + `data/census_firm_sizes.rda`

---

## Testing standards

Use `testthat` edition 3. No single test should take longer than **30 seconds**.
Use short simulation runs (<= 100 steps) and small grids (<= 20x20) in all tests.

### `test-sd.R`

- LV: output tibble has correct columns; prey and predators stay positive over
  the full integration; equilibrium formula is correct; increasing `alpha`
  raises the predator equilibrium as expected analytically.
- ISR: `I + S + R` sums to 1.0 at every time point (within 1e-6); `R` is
  monotone non-decreasing; `R0` matches `beta / gamma`; `peak_I` is
  correctly identified.

### `test-abm-predator-prey.R`

- Model runs 50 steps on a 20x20 grid without error.
- Results tibble has columns `step`, `variable`, `value`.
- Population counts are non-negative integers at every step.
- Two runs with `seed = 42` produce identical results.
- Grass extension: results contain rows where `variable == "grass_fraction"`;
  all values are in [0, 1].

### `test-abm-pandemic.R`

- Model runs 50 steps without error.
- `I + S + R` sums to 1.0 at every step (within 1e-4 — discrete rounding).
- `R` is monotone non-decreasing.
- Network model: `degree_distribution` vector is present; mean degree is
  approximately `2 * n_edges_per_new_node` (within 20%).

---

## Parameters and ecological/epidemiological grounding

Do not choose parameter values arbitrarily. Each default must be either:

1. Justifiable from the ecological or epidemiological literature, **or**
2. Chosen deliberately for pedagogical clarity, with an inline comment explaining
   the choice.

The ISR defaults (`beta = 0.3`, `gamma = 0.05`) yield R0 = 6, which is
measles-like and produces a dramatic, clearly peaked epidemic curve suitable for
teaching. The LV defaults should produce approximately 3-4 oscillation cycles
within the default time window of 200 time units.

---

## What to ask before writing code

Stop and ask if any of the following is ambiguous:

- Whether the network pandemic model should allow re-infection (default: no).
- Whether `demo_isr.R` should show confidence bands across multiple stochastic
  runs (recommended: yes, 10 runs).
- Whether to fetch Census firm-size data from a URL at build time or bundle a
  pre-cleaned CSV committed to the repository (recommended: bundle it alongside
  Axtell's other materials once received).
- Whether any additional packages beyond those in `DESCRIPTION` are needed
  for the `.Rnw` compilation environment (e.g. `tikz`, `pgfplots`).

---

## Out of scope for this build

The following are explicitly **not** part of this codebase:

- NetLogo or any non-R ABM framework.
- Shiny apps or interactive dashboards.
- Bayesian calibration or likelihood-based ABM parameter fitting.
- Any causal inference machinery.
- R Markdown or Quarto notebooks (the demo scripts serve this purpose).

These may be added in a future version. Do not stub them out.
