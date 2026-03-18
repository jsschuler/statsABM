## demo_isr.R
## Compares the ISR ODE epidemic model with the pandemic ABM.
## Figures: ODE curve overlaid with mean +/- 1 SD band from 10 ABM runs.
##
## Run from the package root:
##   Rscript demo/demo_isr.R

pkgload::load_all(".", quiet = TRUE)

if (!dir.exists("figures")) dir.create("figures")

N_RUNS <- 10L   # number of stochastic ABM replicates
STEPS  <- 160L  # match the ODE time span

# ── 1. ODE: ISR pandemic ──────────────────────────────────────────────────────
cli::cli_h1("ISR ODE")

isr <- run_isr(params = ISR_DEFAULTS)

cli::cli_alert_info(
  "ODE: R0 = {round(isr$R0, 2)}, \\
   peak I = {round(isr$peak_I * 100, 1)}% at t = {round(isr$peak_time, 1)}, \\
   final R = {round(isr$final_R * 100, 1)}%"
)

isr_colours <- c(I = COURSE_PALETTE[["abm"]],
                 S = COURSE_PALETTE[["sd"]],
                 R = COURSE_PALETTE[["extended"]])

p_ode <- plot_time_series(
  isr$results,
  title      = "ISR ODE",
  colour_map = isr_colours
)

# ── 2. ABM: pandemic (10 replicates) ──────────────────────────────────────────
cli::cli_h1(sprintf("Pandemic ABM (%d replicates)", N_RUNS))

abm_runs <- vector("list", N_RUNS)
cli::cli_progress_bar("Running replicates", total = N_RUNS)
for (i in seq_len(N_RUNS)) {
  m   <- PandemicModel$new(seed = i)
  out <- m$run(steps = STEPS)
  i_series <- out$results[out$results$variable == "I", ]
  i_series$run <- i
  abm_runs[[i]] <- list(I_series = i_series, R0_emp = out$R0_empirical,
                        peak_I = out$peak_I)
  cli::cli_progress_update()
}
cli::cli_progress_done()

# Empirical R0 distribution across runs
r0_vals <- sapply(abm_runs, function(r) r$R0_emp)
r0_vals <- r0_vals[is.finite(r0_vals)]
cli::cli_alert_info(
  "ABM empirical R0: mean = {round(mean(r0_vals), 2)}, \\
   sd = {round(sd(r0_vals), 2)} (theoretical = {round(ISR_DEFAULTS$beta / ISR_DEFAULTS$gamma, 1)})"
)

peak_I_vals <- sapply(abm_runs, function(r) r$peak_I)
cli::cli_alert_info(
  "ABM peak I: mean = {round(mean(peak_I_vals) * 100, 1)}%, \\
   sd = {round(sd(peak_I_vals) * 100, 1)}% \\
   (ODE = {round(isr$peak_I * 100, 1)}%)"
)

# ── 3. Summary statistics and overlay figure ──────────────────────────────────
cli::cli_h1("Comparison figure")

all_I <- do.call(rbind, lapply(abm_runs, function(r) r$I_series))

abm_summary <- all_I |>
  dplyr::group_by(step) |>
  dplyr::summarise(
    mean_I = mean(value),
    sd_I   = stats::sd(value),
    .groups = "drop"
  ) |>
  dplyr::mutate(
    lo = pmax(0, mean_I - sd_I),
    hi = pmin(1, mean_I + sd_I)
  )

ode_I <- isr$results[isr$results$variable == "I", ]

p_overlay <- ggplot2::ggplot() +
  ggplot2::geom_ribbon(
    data    = abm_summary,
    mapping = ggplot2::aes(x = step, ymin = lo, ymax = hi),
    fill    = COURSE_PALETTE[["abm"]], alpha = 0.25
  ) +
  ggplot2::geom_line(
    data    = abm_summary,
    mapping = ggplot2::aes(x = step, y = mean_I, colour = "ABM mean"),
    linewidth = 0.9
  ) +
  ggplot2::geom_line(
    data    = ode_I,
    mapping = ggplot2::aes(x = time, y = value, colour = "ODE"),
    linewidth = 0.9
  ) +
  ggplot2::scale_colour_manual(
    values = c("ODE" = COURSE_PALETTE[["sd"]], "ABM mean" = COURSE_PALETTE[["abm"]]),
    name   = NULL
  ) +
  ggplot2::labs(
    x     = "time / step",
    y     = "infected fraction",
    title = sprintf("ISR: ODE vs ABM (%d runs, shading = mean \u00b1 1 SD)", N_RUNS)
  ) +
  theme_course_light()

p_ode_full <- plot_time_series(
  isr$results,
  title      = "ISR ODE (all compartments)",
  colour_map = isr_colours
)

fig_comparison <- plot_side_by_side(
  p_ode_full, p_overlay,
  title = "ISR: system dynamics vs agent-based"
)

ggplot2::ggsave("figures/isr_comparison.png", fig_comparison,
                width = 12, height = 4, dpi = 150)
ggplot2::ggsave("figures/isr_overlay.png", p_overlay,
                width = 7, height = 4, dpi = 150)

cli::cli_alert_success("Figures saved to figures/")


## ---- ACTIVITY 3: Stochastic variation in epidemic outcomes (Section 5.6) ---
## Instructions: course document Section 5.6. Expected time: ~20 minutes.
##
## Teaching point: The ODE gives one epidemic curve; the ABM gives a
## distribution. Peak infected fraction varies across runs in ways the ODE
## cannot represent. Statistical thinking — summarising a distribution,
## not reading off a point — is the right framework for ABM output.

# Parameters to modify:
seeds <- 1:10    # try 1:20 for more replicates
steps <- 160L
beta  <- ISR_DEFAULTS$beta    # try 0.1, 0.2, 0.3, 0.4
gamma <- ISR_DEFAULTS$gamma

# Run the pandemic model with the parameters above
runs_a3 <- lapply(seeds, function(s) {
  PandemicModel$new(beta = beta, gamma = gamma, seed = s)$run(steps = steps)
})

# Summarise key statistics across all runs
run_summary <- summarise_runs(runs_a3)
print(run_summary)

# Compare mean ABM trajectory to ODE
results_ode_a3 <- run_isr(params = list(beta = beta, gamma = gamma))
plot_epidemic_comparison(runs_a3, ode = results_ode_a3)

# Q: How much does peak_I vary across runs?
#    Does the ODE peak fall within the ABM distribution?
#    What does this imply for policy decisions based on the ODE peak?

# Extension (fast finishers): Increase initial_infected_fraction to 0.05.
# Does the variance across runs increase or decrease?
# What does this imply about epidemic prediction early vs late in an outbreak?
# YOUR CODE HERE
