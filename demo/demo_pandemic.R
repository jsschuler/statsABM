## demo_pandemic.R
## Compares the well-mixed grid pandemic ABM with the network pandemic ABM.
## Figures: epidemic curves; Barabasi-Albert degree distribution.
##
## Run from the package root:
##   Rscript demo/demo_pandemic.R

pkgload::load_all(".", quiet = TRUE)

if (!dir.exists("figures")) dir.create("figures")

STEPS <- 200L

# ── 1. Grid ABM: well-mixed pandemic ──────────────────────────────────────────
cli::cli_h1("Pandemic ABM: well-mixed grid")

grid_model <- PandemicModel$new(seed = 42L)
grid_out   <- grid_model$run(steps = STEPS)

isr_colours <- c(I = COURSE_PALETTE[["abm"]],
                 S = COURSE_PALETTE[["sd"]],
                 R = COURSE_PALETTE[["extended"]])

p_grid <- plot_time_series(
  grid_out$results,
  title      = "Grid ABM: ISR epidemic",
  colour_map = isr_colours
)

cli::cli_alert_info(
  "Grid ABM: empirical R0 = {round(grid_out$R0_empirical, 2)}, \\
   peak I = {round(grid_out$peak_I * 100, 1)}% at step {grid_out$peak_step}"
)

# ── 2. Network ABM: Barabasi-Albert pandemic ──────────────────────────────────
cli::cli_h1("Pandemic ABM: Barabasi-Albert network")

net_model <- NetworkPandemicModel$new(seed = 42L)
net_out   <- net_model$run(steps = STEPS)

p_net <- plot_time_series(
  net_out$results,
  title      = "Network ABM: ISR epidemic",
  colour_map = isr_colours
)

cli::cli_alert_info(
  "Network ABM: empirical R0 = {round(net_out$R0_empirical, 2)}, \\
   peak I = {round(net_out$peak_I * 100, 1)}% at step {net_out$peak_step}"
)
cli::cli_alert_info(
  "Super-spreader attack rate:     {round(net_out$super_spreader_attack_rate * 100, 1)}%"
)
cli::cli_alert_info(
  "Non-super-spreader attack rate: {round(net_out$non_super_spreader_attack_rate * 100, 1)}%"
)

# ── 3. Degree distribution ────────────────────────────────────────────────────
deg_df <- data.frame(degree = as.integer(net_out$degree_distribution))

p_degree <- plot_distribution(
  deg_df$degree,
  title   = "BA network: degree distribution",
  log_log = TRUE
)

cli::cli_alert_info(
  "Degree: min = {min(deg_df$degree)}, \\
   mean = {round(mean(deg_df$degree), 1)}, \\
   max = {max(deg_df$degree)}"
)

# ── 4. Comparison figures ─────────────────────────────────────────────────────
cli::cli_h1("Comparison figures")

fig_epidemic <- plot_side_by_side(
  p_grid, p_net,
  title = "ISR pandemic: well-mixed grid vs Barabasi-Albert network"
)

fig_net_panel <- plot_side_by_side(
  p_net, p_degree,
  title = "Network ABM: epidemic curve and degree distribution"
)

ggplot2::ggsave("figures/pandemic_grid_vs_network.png", fig_epidemic,
                width = 12, height = 4, dpi = 150)
ggplot2::ggsave("figures/pandemic_network_panel.png", fig_net_panel,
                width = 12, height = 4, dpi = 150)

cli::cli_alert_success("Figures saved to figures/")


## ---- ACTIVITY 4: Hub seeding and epidemic shape (Section 6.7) --------------
## Instructions: course document Section 6.7. Expected time: ~15 minutes.
##
## Teaching point: On a heterogeneous network, WHO gets infected first matters
## as much as HOW MANY. Seeding the epidemic in high-degree hub nodes vs
## peripheral low-degree nodes produces dramatically different epidemic curves
## even with identical parameters — making the super-spreader mechanism
## viscerally observable.

# Parameters to modify:
n_agents <- 500L
beta     <- 0.3     # try 0.1, 0.3, 0.5
gamma    <- 0.05

# Run with random seeding vs hub seeding (same seed for network structure)
results_random <- NetworkPandemicModel$new(
  n_agents  = n_agents, beta = beta, gamma = gamma,
  seed_hubs = FALSE, seed = 42L
)$run(steps = STEPS)

results_hubs <- NetworkPandemicModel$new(
  n_agents  = n_agents, beta = beta, gamma = gamma,
  seed_hubs = TRUE, seed = 42L
)$run(steps = STEPS)

# Compare epidemic curves
plot_side_by_side(
  plot_time_series(results_random$results,
                   title      = "Random seeding",
                   colour_map = isr_colours),
  plot_time_series(results_hubs$results,
                   title      = "Hub seeding",
                   colour_map = isr_colours)
)

# Compare attack rates by node type
cli::cli_alert_info(
  "Random seeding -- super-spreader AR: \\
   {round(results_random$super_spreader_attack_rate * 100, 1)}%, \\
   non-SS AR: {round(results_random$non_super_spreader_attack_rate * 100, 1)}%"
)
cli::cli_alert_info(
  "Hub seeding -- super-spreader AR: \\
   {round(results_hubs$super_spreader_attack_rate * 100, 1)}%, \\
   non-SS AR: {round(results_hubs$non_super_spreader_attack_rate * 100, 1)}%"
)

# Q: Which seeding strategy produces a faster-rising epidemic?
#    Which produces a higher peak?
#    What does this imply for early containment policy?

# Extension (fast finishers): Run both seeding strategies 10 times each.
# Does hub seeding always produce a faster epidemic, or does stochastic
# variation sometimes reverse the result?  At what value of m does
# seeding strategy matter most?
# YOUR CODE HERE
