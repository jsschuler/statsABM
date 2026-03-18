## ---- network-setup ----------------------------------------------------------
pkgload::load_all(here::here(), quiet = TRUE)

## ---- network-run-model ------------------------------------------------------
net_model <- NetworkPandemicModel$new(seed = 42L)
net_out   <- net_model$run(steps = 200L)

## ---- network-summary --------------------------------------------------------
cat(sprintf("Empirical R0: %.2f\n", net_out$R0_empirical))
cat(sprintf("Peak I: %.1f%% at step %d\n",
            net_out$peak_I * 100, net_out$peak_step))
cat(sprintf("Super-spreader attack rate:     %.1f%%\n",
            net_out$super_spreader_attack_rate * 100))
cat(sprintf("Non-super-spreader attack rate: %.1f%%\n",
            net_out$non_super_spreader_attack_rate * 100))

## ---- network-degree-dist ----------------------------------------------------
plot_distribution(
  as.integer(net_out$degree_distribution),
  title   = "BA network: degree distribution (log-log CCDF)",
  log_log = TRUE,
  theme_fn = theme_course_light
)

## ---- network-epidemic-comparison --------------------------------------------
isr_colours <- c(I = COURSE_PALETTE[["abm"]],
                 S = COURSE_PALETTE[["sd"]],
                 R = COURSE_PALETTE[["extended"]])
grid_out <- PandemicModel$new(seed = 42L)$run(steps = 200L)
p_grid <- plot_time_series(grid_out$results,
                            title      = "Grid ABM (well-mixed)",
                            colour_map = isr_colours,
                            theme_fn   = theme_course_light)
p_net  <- plot_time_series(net_out$results,
                            title      = "Network ABM (BA graph)",
                            colour_map = isr_colours,
                            theme_fn   = theme_course_light)
plot_side_by_side(p_grid, p_net,
                  title    = "ISR epidemic: well-mixed grid vs BA network",
                  theme_fn = theme_course_light)

## ---- network-hub-seeding ----------------------------------------------------
net_random <- NetworkPandemicModel$new(seed_hubs = FALSE, seed = 42L)$run(200L)
net_hubs   <- NetworkPandemicModel$new(seed_hubs = TRUE,  seed = 42L)$run(200L)
p_rand <- plot_time_series(net_random$results,
                            title      = "Random seeding",
                            colour_map = isr_colours,
                            theme_fn   = theme_course_light)
p_hub  <- plot_time_series(net_hubs$results,
                            title      = "Hub seeding",
                            colour_map = isr_colours,
                            theme_fn   = theme_course_light)
plot_side_by_side(p_rand, p_hub,
                  title    = "Hub seeding vs random seeding",
                  theme_fn = theme_course_light)

## ---- network-degree-dist-dark -----------------------------------------------
plot_distribution(
  as.integer(net_out$degree_distribution),
  title    = "BA network: degree distribution (log-log CCDF)",
  log_log  = TRUE,
  theme_fn = theme_course_dark
)

## ---- network-epidemic-comparison-dark ---------------------------------------
p_grid_d <- plot_time_series(grid_out$results,
                              title      = "Grid ABM (well-mixed)",
                              colour_map = isr_colours,
                              theme_fn   = theme_course_dark)
p_net_d  <- plot_time_series(net_out$results,
                              title      = "Network ABM (BA graph)",
                              colour_map = isr_colours,
                              theme_fn   = theme_course_dark)
plot_side_by_side(p_grid_d, p_net_d,
                  title    = "ISR epidemic: well-mixed grid vs BA network",
                  theme_fn = theme_course_dark)

## ---- network-hub-seeding-dark -----------------------------------------------
p_rand_d <- plot_time_series(net_random$results,
                              title      = "Random seeding",
                              colour_map = isr_colours,
                              theme_fn   = theme_course_dark)
p_hub_d  <- plot_time_series(net_hubs$results,
                              title      = "Hub seeding",
                              colour_map = isr_colours,
                              theme_fn   = theme_course_dark)
plot_side_by_side(p_rand_d, p_hub_d,
                  title    = "Hub seeding vs random seeding",
                  theme_fn = theme_course_dark)
