## ---- pp-setup ---------------------------------------------------------------
pkgload::load_all(here::here(), quiet = TRUE)

## ---- pp-run-model -----------------------------------------------------------
# seed = 18 on a 20x20 grid shows two clear boom-bust cycles without
# extinction in the first 300 steps (see Caddena.md for rationale).
abm_model <- PredatorPreyModel$new(seed = 18L, width = 20L, height = 20L)
abm_out   <- abm_model$run(steps = 300L)

## ---- pp-summary -------------------------------------------------------------
final_step <- max(abm_out$results$step)
final_s <- abm_out$results$value[
  abm_out$results$step == final_step & abm_out$results$variable == "sheep"]
final_w <- abm_out$results$value[
  abm_out$results$step == final_step & abm_out$results$variable == "wolves"]
cat(sprintf("Extinct: %s   Step %d: sheep = %d, wolves = %d\n",
            abm_out$extinct, final_step, final_s, final_w))

## ---- pp-time-series ---------------------------------------------------------
abm_colours <- c(sheep = COURSE_PALETTE[["sd"]], wolves = COURSE_PALETTE[["abm"]])
plot_time_series(abm_out$results,
                 title      = "Predator-prey ABM: sheep and wolves",
                 colour_map = abm_colours,
                 theme_fn   = theme_course_light)

## ---- pp-phase-portrait ------------------------------------------------------
abm_wide <- tidyr::pivot_wider(abm_out$results,
                                names_from  = "variable",
                                values_from = "value")
plot_phase_portrait(
  data.frame(x = abm_wide$sheep, y = abm_wide$wolves),
  xlabel   = "Sheep",
  ylabel   = "Wolves",
  title    = "ABM phase portrait",
  theme_fn = theme_course_light
)

## ---- pp-vs-ode --------------------------------------------------------------
lv         <- run_lv()
lv_colours <- c(prey = COURSE_PALETTE[["sd"]], predators = COURSE_PALETTE[["abm"]])
p_ode <- plot_time_series(lv$results,
                          title      = "ODE: prey and predators",
                          colour_map = lv_colours,
                          theme_fn   = theme_course_light)
p_abm <- plot_time_series(abm_out$results,
                          title      = "ABM: sheep and wolves (20\u00d720, seed = 18)",
                          colour_map = abm_colours,
                          theme_fn   = theme_course_light)
plot_side_by_side(p_ode, p_abm,
                  title    = "Lotka-Volterra ODE vs predator-prey ABM",
                  theme_fn = theme_course_light)

## ---- pp-extinction-demo -----------------------------------------------------
# 20 replicates to demonstrate stochastic extinction probability
ep <- find_abm_extinction_probability(
  params   = LV_DEFAULTS,
  n_runs   = 20L,
  steps    = 500L,
  width    = 20L,
  height   = 20L
)
cat(sprintf("Extinction probability (20x20, 500 steps, 20 runs): %.0f%%\n",
            ep$extinction_prob * 100))
cat(sprintf("Mean extinction step: %.1f\n", ep$mean_time_to_extinction))

## ---- pp-time-series-dark ----------------------------------------------------
plot_time_series(abm_out$results,
                 title      = "Predator-prey ABM: sheep and wolves",
                 colour_map = abm_colours,
                 theme_fn   = theme_course_dark)

## ---- pp-phase-portrait-dark -------------------------------------------------
plot_phase_portrait(
  data.frame(x = abm_wide$sheep, y = abm_wide$wolves),
  xlabel   = "Sheep",
  ylabel   = "Wolves",
  title    = "ABM phase portrait",
  theme_fn = theme_course_dark
)

## ---- pp-vs-ode-dark ---------------------------------------------------------
p_ode_d <- plot_time_series(lv$results,
                             title      = "ODE: prey and predators",
                             colour_map = lv_colours,
                             theme_fn   = theme_course_dark)
p_abm_d <- plot_time_series(abm_out$results,
                             title      = "ABM: sheep and wolves",
                             colour_map = abm_colours,
                             theme_fn   = theme_course_dark)
plot_side_by_side(p_ode_d, p_abm_d,
                  title    = "Lotka-Volterra ODE vs predator-prey ABM",
                  theme_fn = theme_course_dark)
