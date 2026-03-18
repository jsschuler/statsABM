## ---- grass-setup ------------------------------------------------------------
pkgload::load_all(here::here(), quiet = TRUE)

## ---- grass-run-model --------------------------------------------------------
grass_model <- PredatorPreyGrassModel$new(seed = 10L, width = 20L, height = 20L)
grass_out   <- grass_model$run(steps = 300L)

## ---- grass-summary ----------------------------------------------------------
final_step <- max(grass_out$results$step)
g_frac <- grass_out$results$value[
  grass_out$results$step == final_step &
  grass_out$results$variable == "grass_fraction"]
cat(sprintf("Extinct: %s   Final grass fraction: %.2f\n",
            grass_out$extinct, g_frac))

## ---- grass-time-series ------------------------------------------------------
abm_colours  <- c(sheep = COURSE_PALETTE[["sd"]], wolves = COURSE_PALETTE[["abm"]])
animal_res   <- grass_out$results[grass_out$results$variable != "grass_fraction", ]
grass_res    <- grass_out$results[grass_out$results$variable == "grass_fraction", ]

p_animals <- plot_time_series(animal_res,
                               title      = "Grass ABM: sheep and wolves",
                               colour_map = abm_colours,
                               theme_fn   = theme_course_light)
p_grass   <- plot_time_series(grass_res,
                               title      = "Grass ABM: grass cover fraction",
                               colour_map = c(grass_fraction = COURSE_PALETTE[["extended"]]),
                               theme_fn   = theme_course_light)
plot_side_by_side(p_animals, p_grass,
                  title    = "Predator-prey with grass: animals and vegetation",
                  theme_fn = theme_course_light)

## ---- grass-vs-baseline ------------------------------------------------------
baseline_out <- PredatorPreyModel$new(seed = 18L, width = 20L, height = 20L
                                      )$run(steps = 300L)
p_base <- plot_time_series(baseline_out$results,
                            title      = "Baseline: no grass (seed = 18)",
                            colour_map = abm_colours,
                            theme_fn   = theme_course_light)
p_grass_animals <- plot_time_series(animal_res,
                                     title      = "Grass extension (seed = 10)",
                                     colour_map = abm_colours,
                                     theme_fn   = theme_course_light)
plot_side_by_side(p_base, p_grass_animals,
                  title    = "ABM: baseline vs grass extension",
                  theme_fn = theme_course_light)

## ---- grass-extinction-curve -------------------------------------------------
ext_curve <- find_grass_extinction_curve(
  regrowth_times = c(5L, 15L, 30L, 60L, 120L),
  n_runs         = 20L,
  steps          = 300L,
  width          = 20L,
  height         = 20L
)
plot_extinction_curve(ext_curve, theme_fn = theme_course_light)

## ---- grass-time-series-dark -------------------------------------------------
plot_side_by_side(
  plot_time_series(animal_res,
                   title      = "Grass ABM: sheep and wolves",
                   colour_map = abm_colours,
                   theme_fn   = theme_course_dark),
  plot_time_series(grass_res,
                   title      = "Grass ABM: grass cover fraction",
                   colour_map = c(grass_fraction = COURSE_PALETTE[["extended"]]),
                   theme_fn   = theme_course_dark),
  title    = "Predator-prey with grass: animals and vegetation",
  theme_fn = theme_course_dark
)

## ---- grass-vs-baseline-dark -------------------------------------------------
plot_side_by_side(
  plot_time_series(baseline_out$results,
                   title      = "Baseline: no grass",
                   colour_map = abm_colours,
                   theme_fn   = theme_course_dark),
  plot_time_series(animal_res,
                   title      = "Grass extension",
                   colour_map = abm_colours,
                   theme_fn   = theme_course_dark),
  title    = "ABM: baseline vs grass extension",
  theme_fn = theme_course_dark
)

## ---- grass-extinction-curve-dark --------------------------------------------
plot_extinction_curve(ext_curve, theme_fn = theme_course_dark)
