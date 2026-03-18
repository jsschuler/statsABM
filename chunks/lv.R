## ---- lv-setup ---------------------------------------------------------------
pkgload::load_all(here::here(), quiet = TRUE)

## ---- lv-run-model -----------------------------------------------------------
params <- LV_DEFAULTS
lv     <- run_lv(params = params)

## ---- lv-equilibrium ---------------------------------------------------------
cat(sprintf("Equilibrium: prey* = %.1f,  predators* = %.1f\n",
            lv$equilibrium["prey"], lv$equilibrium["predators"]))

## ---- lv-time-series ---------------------------------------------------------
lv_colours <- c(prey = COURSE_PALETTE[["sd"]], predators = COURSE_PALETTE[["abm"]])
plot_time_series(lv$results,
                 title      = "Lotka-Volterra ODE: prey and predators",
                 colour_map = lv_colours,
                 theme_fn   = theme_course_light)

## ---- lv-phase-portrait ------------------------------------------------------
phase_wide <- tidyr::pivot_wider(lv$results,
                                  names_from  = "variable",
                                  values_from = "value")
plot_phase_portrait(
  data.frame(x = phase_wide$prey, y = phase_wide$predators),
  xlabel   = "Prey",
  ylabel   = "Predators",
  title    = "ODE phase portrait",
  theme_fn = theme_course_light
)

## ---- lv-time-series-dark ----------------------------------------------------
plot_time_series(lv$results,
                 title      = "Lotka-Volterra ODE: prey and predators",
                 colour_map = lv_colours,
                 theme_fn   = theme_course_dark)

## ---- lv-phase-portrait-dark -------------------------------------------------
plot_phase_portrait(
  data.frame(x = phase_wide$prey, y = phase_wide$predators),
  xlabel   = "Prey",
  ylabel   = "Predators",
  title    = "ODE phase portrait",
  theme_fn = theme_course_dark
)
