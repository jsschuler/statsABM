## ---- pandemic-setup ---------------------------------------------------------
pkgload::load_all(here::here(), quiet = TRUE)

## ---- pandemic-run-model -----------------------------------------------------
pandemic_model <- PandemicModel$new(seed = 42L)
pandemic_out   <- pandemic_model$run(steps = 200L)

## ---- pandemic-summary -------------------------------------------------------
cat(sprintf("Empirical R0: %.2f   (theoretical: %.1f)\n",
            pandemic_out$R0_empirical,
            ISR_DEFAULTS$beta / ISR_DEFAULTS$gamma))
cat(sprintf("Peak I: %.1f%% at step %d\n",
            pandemic_out$peak_I * 100, pandemic_out$peak_step))

## ---- pandemic-time-series ---------------------------------------------------
isr_colours <- c(I = COURSE_PALETTE[["abm"]],
                 S = COURSE_PALETTE[["sd"]],
                 R = COURSE_PALETTE[["extended"]])
plot_time_series(pandemic_out$results,
                 title      = "Pandemic ABM: ISR epidemic (well-mixed grid)",
                 colour_map = isr_colours,
                 theme_fn   = theme_course_light)

## ---- pandemic-vs-ode --------------------------------------------------------
isr      <- run_isr()
N_RUNS   <- 10L
abm_runs <- lapply(seq_len(N_RUNS), function(s) {
  PandemicModel$new(seed = s)$run(steps = 200L)
})
p_ode_isr <- plot_time_series(
  isr$results[isr$results$variable == "I", ],
  title      = "ISR ODE",
  colour_map = c(I = COURSE_PALETTE[["sd"]]),
  theme_fn   = theme_course_light
)
p_overlay <- plot_epidemic_comparison(abm_runs, ode = isr,
                                       theme_fn = theme_course_light)
plot_side_by_side(p_ode_isr, p_overlay,
                  title    = "ISR: system dynamics vs agent-based",
                  theme_fn = theme_course_light)

## ---- pandemic-time-series-dark ----------------------------------------------
plot_time_series(pandemic_out$results,
                 title      = "Pandemic ABM: ISR epidemic (well-mixed grid)",
                 colour_map = isr_colours,
                 theme_fn   = theme_course_dark)

## ---- pandemic-vs-ode-dark ---------------------------------------------------
p_ode_d  <- plot_time_series(
  isr$results[isr$results$variable == "I", ],
  title      = "ISR ODE",
  colour_map = c(I = COURSE_PALETTE[["sd"]]),
  theme_fn   = theme_course_dark
)
p_over_d <- plot_epidemic_comparison(abm_runs, ode = isr,
                                      theme_fn = theme_course_dark)
plot_side_by_side(p_ode_d, p_over_d,
                  title    = "ISR: system dynamics vs agent-based",
                  theme_fn = theme_course_dark)
