## ---- isr-setup --------------------------------------------------------------
pkgload::load_all(here::here(), quiet = TRUE)

## ---- isr-run-model ----------------------------------------------------------
params <- ISR_DEFAULTS
isr    <- run_isr(params = params)

## ---- isr-summary ------------------------------------------------------------
cat(sprintf("R0 = %.2f   (beta/gamma = %.2f/%.2f)\n",
            isr$R0, params$beta, params$gamma))
cat(sprintf("Herd immunity threshold: %.1f%%\n",
            herd_immunity_threshold(params) * 100))
cat(sprintf("Peak I = %.1f%% at t = %.1f\n",
            isr$peak_I * 100, isr$peak_time))
cat(sprintf("Final R (epidemic size) = %.1f%%\n",
            isr$final_R * 100))

## ---- isr-time-series --------------------------------------------------------
isr_colours <- c(I = COURSE_PALETTE[["abm"]],
                 S = COURSE_PALETTE[["sd"]],
                 R = COURSE_PALETTE[["extended"]])
plot_time_series(isr$results,
                 title      = "ISR ODE: epidemic compartments",
                 colour_map = isr_colours,
                 theme_fn   = theme_course_light)

## ---- isr-time-series-dark ---------------------------------------------------
plot_time_series(isr$results,
                 title      = "ISR ODE: epidemic compartments",
                 colour_map = isr_colours,
                 theme_fn   = theme_course_dark)
