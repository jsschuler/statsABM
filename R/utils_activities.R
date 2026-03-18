#' Estimate ABM extinction probability across replicate runs
#'
#' Runs the predator-prey ABM with `n_runs` independent seeds and computes
#' the fraction of runs in which either population reached zero. Designed
#' for Activity 1 (ODE vs ABM extinction comparison). The `params` argument
#' accepts a named list of Lotka-Volterra parameters for documentation
#' purposes; the ABM uses its own default energy/reproduction parameters,
#' which are ecologically comparable to the LV defaults.
#'
#' @param params Named list — typically LV parameters (`alpha`, `beta`,
#'   `gamma`, `delta`) identifying the ecological regime being studied.
#'   Currently stored for reference only; the ABM uses independent default
#'   energy/reproduction parameters.
#' @param n_runs Number of independent replications. Default `20L`.
#' @param steps Number of ticks per run. Default `500L`.
#' @param width Grid width. Default `20L`.
#' @param height Grid height. Default `20L`.
#' @param n_sheep Initial sheep count. Default `100L`.
#' @param n_wolves Initial wolf count. Default `25L`.
#' @return Named list:
#'   - `extinction_prob`: fraction of runs where either population hit 0.
#'   - `mean_time_to_extinction`: mean step at first extinction event across
#'     extinct runs; `NA` if no run went extinct.
#'   - `runs`: tibble with columns `run`, `extinct`, `ext_step` (step of
#'     first extinction event, or `NA` if not extinct).
#' @export
#' @examples
#' ep <- find_abm_extinction_probability(params = LV_DEFAULTS, n_runs = 5L,
#'                                       steps = 200L)
#' ep$extinction_prob
find_abm_extinction_probability <- function(
  params,
  n_runs   = 20L,
  steps    = 500L,
  width    = 20L,
  height   = 20L,
  n_sheep  = 100L,
  n_wolves = 25L
) {
  n_runs <- as.integer(n_runs)
  runs_list <- vector("list", n_runs)

  for (i in seq_len(n_runs)) {
    m   <- PredatorPreyModel$new(
      width    = as.integer(width),
      height   = as.integer(height),
      n_sheep  = as.integer(n_sheep),
      n_wolves = as.integer(n_wolves),
      seed     = i
    )
    out <- m$run(steps = as.integer(steps))

    wolf_s  <- out$results$value[out$results$variable == "wolves"]
    sheep_s <- out$results$value[out$results$variable == "sheep"]
    w_ext   <- which(wolf_s  == 0L)[1L]
    s_ext   <- which(sheep_s == 0L)[1L]
    ext_idx <- min(c(w_ext, s_ext), na.rm = TRUE)

    runs_list[[i]] <- data.frame(
      run      = i,
      extinct  = out$extinct,
      ext_step = if (is.infinite(ext_idx)) NA_integer_
                 else as.integer(ext_idx - 1L)
    )
  }

  runs_df   <- dplyr::as_tibble(do.call(rbind, runs_list))
  ext_steps <- runs_df$ext_step[!is.na(runs_df$ext_step)]

  list(
    extinction_prob         = mean(runs_df$extinct),
    mean_time_to_extinction = if (length(ext_steps) > 0L) mean(ext_steps)
                             else NA_real_,
    runs                    = runs_df
  )
}


#' Plot multiple ABM density runs against an ODE trajectory
#'
#' Shows a row of time-series panels — one for each entry in `abm_list` —
#' with the ODE trajectory scaled to the ABM's initial population displayed
#' in each panel for direct comparison. Used in Activity 2 to make the
#' density-dependence of the mean-field approximation visually explicit.
#'
#' @param abm_list Named list of ABM run results (each from
#'   `PredatorPreyModel$run()`). Names become panel sub-titles (e.g.
#'   `list(sparse = ..., medium = ..., dense = ...)`).
#' @param ode Result list from `run_lv()`.
#' @param n_sheep Initial sheep count used in the ABM runs. Used to scale
#'   the ODE trajectory onto the same y-axis. Default `100L`.
#' @param n_prey Initial prey count used by the ODE. Default `40` (matches
#'   `LV_DEFAULTS` initial condition `y0 = c(prey = 40, predators = 9)`).
#' @param theme_fn A ggplot2 theme function. Default `theme_course_light`.
#' @return A patchwork object.
#' @export
#' @examples
#' ode   <- run_lv()
#' dense <- PredatorPreyModel$new(width=20L, height=20L, seed=42L)$run(300L)
#' plot_abm_ode_comparison(list(dense = dense), ode)
plot_abm_ode_comparison <- function(
  abm_list,
  ode,
  n_sheep = 100L,
  n_prey  = 40,
  theme_fn = theme_course_light
) {
  # Scaling factor: bring ODE prey counts onto ABM sheep scale
  scale <- as.numeric(n_sheep) / as.numeric(n_prey)

  ode_scaled <- ode$results
  ode_scaled$value    <- ode_scaled$value * scale
  ode_scaled$variable <- dplyr::recode(
    ode_scaled$variable, prey = "sheep", predators = "wolves"
  )
  ode_scaled$source <- "ODE"

  colours <- c(
    sheep  = unname(COURSE_PALETTE["sd"]),
    wolves = unname(COURSE_PALETTE["abm"])
  )

  plots <- lapply(names(abm_list), function(nm) {
    abm_res <- abm_list[[nm]]$results
    abm_res$source <- "ABM"

    combined <- rbind(abm_res[, c("step", "variable", "value")],
                      ode_scaled[, c("time", "variable", "value")] |>
                        dplyr::rename(step = "time"))

    # ABM as solid lines; ODE as dashed
    p <- ggplot2::ggplot() +
      ggplot2::geom_line(
        data    = ode_scaled,
        mapping = ggplot2::aes(
          x = .data[["time"]], y = .data[["value"]],
          colour = .data[["variable"]]
        ),
        linetype  = "dashed", linewidth = 0.7, alpha = 0.6
      ) +
      ggplot2::geom_line(
        data    = abm_res,
        mapping = ggplot2::aes(
          x = .data[["step"]], y = .data[["value"]],
          colour = .data[["variable"]]
        ),
        linewidth = 0.9
      ) +
      ggplot2::scale_colour_manual(values = colours) +
      ggplot2::labs(
        x      = "step / time",
        y      = "count",
        title  = nm,
        colour = NULL
      ) +
      theme_fn()
    p
  })

  patchwork::wrap_plots(plots, nrow = 1L) +
    patchwork::plot_annotation(
      title = "ABM (solid) vs ODE (dashed, scaled to ABM initial count)",
      theme = theme_fn()
    )
}


#' Summarise key statistics across a list of pandemic model runs
#'
#' Extracts `peak_I`, `peak_step`, `final_R`, and `R0_empirical` from each
#' run and returns them in a tidy tibble, with a trailing summary row
#' containing the mean and SD across runs.
#'
#' @param runs List of result lists from `PandemicModel$run()`.
#' @return Tibble with columns `run` (integer or `"mean"` / `"sd"`),
#'   `peak_I`, `peak_step`, `final_R`, `R0_empirical`.
#' @export
#' @examples
#' runs <- lapply(1:3, function(s) PandemicModel$new(seed=s)$run(50L))
#' summarise_runs(runs)
summarise_runs <- function(runs) {
  rows <- lapply(seq_along(runs), function(i) {
    r      <- runs[[i]]
    res    <- r$results
    I_vals <- res$value[res$variable == "I"]
    R_vals <- res$value[res$variable == "R"]
    data.frame(
      run          = i,
      peak_I       = r$peak_I,
      peak_step    = r$peak_step,
      final_R      = R_vals[length(R_vals)],
      R0_empirical = r$R0_empirical
    )
  })

  df <- dplyr::as_tibble(do.call(rbind, rows))

  finite_cols <- c("peak_I", "peak_step", "final_R", "R0_empirical")
  mn <- vapply(df[finite_cols], function(x) mean(x, na.rm = TRUE), numeric(1L))
  sd_v <- vapply(df[finite_cols], function(x) stats::sd(x,   na.rm = TRUE), numeric(1L))

  summary_rows <- dplyr::as_tibble(
    rbind(
      data.frame(run = "mean", t(mn),  stringsAsFactors = FALSE),
      data.frame(run = "sd",   t(sd_v), stringsAsFactors = FALSE)
    )
  )
  # run column type mismatch: coerce df$run to character
  df$run <- as.character(df$run)

  dplyr::bind_rows(df, summary_rows)
}


#' Plot mean ABM epidemic curve with +/- 1 SD band against ODE
#'
#' Overlays the mean infected fraction across all ABM runs (with a shaded
#' ±1 SD band) on the ODE infected curve. Used in Activity 3.
#'
#' @param runs List of result lists from `PandemicModel$run()`.
#' @param ode Result list from `run_isr()`.
#' @param theme_fn A ggplot2 theme function. Default `theme_course_light`.
#' @return A ggplot object.
#' @export
#' @examples
#' runs <- lapply(1:3, function(s) PandemicModel$new(seed=s)$run(50L))
#' ode  <- run_isr()
#' plot_epidemic_comparison(runs, ode)
plot_epidemic_comparison <- function(runs, ode, theme_fn = theme_course_light) {
  all_I <- do.call(rbind, lapply(seq_along(runs), function(i) {
    r      <- runs[[i]]$results
    i_rows <- r[r$variable == "I", ]
    i_rows$run <- i
    i_rows
  }))

  abm_summary <- all_I |>
    dplyr::group_by(.data[["step"]]) |>
    dplyr::summarise(
      mean_I = mean(.data[["value"]]),
      sd_I   = stats::sd(.data[["value"]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      lo = pmax(0, .data[["mean_I"]] - .data[["sd_I"]]),
      hi = pmin(1, .data[["mean_I"]] + .data[["sd_I"]])
    )

  ode_I <- ode$results[ode$results$variable == "I", ]

  n_runs <- length(runs)

  ggplot2::ggplot() +
    ggplot2::geom_ribbon(
      data    = abm_summary,
      mapping = ggplot2::aes(
        x = .data[["step"]], ymin = .data[["lo"]], ymax = .data[["hi"]]
      ),
      fill  = COURSE_PALETTE[["abm"]], alpha = 0.25
    ) +
    ggplot2::geom_line(
      data    = abm_summary,
      mapping = ggplot2::aes(
        x = .data[["step"]], y = .data[["mean_I"]], colour = "ABM mean"
      ),
      linewidth = 0.9
    ) +
    ggplot2::geom_line(
      data    = ode_I,
      mapping = ggplot2::aes(
        x = .data[["time"]], y = .data[["value"]], colour = "ODE"
      ),
      linewidth = 0.9
    ) +
    ggplot2::scale_colour_manual(
      values = c("ODE" = COURSE_PALETTE[["sd"]],
                 "ABM mean" = COURSE_PALETTE[["abm"]]),
      name   = NULL
    ) +
    ggplot2::labs(
      x     = "time / step",
      y     = "infected fraction",
      title = sprintf("ISR: ODE vs ABM (%d runs, shading = mean \u00b1 1 SD)", n_runs)
    ) +
    theme_fn()
}


#' Compute extinction probability across grass regrowth times
#'
#' For each value in `regrowth_times`, runs `n_runs` independent
#' `PredatorPreyGrassModel` replicates and computes the fraction that went
#' extinct within `steps` ticks. Returns a tibble suitable for
#' `plot_extinction_curve()`.
#'
#' @param regrowth_times Integer vector of `grass_regrowth_time` values.
#' @param n_runs Number of replications per regrowth time. Default `20L`.
#' @param steps Steps per run. Default `500L`.
#' @param width Grid width. Default `20L`.
#' @param height Grid height. Default `20L`.
#' @return Tibble with columns `regrowth_time` (integer), `extinction_prob`
#'   (numeric in [0, 1]), `se` (standard error of proportion).
#' @export
#' @examples
#' curve <- find_grass_extinction_curve(c(10L, 30L, 60L), n_runs = 3L,
#'                                      steps = 100L)
#' curve
find_grass_extinction_curve <- function(
  regrowth_times,
  n_runs = 20L,
  steps  = 500L,
  width  = 20L,
  height = 20L
) {
  n_runs <- as.integer(n_runs)
  rows   <- vector("list", length(regrowth_times))

  for (j in seq_along(regrowth_times)) {
    rt      <- as.integer(regrowth_times[j])
    extinct <- logical(n_runs)
    for (i in seq_len(n_runs)) {
      m <- PredatorPreyGrassModel$new(
        grass_regrowth_time = rt,
        width               = as.integer(width),
        height              = as.integer(height),
        seed                = i
      )
      out        <- m$run(steps = as.integer(steps))
      extinct[i] <- out$extinct
    }
    p <- mean(extinct)
    rows[[j]] <- data.frame(
      regrowth_time  = rt,
      extinction_prob = p,
      se             = sqrt(p * (1 - p) / n_runs)
    )
  }

  dplyr::as_tibble(do.call(rbind, rows))
}


#' Plot extinction probability as a function of grass regrowth time
#'
#' Displays the output of `find_grass_extinction_curve()` as a line plot
#' with ±1 SE error bars, with the minimum extinction probability marked.
#'
#' @param results Tibble from `find_grass_extinction_curve()`.
#' @param theme_fn A ggplot2 theme function. Default `theme_course_light`.
#' @return A ggplot object.
#' @export
#' @examples
#' curve <- find_grass_extinction_curve(c(10L, 30L, 60L), n_runs = 3L,
#'                                      steps = 100L)
#' plot_extinction_curve(curve)
plot_extinction_curve <- function(results, theme_fn = theme_course_light) {
  opt_row <- results[which.min(results$extinction_prob), ]

  ggplot2::ggplot(
    results,
    ggplot2::aes(
      x = .data[["regrowth_time"]],
      y = .data[["extinction_prob"]]
    )
  ) +
    ggplot2::geom_ribbon(
      ggplot2::aes(
        ymin = pmax(0, .data[["extinction_prob"]] - .data[["se"]]),
        ymax = pmin(1, .data[["extinction_prob"]] + .data[["se"]])
      ),
      fill = COURSE_PALETTE[["extended"]], alpha = 0.25
    ) +
    ggplot2::geom_line(
      colour = COURSE_PALETTE[["extended"]], linewidth = 0.9
    ) +
    ggplot2::geom_point(
      colour = COURSE_PALETTE[["extended"]], size = 2.5
    ) +
    ggplot2::geom_point(
      data   = opt_row,
      colour = COURSE_PALETTE[["abm"]], size = 4, shape = 18
    ) +
    ggplot2::scale_y_continuous(limits = c(0, 1)) +
    ggplot2::labs(
      x     = "grass regrowth time (ticks)",
      y     = "extinction probability",
      title = "Extinction probability vs grass regrowth time",
      caption = "Diamond: minimum extinction probability"
    ) +
    theme_fn()
}
