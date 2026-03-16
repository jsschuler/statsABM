#' Person agent for the pandemic ABM
#'
#' @description Individual agent in the ISR pandemic model. Each person
#'   occupies a cell on a toroidal grid and can be Infected, Susceptible,
#'   or Recovered. Compartment ordering is ISR.
#'
#' @export
Person <- R6::R6Class("Person",
  public = list(
    #' @field id Unique integer agent identifier.
    id    = NULL,
    #' @field state Character: one of `"I"`, `"S"`, or `"R"`.
    state = NULL,
    #' @field x Grid column (1-indexed).
    x     = NULL,
    #' @field y Grid row (1-indexed).
    y     = NULL,

    #' @description Create a new Person agent.
    #' @param id Integer identifier.
    #' @param x Grid column.
    #' @param y Grid row.
    #' @param state Initial state: `"I"`, `"S"`, or `"R"`.
    initialize = function(id, x, y, state) {
      self$id    <- id
      self$x     <- x
      self$y     <- y
      self$state <- state
    },

    #' @description Execute one tick of person behaviour (ISR ordering).
    #' @param model The `PandemicModel` instance.
    step = function(model) {
      if (self$state == "S") {
        # Find infected neighbours in Moore neighbourhood (radius 1)
        for (dx in -1L:1L) {
          for (dy in -1L:1L) {
            if (dx == 0L && dy == 0L) next
            nx <- ((self$x - 1L + dx) %% model$width)  + 1L
            ny <- ((self$y - 1L + dy) %% model$height) + 1L
            # Check if any infected agent is at that cell
            if (.any_infected_at(model$agents, nx, ny)) {
              if (stats::runif(1) < model$params$beta) {
                self$state <- "I"
                return(invisible(NULL))
              }
            }
          }
        }
      } else if (self$state == "I") {
        if (stats::runif(1) < model$params$gamma) {
          self$state <- "R"
        }
      }
      # "R" stays "R"
      invisible(NULL)
    }
  )
)

# Internal helper: check if any agent in a list is infected at (x, y)
.any_infected_at <- function(agents, x, y) {
  for (a in agents) {
    if (a$state == "I" && a$x == x && a$y == y) return(TRUE)
  }
  FALSE
}

#' Pandemic ABM on a well-mixed grid
#'
#' @description ISR epidemic model on a toroidal grid. Agents move randomly
#'   and transmission occurs via Moore-neighbourhood contact, approximating
#'   well-mixed dynamics for comparison with the ODE model.
#'
#' @export
PandemicModel <- R6::R6Class("PandemicModel",
  public = list(
    #' @field width Grid width.
    width      = NULL,
    #' @field height Grid height.
    height     = NULL,
    #' @field params Named list of model parameters (`beta`, `gamma`).
    params     = NULL,
    #' @field agents List of `Person` R6 objects.
    agents     = NULL,
    #' @field step_count Current tick count.
    step_count = 0L,

    #' @description Initialise the pandemic model.
    #' @param width Grid width. Default `20L`.
    #' @param height Grid height. Default `20L`.
    #' @param n_agents Total number of agents. Default `400L`. Density of one
    #'   agent per cell keeps dynamics comparable across grid sizes. Larger
    #'   values (e.g. 2500 on a 50x50 grid) produce smoother curves but run
    #'   proportionally slower in single-threaded R.
    #' @param initial_infected_fraction Fraction of agents initially infected.
    #'   Default `0.01`.
    #' @param beta Transmission probability per infected neighbour per tick.
    #'   Default `0.3`.
    #' @param gamma Recovery probability per infected agent per tick.
    #'   Default `0.05`.
    #' @param seed Optional integer random seed for reproducibility.
    initialize = function(
      width                     = 20L,
      height                    = 20L,
      n_agents                  = 400L,
      initial_infected_fraction = 0.01,
      beta                      = 0.3,
      gamma                     = 0.05,
      seed                      = NULL
    ) {
      if (!is.null(seed)) set.seed(seed)

      self$width  <- as.integer(width)
      self$height <- as.integer(height)
      self$params <- list(beta = beta, gamma = gamma)

      n_agents   <- as.integer(n_agents)
      n_infected <- max(1L, as.integer(round(n_agents * initial_infected_fraction)))
      n_suscept  <- n_agents - n_infected

      states <- c(rep("I", n_infected), rep("S", n_suscept))

      self$agents <- vector("list", n_agents)
      for (i in seq_len(n_agents)) {
        self$agents[[i]] <- Person$new(
          id    = i,
          x     = sample.int(self$width,  1L),
          y     = sample.int(self$height, 1L),
          state = states[i]
        )
      }
    },

    #' @description Advance the model by one tick.
    #'
    #' Uses synchronous (snapshot-based) updates so that all agents see the
    #' same world state from the start of the tick.  Transmission is computed
    #' with a vectorised spatial index (O(n) per tick) rather than per-agent
    #' neighbour scans (O(n²)).  `Person$step()` is therefore not called here;
    #' the model drives all state transitions directly.
    step = function() {
      n <- length(self$agents)
      if (n == 0L) {
        self$step_count <- self$step_count + 1L
        return(invisible(NULL))
      }

      # --- Synchronous snapshot -----------------------------------------------
      xs     <- vapply(self$agents, function(a) a$x,     integer(1L))
      ys     <- vapply(self$agents, function(a) a$y,     integer(1L))
      states <- vapply(self$agents, function(a) a$state, character(1L))

      # --- Spatial index: which cells contain ≥1 infected agent? -------------
      infected_mat <- matrix(FALSE, nrow = self$height, ncol = self$width)
      inf_idx <- which(states == "I")
      for (i in inf_idx) infected_mat[ys[i], xs[i]] <- TRUE

      # --- Count infected Moore neighbours per cell (vectorised matrix shift) -
      rows      <- seq_len(self$height)
      cols      <- seq_len(self$width)
      n_inf_nbr <- matrix(0L, nrow = self$height, ncol = self$width)
      for (dy in -1L:1L) {
        for (dx in -1L:1L) {
          if (dy == 0L && dx == 0L) next
          sr        <- ((rows - 1L + dy) %% self$height) + 1L
          sc        <- ((cols - 1L + dx) %% self$width)  + 1L
          n_inf_nbr <- n_inf_nbr + infected_mat[sr, sc, drop = FALSE]
        }
      }

      # --- Compute new states -------------------------------------------------
      new_states <- states

      # Susceptible: P(infection) = 1 - (1-beta)^k  where k = infected neighbours
      s_idx <- which(states == "S")
      if (length(s_idx) > 0L) {
        k        <- n_inf_nbr[cbind(ys[s_idx], xs[s_idx])]
        p_inf    <- 1 - (1 - self$params$beta)^k
        become_I <- stats::runif(length(s_idx)) < p_inf
        new_states[s_idx[become_I]] <- "I"
      }

      # Infected: recover with probability gamma
      if (length(inf_idx) > 0L) {
        recover <- stats::runif(length(inf_idx)) < self$params$gamma
        new_states[inf_idx[recover]] <- "R"
      }

      # --- Apply new states ---------------------------------------------------
      for (i in seq_len(n)) self$agents[[i]]$state <- new_states[i]

      # --- Vectorised movement (well-mixed approximation) ---------------------
      dx_v  <- sample(c(-1L, 0L, 1L), n, replace = TRUE)
      dy_v  <- sample(c(-1L, 0L, 1L), n, replace = TRUE)
      new_x <- ((xs - 1L + dx_v) %% self$width)  + 1L
      new_y <- ((ys - 1L + dy_v) %% self$height) + 1L
      for (i in seq_len(n)) {
        self$agents[[i]]$x <- new_x[i]
        self$agents[[i]]$y <- new_y[i]
      }

      self$step_count <- self$step_count + 1L
      invisible(NULL)
    },

    #' @description Run the model for a specified number of steps.
    #' @param steps Number of ticks. Default `200L`.
    #' @return Named list:
    #'   - `results`: long-format tibble with columns `step`, `variable`,
    #'       `value` (`variable` in `c("I", "S", "R")` as fractions)
    #'   - `R0_empirical`: estimated from early exponential growth rate
    #'   - `peak_I`: peak infected fraction
    #'   - `peak_step`: step at peak
    #'   - `params`: the params list used
    run = function(steps = 200L) {
      n <- length(self$agents)

      .counts <- function() {
        states <- vapply(self$agents, function(a) a$state, character(1L))
        c(
          I = sum(states == "I") / n,
          S = sum(states == "S") / n,
          R = sum(states == "R") / n
        )
      }

      records <- vector("list", steps + 1L)
      init_c  <- .counts()
      records[[1L]] <- data.frame(
        step     = 0L,
        variable = c("I", "S", "R"),
        value    = unname(init_c)
      )

      for (s in seq_len(steps)) {
        self$step()
        c_now <- .counts()
        records[[s + 1L]] <- data.frame(
          step     = s,
          variable = c("I", "S", "R"),
          value    = unname(c_now)
        )
      }

      results <- dplyr::as_tibble(do.call(rbind, records))

      # Extract I time series for summary stats
      I_series  <- results$value[results$variable == "I"]
      peak_idx  <- which.max(I_series)

      # Empirical R0: estimated from slope of log(I) in the first 10% of steps
      early_end <- max(2L, as.integer(steps * 0.1))
      early_I   <- I_series[seq_len(early_end + 1L)]
      early_I   <- early_I[early_I > 0]
      R0_emp    <- NA_real_
      if (length(early_I) >= 3L) {
        fit    <- stats::lm(log(early_I) ~ seq_along(early_I))
        growth <- unname(stats::coef(fit)[2L])
        # R0 ~ (growth_rate / gamma) + 1 (approximate)
        R0_emp <- growth / self$params$gamma + 1
      }

      list(
        results      = results,
        R0_empirical = R0_emp,
        peak_I       = I_series[peak_idx],
        peak_step    = peak_idx - 1L,
        params       = self$params
      )
    }
  )
)
