#' Predator-prey ABM with grass dynamics
#'
#' @description Extends `PredatorPreyModel` with a grass layer. Sheep gain
#'   energy only from fully grown grass cells. Eaten grass regrows after
#'   `grass_regrowth_time` steps. All wolf and sheep agent logic is inherited
#'   from the parent class.
#'
#' @export
PredatorPreyGrassModel <- R6::R6Class("PredatorPreyGrassModel",
  inherit = PredatorPreyModel,

  public = list(
    #' @field grass Logical matrix (`height` x `width`); `TRUE` = grass present.
    grass               = NULL,
    #' @field grass_countdown Integer matrix of regrowth timers.
    grass_countdown     = NULL,
    #' @field grass_regrowth_time Ticks required for grazed grass to regrow.
    grass_regrowth_time = NULL,

    #' @description Initialise the grass-extension model.
    #' @param ... All arguments passed through to `PredatorPreyModel$initialize()`.
    #' @param grass_regrowth_time Ticks for grass to regrow. Default `30L`.
    #' @param initial_grass_fraction Fraction of cells starting with grass.
    #'   Default `0.5`.
    #' @param seed Optional integer random seed for reproducibility.
    initialize = function(
      ...,
      grass_regrowth_time    = 30L,
      initial_grass_fraction = 0.5,
      seed                   = NULL
    ) {
      if (!is.null(seed)) set.seed(seed)
      super$initialize(..., seed = NULL)   # seed already applied above

      self$grass_regrowth_time <- as.integer(grass_regrowth_time)

      # Initialise grass grid: logical matrix
      n_cells     <- self$width * self$height
      grown_cells <- sample(
        c(TRUE, FALSE),
        n_cells,
        replace = TRUE,
        prob    = c(initial_grass_fraction, 1 - initial_grass_fraction)
      )
      self$grass <- matrix(grown_cells, nrow = self$height, ncol = self$width)

      # Countdown matrix: 0 for grown cells, random partial countdown for bare
      self$grass_countdown <- matrix(0L, nrow = self$height, ncol = self$width)
      bare_idx <- which(!self$grass)
      self$grass_countdown[bare_idx] <- sample.int(
        self$grass_regrowth_time, length(bare_idx), replace = TRUE
      )
    },

    #' @description Advance the model one tick, including grass dynamics.
    step = function() {
      # Tick down grass countdowns and regrow where ready
      bare_mask <- !self$grass
      self$grass_countdown[bare_mask] <- self$grass_countdown[bare_mask] - 1L
      ready_mask <- self$grass_countdown <= 0L & bare_mask
      self$grass[ready_mask]          <- TRUE
      self$grass_countdown[ready_mask] <- 0L

      # Override sheep gain-from-food: zero on bare cells, normal on grass cells
      # We patch each sheep's step by temporarily adjusting energy here instead
      # of modifying the base Sheep class. We override the gain by hooking into
      # the model params after position update.
      # This is handled inside the overridden step: for each sheep, check grass
      # at their *post-move* cell and set energy gain accordingly.
      # We achieve this by temporarily setting sheep_gain_from_food to 0 and
      # applying grass gains manually after each sheep step.

      # Shuffle all agents and step them
      all_agents <- c(self$sheep, self$wolves)
      idx        <- sample.int(length(all_agents))

      orig_gain  <- self$params$sheep_gain_from_food
      # Set gain to 0 so Sheep$step doesn't add energy; we'll do it ourselves
      self$params$sheep_gain_from_food <- 0

      for (i in idx) {
        agent <- all_agents[[i]]
        if (!agent$alive) next
        agent$step(self)

        # For sheep: apply grass gain if current cell has grass
        if (inherits(agent, "Sheep") && agent$alive) {
          if (self$grass[agent$y, agent$x]) {
            agent$energy           <- agent$energy + orig_gain
            self$grass[agent$y, agent$x]  <- FALSE
            self$grass_countdown[agent$y, agent$x] <- self$grass_regrowth_time
          }
        }
      }

      # Restore original gain for consistency
      self$params$sheep_gain_from_food <- orig_gain

      # Purge dead agents
      self$sheep  <- Filter(function(a) a$alive, self$sheep)
      self$wolves <- Filter(function(a) a$alive, self$wolves)

      self$step_count <- self$step_count + 1L
      invisible(NULL)
    },

    #' @description Run the model for a specified number of steps.
    #' @param steps Number of ticks to run. Default `500L`.
    #' @return Named list:
    #'   - `results`: long-format tibble with columns `step`, `variable`,
    #'       `value` (`variable` in `c("sheep", "wolves", "grass_fraction")`)
    #'   - `extinct`: logical, `TRUE` if either animal population hit zero
    #'   - `params`: the params list used
    run = function(steps = 500L) {
      n_cells <- self$width * self$height

      records <- vector("list", steps + 1L)
      records[[1L]] <- data.frame(
        step     = 0L,
        variable = c("sheep", "wolves", "grass_fraction"),
        value    = c(
          length(self$sheep),
          length(self$wolves),
          sum(self$grass) / n_cells
        )
      )

      for (s in seq_len(steps)) {
        self$step()
        records[[s + 1L]] <- data.frame(
          step     = s,
          variable = c("sheep", "wolves", "grass_fraction"),
          value    = c(
            length(self$sheep),
            length(self$wolves),
            sum(self$grass) / n_cells
          )
        )

        if (length(self$sheep) == 0L || length(self$wolves) == 0L) {
          for (r in seq(s + 2L, steps + 1L)) {
            records[[r]] <- data.frame(
              step     = r - 1L,
              variable = c("sheep", "wolves", "grass_fraction"),
              value    = c(
                length(self$sheep),
                length(self$wolves),
                sum(self$grass) / n_cells
              )
            )
          }
          break
        }
      }

      results <- dplyr::as_tibble(do.call(rbind, records))
      self$history <- results

      list(
        results = results,
        extinct = any(
          results$value[results$step == max(results$step) &
                          results$variable != "grass_fraction"] == 0L
        ),
        params  = self$params
      )
    }
  )
)
