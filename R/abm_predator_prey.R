#' Sheep agent
#'
#' @description Grazes on a plain grid. Reproduces probabilistically.
#'   Dies when energy reaches zero.
#'
#' @export
Sheep <- R6::R6Class("Sheep",
  public = list(
    #' @field id Unique integer agent identifier.
    id     = NULL,
    #' @field energy Current energy level (numeric).
    energy = NULL,
    #' @field x Grid column (1-indexed).
    x      = NULL,
    #' @field y Grid row (1-indexed).
    y      = NULL,
    #' @field alive Logical; `FALSE` once the agent has died.
    alive  = TRUE,

    #' @description Create a new Sheep agent.
    #' @param id Integer identifier.
    #' @param x Initial grid column.
    #' @param y Initial grid row.
    #' @param energy Initial energy.
    initialize = function(id, x, y, energy) {
      self$id     <- id
      self$x      <- x
      self$y      <- y
      self$energy <- energy
    },

    #' @description Execute one tick of sheep behaviour.
    #' @param model The `PredatorPreyModel` (or subclass) instance.
    step = function(model) {
      if (!self$alive) return(invisible(NULL))

      # 1. Move to a random Moore neighbor (toroidal wrap)
      dx <- sample(-1L:1L, 1L)
      dy <- sample(-1L:1L, 1L)
      self$x <- ((self$x - 1L + dx) %% model$width)  + 1L
      self$y <- ((self$y - 1L + dy) %% model$height) + 1L

      # 2. Graze (gain energy from grass if available, else use baseline)
      self$energy <- self$energy + model$params$sheep_gain_from_food

      # 3. Lose energy each tick
      self$energy <- self$energy - 1

      # 4. Reproduce probabilistically
      if (self$energy > model$params$sheep_reproduce_threshold &&
          stats::runif(1) < model$params$sheep_reproduce) {
        self$energy  <- self$energy / 2
        offspring_id <- model$.next_id()
        offspring    <- Sheep$new(offspring_id, self$x, self$y, self$energy)
        model$sheep  <- c(model$sheep, list(offspring))
      }

      # 5. Die if out of energy
      if (self$energy <= 0) self$alive <- FALSE

      invisible(NULL)
    }
  )
)

#' Wolf agent
#'
#' @description Hunts sheep on a plain grid. Reproduces probabilistically.
#'   Dies when energy reaches zero.
#'
#' @export
Wolf <- R6::R6Class("Wolf",
  public = list(
    #' @field id Unique integer agent identifier.
    id     = NULL,
    #' @field energy Current energy level (numeric).
    energy = NULL,
    #' @field x Grid column (1-indexed).
    x      = NULL,
    #' @field y Grid row (1-indexed).
    y      = NULL,
    #' @field alive Logical; `FALSE` once the agent has died.
    alive  = TRUE,

    #' @description Create a new Wolf agent.
    #' @param id Integer identifier.
    #' @param x Initial grid column.
    #' @param y Initial grid row.
    #' @param energy Initial energy.
    initialize = function(id, x, y, energy) {
      self$id     <- id
      self$x      <- x
      self$y      <- y
      self$energy <- energy
    },

    #' @description Execute one tick of wolf behaviour.
    #' @param model The `PredatorPreyModel` (or subclass) instance.
    step = function(model) {
      if (!self$alive) return(invisible(NULL))

      # 1. Move to a random Moore neighbor (toroidal wrap)
      dx <- sample(-1L:1L, 1L)
      dy <- sample(-1L:1L, 1L)
      self$x <- ((self$x - 1L + dx) %% model$width)  + 1L
      self$y <- ((self$y - 1L + dy) %% model$height) + 1L

      # 2. Lose energy each tick
      self$energy <- self$energy - 1

      # 3. If any sheep at current cell: eat one.
      #    Use the cell lookup (built at tick start) to find candidates, then
      #    verify each is still alive AND still at this cell — sheep that moved
      #    earlier this tick will have a different position, so they escape.
      key       <- paste0(self$x, ",", self$y)
      prey_here <- model$sheep_cell[[key]]
      if (length(prey_here) > 0L) {
        prey_here <- prey_here[
          vapply(prey_here, function(idx) {
            s <- model$sheep[[idx]]
            s$alive && s$x == self$x && s$y == self$y
          }, logical(1L))
        ]
      }
      if (length(prey_here) > 0L) {
        chosen <- prey_here[sample.int(length(prey_here), 1L)]
        model$sheep[[chosen]]$alive <- FALSE
        self$energy <- self$energy + model$params$wolf_gain_from_food
      }

      # 4. Reproduce probabilistically
      if (self$energy > model$params$wolf_reproduce_threshold &&
          stats::runif(1) < model$params$wolf_reproduce) {
        self$energy  <- self$energy / 2
        offspring_id <- model$.next_id()
        offspring    <- Wolf$new(offspring_id, self$x, self$y, self$energy)
        model$wolves <- c(model$wolves, list(offspring))
      }

      # 5. Die if out of energy
      if (self$energy <= 0) self$alive <- FALSE

      invisible(NULL)
    }
  )
)

#' Predator-prey ABM
#'
#' @description Baseline wolf-sheep model on a toroidal grid. Implemented
#'   with R6 classes following standard ABM conventions: randomised agent
#'   stepping order, Moore-neighbourhood movement, and probabilistic
#'   reproduction and death.
#'
#' @export
PredatorPreyModel <- R6::R6Class("PredatorPreyModel",
  public = list(
    #' @field width Grid width (number of columns).
    width      = NULL,
    #' @field height Grid height (number of rows).
    height     = NULL,
    #' @field params Named list of model parameters.
    params     = NULL,
    #' @field sheep List of `Sheep` R6 objects.
    sheep      = NULL,
    #' @field wolves List of `Wolf` R6 objects.
    wolves     = NULL,
    #' @field step_count Current tick count.
    step_count = 0L,
    #' @field history Tibble of recorded population counts built by `run()`.
    history    = NULL,
    #' @field sheep_cell Named list mapping `"x,y"` keys to integer vectors of
    #'   sheep indices. Built at the start of each tick so `Wolf$step()` can
    #'   do O(1) cell lookups instead of scanning all sheep (O(n_sheep)).
    sheep_cell = NULL,

    #' @description Initialise the predator-prey model.
    #' @param width Grid width. Default `50L`.
    #' @param height Grid height. Default `50L`.
    #' @param n_sheep Initial number of sheep. Default `100L`.
    #' @param n_wolves Initial number of wolves. Default `25L`.
    #' @param sheep_reproduce Per-tick reproduction probability for sheep
    #'   (when above threshold). Default `0.04`.
    #' @param wolf_reproduce Per-tick reproduction probability for wolves
    #'   (when above threshold). Default `0.05`.
    #' @param wolf_gain_from_food Energy gained by wolf when eating a sheep.
    #'   Default `20.0`.
    #' @param sheep_gain_from_food Energy gained by sheep each tick (grass
    #'   baseline). Default `4.0`.
    #' @param sheep_reproduce_threshold Energy threshold above which a sheep
    #'   may reproduce. Default `10.0`.
    #' @param wolf_reproduce_threshold Energy threshold above which a wolf
    #'   may reproduce. Default `20.0`.
    #' @param seed Optional integer random seed for reproducibility.
    initialize = function(
      width                     = 50L,
      height                    = 50L,
      n_sheep                   = 100L,
      n_wolves                  = 25L,
      sheep_reproduce           = 0.04,
      wolf_reproduce            = 0.05,
      wolf_gain_from_food       = 20.0,
      sheep_gain_from_food      = 4.0,
      sheep_reproduce_threshold = 10.0,
      wolf_reproduce_threshold  = 20.0,
      seed                      = NULL
    ) {
      if (!is.null(seed)) set.seed(seed)

      self$width  <- as.integer(width)
      self$height <- as.integer(height)
      self$params <- list(
        sheep_reproduce           = sheep_reproduce,
        wolf_reproduce            = wolf_reproduce,
        wolf_gain_from_food       = wolf_gain_from_food,
        sheep_gain_from_food      = sheep_gain_from_food,
        sheep_reproduce_threshold = sheep_reproduce_threshold,
        wolf_reproduce_threshold  = wolf_reproduce_threshold
      )

      private$.id_counter <- 0L

      # Initialise sheep
      self$sheep <- vector("list", n_sheep)
      for (i in seq_len(n_sheep)) {
        self$sheep[[i]] <- Sheep$new(
          id     = private$.next_id_impl(),
          x      = sample.int(self$width,  1L),
          y      = sample.int(self$height, 1L),
          energy = stats::runif(1, 1, sheep_reproduce_threshold * 2)
        )
      }

      # Initialise wolves
      self$wolves <- vector("list", n_wolves)
      for (i in seq_len(n_wolves)) {
        self$wolves[[i]] <- Wolf$new(
          id     = private$.next_id_impl(),
          x      = sample.int(self$width,  1L),
          y      = sample.int(self$height, 1L),
          energy = stats::runif(1, 1, wolf_reproduce_threshold * 2)
        )
      }
    },

    #' @description Advance the model by one tick.
    #'
    #' Steps all agents in random order, then removes dead agents.
    step = function() {
      # Build O(1) cell lookup for wolf hunting before any agent moves
      private$.build_sheep_cell()

      # Combine agents and shuffle for fairness
      all_agents <- c(self$sheep, self$wolves)
      idx        <- sample.int(length(all_agents))
      for (i in idx) {
        all_agents[[i]]$step(self)
      }

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
    #'       `value` (`variable` in `c("sheep", "wolves")`)
    #'   - `extinct`: logical, `TRUE` if either population hit zero
    #'   - `params`: the params list used
    run = function(steps = 500L) {
      records <- vector("list", steps + 1L)
      records[[1L]] <- data.frame(
        step     = 0L,
        variable = c("sheep", "wolves"),
        value    = c(length(self$sheep), length(self$wolves))
      )

      for (s in seq_len(steps)) {
        self$step()
        records[[s + 1L]] <- data.frame(
          step     = s,
          variable = c("sheep", "wolves"),
          value    = c(length(self$sheep), length(self$wolves))
        )

        if (length(self$sheep) == 0L || length(self$wolves) == 0L) {
          # Pad remaining records with NAs for consistent tibble shape
          for (r in seq(s + 2L, steps + 1L)) {
            records[[r]] <- data.frame(
              step     = r - 1L,
              variable = c("sheep", "wolves"),
              value    = c(length(self$sheep), length(self$wolves))
            )
          }
          break
        }
      }

      results <- dplyr::as_tibble(do.call(rbind, records))
      self$history <- results

      list(
        results = results,
        extinct = any(results$value[results$step == max(results$step)] == 0L),
        params  = self$params
      )
    },

    #' @description Internal: get and increment the agent ID counter.
    #' @return Integer next ID.
    .next_id = function() {
      private$.next_id_impl()
    }
  ),

  private = list(
    .id_counter = 0L,

    .next_id_impl = function() {
      private$.id_counter <- private$.id_counter + 1L
      private$.id_counter
    },

    # Build cell → sheep-index lookup: called once per tick before agent stepping.
    # Stores result in self$sheep_cell (public) so Wolf$step() can access it.
    .build_sheep_cell = function() {
      cell <- list()
      for (i in seq_along(self$sheep)) {
        s   <- self$sheep[[i]]
        key <- paste0(s$x, ",", s$y)
        cell[[key]] <- c(cell[[key]], i)
      }
      self$sheep_cell <- cell
    }
  )
)
