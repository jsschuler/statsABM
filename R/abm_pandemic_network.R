#' Network pandemic ABM
#'
#' @description ISR epidemic model on a Barabasi-Albert preferential
#'   attachment graph (via `igraph`). The power-law degree distribution
#'   produces natural super-spreader structure, making the well-mixed
#'   assumption of the ODE model explicit and removable. Super-spreaders
#'   are agents in the top decile of degree.
#'
#' @export
NetworkPandemicModel <- R6::R6Class("NetworkPandemicModel",
  public = list(
    #' @field n_agents Number of agents (graph nodes).
    n_agents   = NULL,
    #' @field params Named list of model parameters (`beta`, `gamma`).
    params     = NULL,
    #' @field graph An `igraph` graph object (undirected BA network).
    graph      = NULL,
    #' @field agents Data frame with columns `id`, `state`, `degree`,
    #'   `is_super_spreader`.
    agents     = NULL,
    #' @field step_count Current tick count.
    step_count = 0L,

    #' @description Initialise the network pandemic model.
    #' @param n_agents Number of agents. Default `500L`.
    #' @param n_edges_per_new_node Number of edges added per new node when
    #'   building the BA graph (`m` in `igraph::sample_pa()`). Default `3L`.
    #' @param initial_infected_fraction Fraction of agents initially infected.
    #'   Default `0.01`.
    #' @param beta Transmission probability per infected neighbour per tick.
    #'   Default `0.3`.
    #' @param gamma Recovery probability per infected agent per tick.
    #'   Default `0.05`.
    #' @param seed_hubs Logical; if `TRUE` the initially infected agents are
    #'   drawn from the highest-degree nodes rather than uniformly at random.
    #'   Used in Activity 4 to demonstrate how seeding location affects
    #'   epidemic dynamics. Default `FALSE`.
    #' @param seed Optional integer random seed for reproducibility.
    initialize = function(
      n_agents                  = 500L,
      n_edges_per_new_node      = 3L,
      initial_infected_fraction = 0.01,
      beta                      = 0.3,
      gamma                     = 0.05,
      seed_hubs                 = FALSE,
      seed                      = NULL
    ) {
      if (!is.null(seed)) set.seed(seed)

      self$n_agents <- as.integer(n_agents)
      self$params   <- list(beta = beta, gamma = gamma,
                            n_edges_per_new_node = as.integer(n_edges_per_new_node),
                            seed_hubs = seed_hubs)

      # Build Barabasi-Albert graph
      self$graph <- igraph::sample_pa(
        n        = self$n_agents,
        m        = as.integer(n_edges_per_new_node),
        directed = FALSE
      )

      degrees <- igraph::degree(self$graph)
      cutoff  <- stats::quantile(degrees, 0.9)

      # Initialise agent data frame
      n_infected <- max(1L, as.integer(round(self$n_agents * initial_infected_fraction)))

      if (seed_hubs) {
        # Infect the top n_infected nodes by degree
        ranked   <- order(degrees, decreasing = TRUE)
        inf_ids  <- ranked[seq_len(n_infected)]
        states   <- rep("S", self$n_agents)
        states[inf_ids] <- "I"
      } else {
        states <- sample(
          c(rep("I", n_infected), rep("S", self$n_agents - n_infected))
        )
      }

      self$agents <- data.frame(
        id               = seq_len(self$n_agents),
        state            = states,
        degree           = as.integer(degrees),
        is_super_spreader = degrees >= cutoff,
        stringsAsFactors = FALSE
      )
    },

    #' @description Advance the model by one tick.
    step = function() {
      # Pre-compute current states to avoid within-step update feedback
      current_states <- self$agents$state

      new_states <- current_states

      # Transmission: infected agents attempt to infect susceptible neighbours
      infected_ids <- which(current_states == "I")
      for (v in infected_ids) {
        nbrs <- igraph::neighbors(self$graph, v)
        for (u in nbrs) {
          if (current_states[u] == "S") {
            if (stats::runif(1) < self$params$beta) {
              new_states[u] <- "I"
            }
          }
        }
      }

      # Recovery
      for (v in infected_ids) {
        if (stats::runif(1) < self$params$gamma) {
          new_states[v] <- "R"
        }
      }

      self$agents$state <- new_states
      self$step_count   <- self$step_count + 1L
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
    #'   - `super_spreader_attack_rate`: fraction of super-spreaders ever infected
    #'   - `non_super_spreader_attack_rate`: fraction of non-super-spreaders ever infected
    #'   - `degree_distribution`: integer vector of node degrees
    #'   - `params`: the params list used
    run = function(steps = 200L) {
      n <- self$n_agents

      .fracs <- function() {
        s <- self$agents$state
        c(I = sum(s == "I") / n, S = sum(s == "S") / n, R = sum(s == "R") / n)
      }

      records <- vector("list", steps + 1L)
      init_f  <- .fracs()
      records[[1L]] <- data.frame(
        step     = 0L,
        variable = c("I", "S", "R"),
        value    = unname(init_f)
      )

      # Track who was ever infected (started as I or became I during run)
      ever_infected <- self$agents$state == "I"

      for (s in seq_len(steps)) {
        self$step()
        f_now <- .fracs()
        records[[s + 1L]] <- data.frame(
          step     = s,
          variable = c("I", "S", "R"),
          value    = unname(f_now)
        )
        ever_infected <- ever_infected | (self$agents$state != "S")
      }

      results <- dplyr::as_tibble(do.call(rbind, records))

      I_series <- results$value[results$variable == "I"]
      peak_idx <- which.max(I_series)

      # Empirical R0 from early growth
      early_end <- max(2L, as.integer(steps * 0.1))
      early_I   <- I_series[seq_len(early_end + 1L)]
      early_I   <- early_I[early_I > 0]
      R0_emp    <- NA_real_
      if (length(early_I) >= 3L) {
        fit    <- stats::lm(log(early_I) ~ seq_along(early_I))
        growth <- unname(stats::coef(fit)[2L])
        R0_emp <- growth / self$params$gamma + 1
      }

      # Attack rates by super-spreader status
      ss_mask  <- self$agents$is_super_spreader
      ss_ar    <- if (sum(ss_mask)  > 0) mean(ever_infected[ss_mask])  else NA_real_
      nss_ar   <- if (sum(!ss_mask) > 0) mean(ever_infected[!ss_mask]) else NA_real_

      list(
        results                     = results,
        R0_empirical                = R0_emp,
        peak_I                      = I_series[peak_idx],
        peak_step                   = peak_idx - 1L,
        super_spreader_attack_rate  = ss_ar,
        non_super_spreader_attack_rate = nss_ar,
        degree_distribution         = igraph::degree(self$graph),
        params                      = self$params
      )
    }
  )
)
