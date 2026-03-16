#' Default Lotka-Volterra parameters
#'
#' - `alpha`: prey birth rate (per capita, per time unit)
#' - `beta`: predation rate (prey removed per predator per prey per time)
#' - `gamma`: predator death rate (per capita)
#' - `delta`: predator birth rate per prey eaten
#'
#' Chosen to produce approximately 3-4 oscillation cycles within the
#' default time window of 200 time units.
#'
#' @export
LV_DEFAULTS <- list(
  alpha = 0.1,    # prey birth rate
  beta  = 0.02,   # predation rate (prey removed per predator per prey)
  gamma = 0.3,    # predator death rate
  delta = 0.01    # predator birth rate per prey eaten
)

# Internal ODE right-hand side for deSolve
.lv_ode <- function(t, y, parms) {
  with(as.list(c(y, parms)), {
    d_prey      <-  alpha * prey      - beta  * prey * predators
    d_predators <- -gamma * predators + delta * prey * predators
    list(c(d_prey, d_predators))
  })
}

#' Run the Lotka-Volterra ODE system
#'
#' Integrates the classical Lotka-Volterra predator-prey equations using
#' `deSolve::ode()` with the `lsoda` method.
#'
#' @param params Named list of parameters. Defaults to `LV_DEFAULTS`.
#'   Elements: `alpha` (prey birth rate), `beta` (predation rate),
#'   `gamma` (predator death rate), `delta` (predator birth per prey eaten).
#' @param y0 Named numeric vector of initial conditions.
#'   Default `c(prey = 40, predators = 9)`.
#' @param t_span Numeric vector `c(start, end)`. Default `c(0, 200)`.
#' @param n_points Number of output time points. Default `2000`.
#' @return Named list:
#'   - `results`: long-format tibble with columns `time`, `variable`, `value`
#'       (`variable` is `"prey"` or `"predators"`)
#'   - `equilibrium`: named vector `c(prey = ..., predators = ...)`
#'   - `params`: the params list used
#' @export
#' @examples
#' out <- run_lv()
#' head(out$results)
#' out$equilibrium
run_lv <- function(params   = LV_DEFAULTS,
                   y0       = c(prey = 40, predators = 9),
                   t_span   = c(0, 200),
                   n_points = 2000L) {
  times <- seq(t_span[1], t_span[2], length.out = n_points)

  raw <- deSolve::ode(
    y     = y0,
    times = times,
    func  = .lv_ode,
    parms = params,
    method = "lsoda"
  )

  # Convert to long-format tibble
  df <- as.data.frame(raw)
  results <- tidyr::pivot_longer(
    df,
    cols      = -time,
    names_to  = "variable",
    values_to = "value"
  )
  results <- dplyr::as_tibble(results)

  list(
    results    = results,
    equilibrium = lv_equilibrium(params),
    params      = params
  )
}

#' Compute the non-trivial Lotka-Volterra equilibrium
#'
#' At the interior fixed point, both populations coexist:
#' \eqn{prey^* = \gamma / \delta} and
#' \eqn{predators^* = \alpha / \beta}.
#'
#' @param params Named list with elements `alpha`, `beta`, `gamma`, `delta`.
#' @return Named numeric vector `c(prey = gamma/delta, predators = alpha/beta)`.
#' @export
#' @examples
#' lv_equilibrium(LV_DEFAULTS)
lv_equilibrium <- function(params) {
  c(
    prey       = params$gamma / params$delta,
    predators  = params$alpha / params$beta
  )
}
