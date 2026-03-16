#' Default ISR pandemic parameters
#'
#' Note: compartment ordering is **ISR** (Infected, Susceptible, Recovered),
#' not the conventional SIR.
#'
#' - `beta`: transmission rate (per infected-susceptible contact per time unit)
#' - `gamma`: recovery rate (per infected individual per time unit)
#'
#' These defaults yield R0 = beta / gamma = 6, which is measles-like and
#' produces a dramatic, clearly peaked epidemic curve suitable for teaching.
#'
#' @export
ISR_DEFAULTS <- list(
  beta  = 0.3,    # transmission rate
  gamma = 0.05    # recovery rate
  # R0 = beta / gamma = 6 (measles-like; produces a dramatic curve)
)

# Internal ODE right-hand side for deSolve (ISR ordering)
.isr_ode <- function(t, y, parms) {
  with(as.list(c(y, parms)), {
    N       <- I + S + R          # should be 1 throughout (fractions)
    new_inf <- beta * I * S       # mass-action transmission
    recovery <- gamma * I

    dI <- new_inf - recovery
    dS <- -new_inf
    dR <- recovery
    list(c(dI, dS, dR))
  })
}

#' Run the ISR compartmental ODE model
#'
#' Integrates the ISR (Infected-Susceptible-Recovered) epidemic ODE using
#' `deSolve::ode()`. Compartment ordering is ISR, not SIR.
#'
#' @param params Named list. Defaults to `ISR_DEFAULTS`.
#'   Elements: `beta` (transmission rate), `gamma` (recovery rate).
#' @param y0 Named numeric vector of initial fractions.
#'   Default `c(I = 0.01, S = 0.98, R = 0.01)`. Must sum to 1.
#' @param t_span Numeric vector `c(start, end)`. Default `c(0, 160)`.
#' @param n_points Integer number of output time points. Default `1600`.
#' @return Named list:
#'   - `results`: long-format tibble with columns `time`, `variable`, `value`
#'       (`variable` in `c("I", "S", "R")`)
#'   - `R0`: basic reproduction number (`beta / gamma`)
#'   - `peak_I`: peak infected fraction
#'   - `peak_time`: time of peak infected fraction
#'   - `final_R`: fraction recovered at end of simulation
#'   - `params`: the params list used
#' @export
#' @examples
#' out <- run_isr()
#' out$R0
#' out$peak_I
run_isr <- function(params   = ISR_DEFAULTS,
                    y0       = c(I = 0.01, S = 0.98, R = 0.01),
                    t_span   = c(0, 160),
                    n_points = 1600L) {
  times <- seq(t_span[1], t_span[2], length.out = n_points)

  raw <- deSolve::ode(
    y      = y0,
    times  = times,
    func   = .isr_ode,
    parms  = params,
    method = "lsoda"
  )

  df      <- as.data.frame(raw)
  results <- tidyr::pivot_longer(
    df,
    cols      = -time,
    names_to  = "variable",
    values_to = "value"
  )
  results <- dplyr::as_tibble(results)

  # Summary statistics
  I_vals   <- df[["I"]]
  peak_idx <- which.max(I_vals)

  list(
    results    = results,
    R0         = params$beta / params$gamma,
    peak_I     = I_vals[peak_idx],
    peak_time  = df[["time"]][peak_idx],
    final_R    = df[["R"]][nrow(df)],
    params     = params
  )
}

#' Herd immunity threshold
#'
#' The fraction of the population that must be immune to prevent epidemic
#' spread, derived from the condition R_effective < 1.
#'
#' @param params Named list with elements `beta` and `gamma`.
#' @return Scalar `1 - 1 / R0`.
#' @export
#' @examples
#' herd_immunity_threshold(ISR_DEFAULTS)
herd_immunity_threshold <- function(params) {
  R0 <- params$beta / params$gamma
  1 - 1 / R0
}
