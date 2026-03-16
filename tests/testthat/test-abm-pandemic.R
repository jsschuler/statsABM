test_that("PandemicModel runs 50 steps without error", {
  m   <- PandemicModel$new(width = 20L, height = 20L,
                           n_agents = 100L, seed = 1L)
  out <- m$run(steps = 50L)
  expect_named(out, c("results", "R0_empirical", "peak_I", "peak_step", "params"))
})

test_that("PandemicModel I + S + R sums to 1 at every step", {
  m   <- PandemicModel$new(width = 20L, height = 20L,
                           n_agents = 100L, seed = 1L)
  out <- m$run(steps = 50L)
  wide <- tidyr::pivot_wider(out$results, names_from = "variable",
                              values_from = "value")
  totals <- wide$I + wide$S + wide$R
  expect_true(all(abs(totals - 1) < 1e-4),
              info = "I + S + R must sum to 1 (within rounding)")
})

test_that("PandemicModel R is monotone non-decreasing", {
  m   <- PandemicModel$new(width = 20L, height = 20L,
                           n_agents = 100L, seed = 1L)
  out <- m$run(steps = 50L)
  R_vals <- out$results$value[out$results$variable == "R"]
  diffs  <- diff(R_vals)
  expect_true(all(diffs >= -1e-9),
              info = "R should be non-decreasing")
})

test_that("NetworkPandemicModel runs without error", {
  m   <- NetworkPandemicModel$new(n_agents = 100L,
                                  n_edges_per_new_node = 3L,
                                  seed = 1L)
  out <- m$run(steps = 50L)
  expect_true("degree_distribution" %in% names(out))
})

test_that("NetworkPandemicModel degree_distribution has correct length", {
  n   <- 100L
  m   <- NetworkPandemicModel$new(n_agents = n,
                                  n_edges_per_new_node = 3L,
                                  seed = 1L)
  out <- m$run(steps = 10L)
  expect_equal(length(out$degree_distribution), n)
})

test_that("NetworkPandemicModel mean degree is approximately 2 * m", {
  n   <- 200L
  m_  <- 3L
  m   <- NetworkPandemicModel$new(n_agents = n,
                                  n_edges_per_new_node = m_,
                                  seed = 1L)
  out <- m$run(steps = 5L)
  mean_deg <- mean(out$degree_distribution)
  # Mean degree ~ 2 * m for large PA graphs; allow 20% tolerance
  expected <- 2 * m_
  expect_true(
    abs(mean_deg - expected) / expected < 0.20,
    info = sprintf("mean degree %.2f, expected ~ %.2f", mean_deg, expected)
  )
})

# --- Behavioural tests -------------------------------------------------------

test_that("No new infections when beta = 0 and gamma = 0", {
  m <- PandemicModel$new(
    width = 10L, height = 10L, n_agents = 100L,
    beta = 0.0, gamma = 0.0,
    initial_infected_fraction = 0.2,
    seed = 1L
  )
  out <- m$run(steps = 10L)

  I_series <- out$results$value[out$results$variable == "I"]
  S_series <- out$results$value[out$results$variable == "S"]
  # With no transmission and no recovery, I and S are frozen
  expect_equal(I_series[1], I_series[length(I_series)], tolerance = 1e-9)
  expect_equal(S_series[1], S_series[length(S_series)], tolerance = 1e-9)
})

test_that("All infected recover in one step when gamma = 1", {
  m <- PandemicModel$new(
    width = 10L, height = 10L, n_agents = 100L,
    beta = 0.0, gamma = 1.0,
    initial_infected_fraction = 0.5,
    seed = 1L
  )
  m$step()

  states <- vapply(m$agents, function(a) a$state, character(1L))
  expect_equal(sum(states == "I"), 0L)
})

test_that("Infection spreads when beta = 1 and gamma = 0", {
  # With beta = 1, every susceptible agent adjacent to an infected one
  # becomes infected.  The I fraction must strictly increase early in the run.
  m <- PandemicModel$new(
    width = 20L, height = 20L, n_agents = 400L,
    beta = 1.0, gamma = 0.0,
    initial_infected_fraction = 0.05,
    seed = 1L
  )
  out <- m$run(steps = 10L)

  I_series <- out$results$value[out$results$variable == "I"]
  expect_gt(I_series[length(I_series)], I_series[1])
})

test_that("Super-spreaders have higher attack rate than non-super-spreaders", {
  # High-degree nodes are exposed to more infected contacts; over a full
  # epidemic their cumulative attack rate should exceed that of low-degree nodes.
  m   <- NetworkPandemicModel$new(
    n_agents             = 300L,
    n_edges_per_new_node = 3L,
    beta = 0.3, gamma = 0.05,
    seed = 42L
  )
  out <- m$run(steps = 100L)

  expect_gte(out$super_spreader_attack_rate, out$non_super_spreader_attack_rate)
})
