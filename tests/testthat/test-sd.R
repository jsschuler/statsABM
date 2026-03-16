test_that("run_lv returns correct structure", {
  out <- run_lv()
  expect_named(out, c("results", "equilibrium", "params"))
  expect_true(tibble::is_tibble(out$results))
  expect_true(all(c("time", "variable", "value") %in% names(out$results)))
  expect_setequal(unique(out$results$variable), c("prey", "predators"))
})

test_that("run_lv: prey and predators stay positive", {
  out <- run_lv(
    params   = LV_DEFAULTS,
    y0       = c(prey = 40, predators = 9),
    t_span   = c(0, 200),
    n_points = 500L
  )
  prey_vals      <- out$results$value[out$results$variable == "prey"]
  predator_vals  <- out$results$value[out$results$variable == "predators"]
  expect_true(all(prey_vals > 0),      info = "prey should stay positive")
  expect_true(all(predator_vals > 0),  info = "predators should stay positive")
})

test_that("lv_equilibrium formula is correct", {
  p   <- LV_DEFAULTS
  eq  <- lv_equilibrium(p)
  expect_named(eq, c("prey", "predators"))
  expect_equal(eq["prey"],      c(prey = p$gamma / p$delta),     tolerance = 1e-10)
  expect_equal(eq["predators"], c(predators = p$alpha / p$beta), tolerance = 1e-10)
})

test_that("increasing alpha raises predator equilibrium", {
  p_low  <- LV_DEFAULTS
  p_high <- modifyList(LV_DEFAULTS, list(alpha = LV_DEFAULTS$alpha * 2))

  eq_low  <- lv_equilibrium(p_low)
  eq_high <- lv_equilibrium(p_high)

  expect_gt(eq_high["predators"], eq_low["predators"])
})

test_that("run_isr returns correct structure", {
  out <- run_isr()
  expect_named(out, c("results", "R0", "peak_I", "peak_time", "final_R", "params"))
  expect_true(tibble::is_tibble(out$results))
  expect_true(all(c("time", "variable", "value") %in% names(out$results)))
  expect_setequal(unique(out$results$variable), c("I", "S", "R"))
})

test_that("run_isr: I + S + R = 1 at every time point", {
  out <- run_isr(n_points = 200L)
  wide <- tidyr::pivot_wider(out$results, names_from = "variable", values_from = "value")
  totals <- wide$I + wide$S + wide$R
  expect_true(all(abs(totals - 1) < 1e-6),
              info = "I + S + R must equal 1 everywhere")
})

test_that("run_isr: R is monotone non-decreasing", {
  out <- run_isr(n_points = 200L)
  R_vals <- out$results$value[out$results$variable == "R"]
  diffs  <- diff(R_vals)
  expect_true(all(diffs >= -1e-9),
              info = "R should be non-decreasing")
})

test_that("run_isr: R0 matches beta / gamma", {
  out <- run_isr()
  expect_equal(out$R0, ISR_DEFAULTS$beta / ISR_DEFAULTS$gamma, tolerance = 1e-10)
})

test_that("run_isr: peak_I is correctly identified", {
  out <- run_isr()
  I_vals   <- out$results$value[out$results$variable == "I"]
  expect_equal(out$peak_I, max(I_vals), tolerance = 1e-8)
})
