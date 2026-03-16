test_that("PredatorPreyModel runs 50 steps on 20x20 grid without error", {
  m   <- PredatorPreyModel$new(width = 20L, height = 20L,
                               n_sheep = 40L, n_wolves = 10L, seed = 1L)
  out <- m$run(steps = 50L)
  expect_named(out, c("results", "extinct", "params"))
})

test_that("PredatorPreyModel results tibble has correct columns", {
  m   <- PredatorPreyModel$new(width = 20L, height = 20L,
                               n_sheep = 40L, n_wolves = 10L, seed = 1L)
  out <- m$run(steps = 10L)
  expect_true(tibble::is_tibble(out$results))
  expect_true(all(c("step", "variable", "value") %in% names(out$results)))
})

test_that("PredatorPreyModel population counts are non-negative integers", {
  m   <- PredatorPreyModel$new(width = 20L, height = 20L,
                               n_sheep = 40L, n_wolves = 10L, seed = 1L)
  out <- m$run(steps = 50L)
  counts <- out$results$value[out$results$variable %in% c("sheep", "wolves")]
  expect_true(all(counts >= 0),         info = "counts must be non-negative")
  expect_true(all(counts == round(counts)), info = "counts must be integers")
})

test_that("PredatorPreyModel is reproducible with seed = 42", {
  m1  <- PredatorPreyModel$new(width = 20L, height = 20L,
                               n_sheep = 40L, n_wolves = 10L, seed = 42L)
  out1 <- m1$run(steps = 30L)

  m2  <- PredatorPreyModel$new(width = 20L, height = 20L,
                               n_sheep = 40L, n_wolves = 10L, seed = 42L)
  out2 <- m2$run(steps = 30L)

  expect_equal(out1$results, out2$results)
})

test_that("PredatorPreyGrassModel results contain grass_fraction column", {
  m   <- PredatorPreyGrassModel$new(
    width = 20L, height = 20L,
    n_sheep = 40L, n_wolves = 10L,
    grass_regrowth_time = 5L,
    initial_grass_fraction = 0.5,
    seed = 1L
  )
  out <- m$run(steps = 50L)
  expect_true("grass_fraction" %in% unique(out$results$variable))
})

test_that("PredatorPreyGrassModel grass_fraction values are in [0, 1]", {
  m   <- PredatorPreyGrassModel$new(
    width = 20L, height = 20L,
    n_sheep = 40L, n_wolves = 10L,
    grass_regrowth_time = 5L,
    initial_grass_fraction = 0.5,
    seed = 1L
  )
  out <- m$run(steps = 50L)
  gf  <- out$results$value[out$results$variable == "grass_fraction"]
  expect_true(all(gf >= 0 & gf <= 1),
              info = "grass_fraction must be in [0, 1]")
})

# --- Behavioural tests -------------------------------------------------------

test_that("Wolves die when energy runs out with no sheep to eat", {
  m <- PredatorPreyModel$new(
    width = 10L, height = 10L,
    n_sheep = 0L, n_wolves = 15L,
    wolf_reproduce           = 0.0,
    wolf_reproduce_threshold = 1e6,   # never reproduce
    seed = 1L
  )
  # Force low energy: wolves lose 1/tick, die at energy <= 0, so 3 ticks to die
  for (w in m$wolves) w$energy <- 3.0
  # Call step() directly to avoid run()'s early-exit when sheep == 0
  for (i in seq_len(5L)) m$step()
  expect_equal(length(m$wolves), 0L)
})

test_that("Sheep count increases without wolves (baseline: unlimited grass)", {
  # In the baseline model, sheep gain sheep_gain_from_food every tick and
  # only die from wolf predation.  With no wolves and a high reproduce rate
  # the population must grow.
  m <- PredatorPreyModel$new(
    width = 20L, height = 20L,
    n_sheep  = 30L, n_wolves = 0L,
    sheep_reproduce           = 0.5,
    sheep_reproduce_threshold = 1.0,
    seed = 1L
  )
  out <- m$run(steps = 20L)

  sheep_start <- out$results$value[out$results$step == 0L &
                                     out$results$variable == "sheep"]
  sheep_end   <- out$results$value[out$results$step == max(out$results$step) &
                                     out$results$variable == "sheep"]
  expect_gt(sheep_end, sheep_start)
})

test_that("Grass fraction decreases when sheep graze with slow regrowth", {
  m <- PredatorPreyGrassModel$new(
    width  = 10L, height = 10L,
    n_sheep = 60L, n_wolves = 0L,
    grass_regrowth_time    = 50L,  # slow regrowth
    initial_grass_fraction = 1.0,  # start fully covered
    sheep_reproduce        = 0.0,  # no reproduction — hold N constant
    seed = 1L
  )
  out <- m$run(steps = 10L)

  gf <- out$results$value[out$results$variable == "grass_fraction"]
  expect_lt(gf[length(gf)], gf[1])
})
