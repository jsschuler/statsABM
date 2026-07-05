#!/usr/bin/env Rscript
# render_parallel_runs.R
#
# Runs N_RUNS independent replicates of two Wolf-Sheep variants:
#
#   No Grass — sheep reproduce freely; wolves must eat to survive.
#              Typical outcome: wolf overshoot → extinction.
#
#   Grass    — sheep must eat grass to maintain energy; grass regrows slowly.
#              The third trophic level buffers the boom-bust cycle and
#              sustains oscillations that the no-grass model rarely achieves.
#
# Output: parallel_runs.png — two-panel spaghetti plot for the slide deck.

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
})

# ---- Parameters (NetLogo defaults throughout) --------------------------------
G                    <- 51L
TMAX                 <- 300L
N_RUNS               <- 50L

initial_sheep        <- 100L
initial_wolves       <- 50L
sheep_reproduce      <- 4.0    # % chance per tick
wolf_reproduce       <- 5.0    # % chance per tick
wolf_gain_food       <- 20L

# Grass-only parameters
sheep_gain_from_food <- 4L
grass_regrowth_time  <- 30L

# ---- Helpers -----------------------------------------------------------------
wrap <- function(v, g) ((v - 1L) %% g) + 1L

random_walk <- function(x, y, g) {
  n  <- length(x)
  dx <- sample(-1L:1L, n, replace = TRUE)
  dy <- sample(-1L:1L, n, replace = TRUE)
  list(x = wrap(x + dx, g), y = wrap(y + dy, g))
}

# Map (x, y) grid coordinates to a flat vector index
cell_idx <- function(x, y, g) (x - 1L) * g + y

# Wolves eat one random sheep on the same cell; each sheep eaten at most once
eat_sheep <- function(agents, wolf_gain) {
  wolf_df  <- agents[agents$type == "wolf",  c("id","x","y"), drop = FALSE]
  sheep_df <- agents[agents$type == "sheep", c("id","x","y"), drop = FALSE]
  if (nrow(wolf_df) == 0L || nrow(sheep_df) == 0L) return(agents)

  eats <- wolf_df |>
    left_join(sheep_df, by = c("x","y"), suffix = c("_w","_s"),
              relationship = "many-to-many") |>
    rename(wolf_id = id_w, sheep_id = id_s) |>
    filter(!is.na(sheep_id)) |>
    group_by(wolf_id)  |> slice_sample(n = 1L) |> ungroup() |>
    group_by(sheep_id) |> slice_sample(n = 1L) |> ungroup()

  if (nrow(eats) > 0L) {
    idx <- match(eats$wolf_id, agents$id)
    agents$energy[idx] <- agents$energy[idx] + wolf_gain
    agents <- agents[!agents$id %in% eats$sheep_id, ]
  }
  agents
}

# Reproduction: parent halves energy; offspring placed in adjacent cell
reproduce <- function(agents, next_id, g) {
  p_rep    <- ifelse(agents$type == "sheep", sheep_reproduce, wolf_reproduce) / 100
  rep_mask <- runif(nrow(agents)) < p_rep
  if (!any(rep_mask)) return(list(agents = agents, next_id = next_id))

  parents <- agents[rep_mask, ]
  agents$energy[rep_mask] <- agents$energy[rep_mask] %/% 2L
  n_new   <- nrow(parents)
  off_pos <- random_walk(parents$x, parents$y, g)
  offspring <- parents |>
    mutate(id = seq(next_id, next_id + n_new - 1L),
           x  = off_pos$x,
           y  = off_pos$y)
  list(agents  = bind_rows(agents, offspring),
       next_id = next_id + n_new)
}

# ---- No-grass simulation -----------------------------------------------------
# Sheep have no energy cost to live; they reproduce freely.
# Death comes only from wolf predation.

run_no_grass <- function(seed) {
  set.seed(seed)

  agents <- bind_rows(
    tibble(
      id     = seq_len(initial_sheep),
      type   = "sheep",
      x      = sample(G, initial_sheep, replace = TRUE),
      y      = sample(G, initial_sheep, replace = TRUE),
      energy = 1L + sample(50L, initial_sheep, replace = TRUE)
    ),
    tibble(
      id     = seq(initial_sheep + 1L, initial_sheep + initial_wolves),
      type   = "wolf",
      x      = sample(G, initial_wolves, replace = TRUE),
      y      = sample(G, initial_wolves, replace = TRUE),
      energy = sample(2L * wolf_gain_food, initial_wolves, replace = TRUE)
    )
  )
  next_id <- initial_sheep + initial_wolves + 1L
  counts  <- tibble(t = 0L, n_sheep = initial_sheep, n_wolf = initial_wolves)

  for (tt in seq_len(TMAX)) {
    if (sum(agents$type == "sheep") == 0L ||
        sum(agents$type == "wolf")  == 0L) break

    pos <- random_walk(agents$x, agents$y, G)
    agents$x <- pos$x; agents$y <- pos$y

    agents$energy[agents$type == "wolf"] <-
      agents$energy[agents$type == "wolf"] - 1L

    agents <- eat_sheep(agents, wolf_gain_food)
    agents <- agents[agents$energy >= 0L, ]

    r <- reproduce(agents, next_id, G)
    agents  <- r$agents
    next_id <- r$next_id

    counts <- add_row(counts, t = tt,
                      n_sheep = sum(agents$type == "sheep"),
                      n_wolf  = sum(agents$type == "wolf"))
  }

  counts |> mutate(run = seed, model = "No Grass")
}

# ---- Grass simulation --------------------------------------------------------
# Sheep lose 1 energy per tick; gain sheep_gain_from_food when on a green patch.
# Patches regrow from brown to green after grass_regrowth_time ticks.
# The resource cycle gives sheep a recovery mechanism that stabilizes dynamics.

run_grass <- function(seed) {
  set.seed(seed)

  n_cells <- G * G
  # NetLogo init: patches randomly green or brown; brown patches start with a
  # random countdown so regrowth is staggered rather than synchronised.
  patch_countdown <- integer(n_cells)
  brown_init      <- sample(c(TRUE, FALSE), n_cells, replace = TRUE)
  patch_countdown[brown_init] <-
    sample(grass_regrowth_time, sum(brown_init), replace = TRUE)

  agents <- bind_rows(
    tibble(
      id     = seq_len(initial_sheep),
      type   = "sheep",
      x      = sample(G, initial_sheep, replace = TRUE),
      y      = sample(G, initial_sheep, replace = TRUE),
      energy = sample(2L * sheep_gain_from_food, initial_sheep, replace = TRUE)
    ),
    tibble(
      id     = seq(initial_sheep + 1L, initial_sheep + initial_wolves),
      type   = "wolf",
      x      = sample(G, initial_wolves, replace = TRUE),
      y      = sample(G, initial_wolves, replace = TRUE),
      energy = sample(2L * wolf_gain_food, initial_wolves, replace = TRUE)
    )
  )
  next_id <- initial_sheep + initial_wolves + 1L
  counts  <- tibble(t = 0L, n_sheep = initial_sheep, n_wolf = initial_wolves)

  for (tt in seq_len(TMAX)) {
    if (sum(agents$type == "sheep") == 0L &&
        sum(agents$type == "wolf")  == 0L) break

    pos <- random_walk(agents$x, agents$y, G)
    agents$x <- pos$x; agents$y <- pos$y

    # All agents lose 1 energy per tick (NetLogo: both sheep and wolves)
    agents$energy <- agents$energy - 1L

    # Sheep eat grass: one sheep per green cell gains energy, cell turns brown
    sheep_idx <- which(agents$type == "sheep")
    if (length(sheep_idx) > 0L) {
      s_cells  <- cell_idx(agents$x[sheep_idx], agents$y[sheep_idx], G)
      is_green <- patch_countdown[s_cells] == 0L
      if (any(is_green)) {
        eat_df <- tibble(row = sheep_idx[is_green], cell = s_cells[is_green]) |>
          group_by(cell) |> slice_sample(n = 1L) |> ungroup()
        agents$energy[eat_df$row] <-
          agents$energy[eat_df$row] + sheep_gain_from_food
        patch_countdown[eat_df$cell] <- grass_regrowth_time
      }
    }

    agents <- eat_sheep(agents, wolf_gain_food)
    agents <- agents[agents$energy >= 0L, ]

    if (nrow(agents) > 0L) {
      r <- reproduce(agents, next_id, G)
      agents  <- r$agents
      next_id <- r$next_id
    }

    # Grass regrowth: decrement countdown on all brown patches
    brown_mask <- patch_countdown > 0L
    patch_countdown[brown_mask] <- patch_countdown[brown_mask] - 1L

    counts <- add_row(counts, t = tt,
                      n_sheep = sum(agents$type == "sheep"),
                      n_wolf  = sum(agents$type == "wolf"))
  }

  counts |> mutate(run = seed, model = "Grass")
}

# ---- Run all replicates ------------------------------------------------------
cat(sprintf("Running %d no-grass replicates...\n", N_RUNS))
no_grass_runs <- map_dfr(seq_len(N_RUNS), \(s) {
  if (s %% 10 == 0) cat(sprintf("  run %d/%d\n", s, N_RUNS))
  run_no_grass(s)
})

cat(sprintf("Running %d grass replicates...\n", N_RUNS))
grass_runs <- map_dfr(seq_len(N_RUNS), \(s) {
  if (s %% 10 == 0) cat(sprintf("  run %d/%d\n", s, N_RUNS))
  run_grass(s)
})

all_runs <- bind_rows(no_grass_runs, grass_runs) |>
  mutate(model = factor(model, levels = c("No Grass", "Grass")))

# ---- Summarise outcomes -------------------------------------------------------
outcomes <- all_runs |>
  group_by(model, run) |>
  summarise(
    sheep_extinct = last(n_sheep) == 0L,
    wolf_extinct  = last(n_wolf)  == 0L,
    max_t         = max(t),
    .groups = "drop"
  ) |>
  mutate(outcome = case_when(
    sheep_extinct & wolf_extinct  ~ "both extinct",
    sheep_extinct                 ~ "sheep extinct (wolf overshoot)",
    wolf_extinct                  ~ "wolf extinct (prey collapse)",
    TRUE                          ~ "both survive to t_max"
  ))

cat("\nOutcome summary:\n")
print(outcomes |> count(model, outcome))

# ---- Plot --------------------------------------------------------------------
cat("\nBuilding spaghetti plot...\n")

plot_data <- all_runs |>
  pivot_longer(c(n_sheep, n_wolf), names_to = "species", values_to = "count") |>
  mutate(species = recode(species, n_sheep = "Sheep", n_wolf = "Wolf"))

p_nograss <- plot_data |>
  filter(model == "No Grass") |>
  ggplot(aes(x = t, y = count,
             group = interaction(run, species),
             color = species)) +
  geom_line(alpha = 0.20, linewidth = 0.35) +
  scale_color_manual(values = c(Sheep = "#2D5F8A", Wolf = "#C0392B"),
                     name = NULL) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "No Grass",
       x = "Tick", y = "Population") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "plain",
                                   margin = margin(b = 6)),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAF7", color = NA),
    legend.position = "bottom"
  )

p_grass <- plot_data |>
  filter(model == "Grass") |>
  ggplot(aes(x = t, y = count,
             group = interaction(run, species),
             color = species)) +
  geom_line(alpha = 0.20, linewidth = 0.35) +
  scale_color_manual(values = c(Sheep = "#2D5F8A", Wolf = "#C0392B"),
                     name = NULL) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "Grass",
       x = "Tick", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "plain",
                                   margin = margin(b = 6)),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "#FAFAF7", color = NA),
    legend.position = "bottom"
  )

combined <- (p_nograss | p_grass) +
  plot_annotation(
    title   = paste0("Wolf-Sheep Predation \u2014 ", N_RUNS,
                     " Independent Runs, ", TMAX, " Ticks"),
    caption = paste0(
      "Sheep \u2022 blue    Wolf \u2022 red    ",
      "Without grass, cycling is structurally sparse: most runs end in extinction. ",
      "Grass provides a resource buffer that sustains oscillations."
    )
  )

ggsave("parallel_runs.png", combined,
       width = 10, height = 4.5, dpi = 150, bg = "#FAFAF7")

cat("Done: parallel_runs.png\n")
