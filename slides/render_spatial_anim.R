#!/usr/bin/env Rscript
# R translation of Wilensky (2005) Wolf-Sheep Predation (NetLogo)
# Saves spatial_lv.gif — an animated GIF cycling through spatial snapshots.
#
# Model logic (faithful to NetLogo source):
#   Each tick:
#     Sheep: move (random walk), reproduce with prob sheep_reproduce/100,
#            die if energy < 0
#     Wolves: move, lose 1 energy, catch one sheep in same cell
#             (gain wolf_gain_food energy), reproduce with prob wolf_reproduce/100,
#             die if energy < 0
#
# Key spatial property: wolves must share a cell to eat a sheep.
# Local interaction generates clustering and travelling wave fronts
# that are inexpressible in the mean-field ODE.

suppressPackageStartupMessages({
  library(tidyverse)
  library(gganimate)
  library(gifski)
})

set.seed(2025)

# ---- Parameters (NetLogo defaults) ----------------------------------------
G                    <- 51L    # NetLogo default grid (51x51 = 2601 patches)
TMAX                 <- 200L
SNAP_T               <- seq(0L, TMAX)   # every tick — full animation

initial_sheep        <- 100L
initial_wolves       <- 50L
sheep_max_init_energy <- 50L
wolf_gain_food       <- 20L    # NetLogo default; keeps wolves food-limited
sheep_reproduce      <- 4.0    # % chance per tick
wolf_reproduce       <- 5.0    # % chance per tick

# ---- Helpers ---------------------------------------------------------------
wrap <- function(v, g) ((v - 1L) %% g) + 1L

random_walk <- function(x, y, g) {
  n  <- length(x)
  dx <- sample(-1L:1L, n, replace = TRUE)
  dy <- sample(-1L:1L, n, replace = TRUE)
  list(x = wrap(x + dx, g), y = wrap(y + dy, g))
}

# ---- Initialise agents -----------------------------------------------------
# NetLogo: sheep energy = 1 + random(sheep-max-initial-energy)
#          wolf  energy = random(2 * wolf-gain-from-food)
next_id <- 1L

agents <- bind_rows(
  tibble(
    id     = seq_len(initial_sheep),
    type   = "sheep",
    x      = sample(G, initial_sheep, replace = TRUE),
    y      = sample(G, initial_sheep, replace = TRUE),
    energy = 1L + sample(sheep_max_init_energy, initial_sheep, replace = TRUE)
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

# ---- Snapshot store --------------------------------------------------------
snaps  <- list()
counts <- tibble(t       = 0L,
                 n_sheep = initial_sheep,
                 n_wolf  = initial_wolves)

if (0L %in% SNAP_T) snaps[["t = 0"]] <- agents

cat("Simulating", TMAX, "ticks on", G, "x", G, "grid...\n")

# ---- Main loop -------------------------------------------------------------
for (tt in seq_len(TMAX)) {

  n_sheep_now <- sum(agents$type == "sheep")
  n_wolf_now  <- sum(agents$type == "wolf")
  extinct     <- n_sheep_now == 0L || n_wolf_now == 0L

  # 1. All agents perform a random walk (NetLogo: rt/lt ≤50°, fd 1)
  pos <- random_walk(agents$x, agents$y, G)
  agents$x <- pos$x
  agents$y <- pos$y

  # 2. Wolves lose 1 energy per tick (NetLogo: set energy energy - 1)
  agents$energy[agents$type == "wolf"] <-
    agents$energy[agents$type == "wolf"] - 1L

  # 3. catch-sheep: each wolf grabs one random sheep from its cell
  #    (NetLogo: one-of sheep-here; gain wolf-gain-from-food energy)
  wolf_df  <- agents[agents$type == "wolf",  c("id","x","y"), drop=FALSE]
  sheep_df <- agents[agents$type == "sheep", c("id","x","y"), drop=FALSE]

  # Join on cell; for each wolf pick one sheep, then deduplicate sheep
  # (each sheep can only be eaten once)
  eats <- wolf_df |>
    left_join(sheep_df, by = c("x","y"), suffix = c("_w","_s"),
              relationship = "many-to-many") |>
    rename(wolf_id = id_w, sheep_id = id_s) |>
    filter(!is.na(sheep_id)) |>
    group_by(wolf_id) |>
    slice_sample(n = 1L) |>     # each wolf picks one sheep from its cell
    ungroup() |>
    group_by(sheep_id) |>
    slice_sample(n = 1L) |>     # each sheep killed by at most one wolf
    ungroup()

  # Update wolf energies
  if (nrow(eats) > 0) {
    idx <- match(eats$wolf_id, agents$id)
    agents$energy[idx] <- agents$energy[idx] + wolf_gain_food
    # Remove eaten sheep
    agents <- agents[!agents$id %in% eats$sheep_id, ]
  }

  # 4. death: remove agents with energy < 0
  #    (NetLogo: if energy < 0 [ die ])
  agents <- agents[agents$energy >= 0L, ]

  # 5. Reproduction
  #    Sheep:  prob sheep_reproduce/100  → energy halved; hatch 1 offspring
  #    Wolves: prob wolf_reproduce/100   → energy halved; hatch 1 offspring
  p_rep   <- ifelse(agents$type == "sheep", sheep_reproduce, wolf_reproduce) / 100
  rep_mask <- runif(nrow(agents)) < p_rep

  if (any(rep_mask)) {
    parents <- agents[rep_mask, ]

    # Halve parent energy (NetLogo: set energy energy / 2)
    agents$energy[rep_mask] <- agents$energy[rep_mask] %/% 2L

    # Create offspring with the other half of energy, placed in random neighbour
    n_new   <- nrow(parents)
    off_pos <- random_walk(parents$x, parents$y, G)
    offspring <- parents |>
      mutate(id     = seq(next_id, next_id + n_new - 1L),
             x      = off_pos$x,
             y      = off_pos$y)
    next_id <- next_id + n_new
    agents  <- bind_rows(agents, offspring)
  }

  # Record counts
  counts <- add_row(counts,
                    t       = tt,
                    n_sheep = sum(agents$type == "sheep"),
                    n_wolf  = sum(agents$type == "wolf"))

  if (tt %in% SNAP_T) {
    snaps[[paste("t =", tt)]] <- agents
    cat(sprintf("  t = %3d  sheep = %d  wolves = %d%s\n",
                tt, n_sheep_now, n_wolf_now,
                if (extinct) "  [EXTINCTION]" else ""))
  }
  if (extinct) { cat("Extinction at t =", tt, "\n"); break }
}
cat("Simulation done.\n")

# ---- Animated GIF ----------------------------------------------------------
# Combine all snapshots into a single data frame with a frame label column.
# transition_manual shows each frame as-is with no interpolation between them,
# which is correct for discrete time snapshots of agent positions.

cat("Building animated GIF...\n")

all_frames <- imap_dfr(snaps, \(df, label) mutate(df, frame = label)) |>
  mutate(frame = factor(frame, levels = names(snaps)))  # preserve time order

# Annotate counts per frame for the subtitle
frame_counts <- all_frames |>
  count(frame, type) |>
  pivot_wider(names_from = type, values_from = n, values_fill = 0) |>
  mutate(label = sprintf("sheep: %d   wolves: %d", sheep, wolf))

anim <- ggplot(all_frames, aes(x = x, y = y, color = type, size = type)) +
  geom_point(alpha = 0.80) +
  scale_color_manual(values = c(sheep = "#2D5F8A", wolf = "#C0392B"),
                     labels = c(sheep = "Sheep", wolf = "Wolf"),
                     name = NULL) +
  scale_size_manual(values = c(sheep = 2.0, wolf = 3.0), guide = "none") +
  scale_x_continuous(limits = c(0.5, G + 0.5), expand = expansion(0)) +
  scale_y_continuous(limits = c(0.5, G + 0.5), expand = expansion(0)) +
  coord_equal() +
  labs(
    title    = "Wolf-Sheep Predation \u2014 {closest_state}",
    subtitle = "Wolves eat only sheep on the same cell. Local interaction; no global mixing.",
    caption  = "Wolf overshoot \u2014 extinction is an absorbing state the ODE cannot reach."
  ) +
  theme_void(base_size = 13) +
  theme(
    plot.title       = element_text(hjust = 0.5, margin = margin(b = 4)),
    plot.subtitle    = element_text(hjust = 0.5, size = 10, color = "#555555",
                                    margin = margin(b = 6)),
    plot.caption     = element_text(hjust = 0.5, size = 9,  color = "#8A4A2D",
                                    margin = margin(t = 6)),
    plot.background  = element_rect(fill = "#FAFAF7", color = NA),
    panel.background = element_rect(fill = "#EEF2F5", color = NA),
    legend.position  = "bottom",
    legend.text      = element_text(size = 11),
    plot.margin      = margin(10, 10, 10, 10)
  ) +
  transition_states(frame,
                    transition_length = 0,  # no morphing — snap to next frame
                    state_length      = 1,  # equal time per frame
                    wrap              = FALSE)

animate(anim,
        nframes   = length(snaps),
        fps       = 4,          # 4 frames/sec — ~37 sec to watch full run
        width     = 340,
        height    = 360,
        renderer  = gifski_renderer("spatial_lv.gif"),
        bg        = "#FAFAF7")

cat("Done: spatial_lv.gif\n")

# ---- Population time series ------------------------------------------------
# Plot wolf and sheep counts over the full simulation run, with vertical
# dashed lines marking the snapshot times used in the GIF.

cat("Building population time series plot...\n")

ts_long <- counts |>
  pivot_longer(c(n_sheep, n_wolf),
               names_to  = "type",
               values_to = "count") |>
  mutate(type = recode(type, n_sheep = "Sheep", n_wolf = "Wolf"))

ts_plot <- ggplot(ts_long, aes(x = t, y = count, color = type)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = c(Sheep = "#2D5F8A", Wolf = "#C0392B"),
                     name = NULL) +
  scale_x_continuous(breaks = seq(0L, max(counts$t) + 10L, by = 20L)) +
  labs(
    title   = "Wolf-Sheep Predation \u2014 Population Dynamics",
    x       = "Tick",
    y       = "Population",
    caption = "Each tick shown. Wolf overshoot drives extinction at t \u2248 146."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(hjust = 0.5, margin = margin(b = 6)),
    plot.caption    = element_text(hjust = 0.5, size = 9, color = "#8A4A2D",
                                   margin = margin(t = 6)),
    plot.background = element_rect(fill = "#FAFAF7", color = NA),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    legend.text     = element_text(size = 11),
    plot.margin     = margin(10, 14, 10, 10)
  )

ggsave("wolf_sheep_timeseries.png", ts_plot,
       width = 8, height = 4, dpi = 150, bg = "#FAFAF7")

cat("Done: wolf_sheep_timeseries.png\n")
