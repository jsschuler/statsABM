#!/usr/bin/env Rscript
# Vectorized spatial LV — saves spatial_lv.png (run from slides/)

suppressPackageStartupMessages({
  library(tidyverse)
  library(patchwork)
})

set.seed(2025)
G     <- 40L
T_MAX <- 120L
SNAP  <- c(1L, 20L, 40L, 60L, 90L, 120L)

# Toroidal matrix shift
shift_wrap <- function(m, dr, dc) {
  m[((seq_len(G) - 1L - dr) %% G) + 1L,
    ((seq_len(G) - 1L - dc) %% G) + 1L,
    drop = FALSE]
}

# Sum of all 8 Moore neighbors (vectorized)
nbr_sum <- function(m) {
  shift_wrap(m,-1L,-1L) + shift_wrap(m,-1L, 0L) + shift_wrap(m,-1L, 1L) +
  shift_wrap(m, 0L,-1L) +                          shift_wrap(m, 0L, 1L) +
  shift_wrap(m, 1L,-1L) + shift_wrap(m, 1L, 0L) + shift_wrap(m, 1L, 1L)
}

MAX_CELL <- 20L   # cap per-cell counts to prevent overflow

step <- function(prey, pred,
                 p_birth = 0.22, p_eat = 0.30,
                 p_death = 0.10, p_conv = 0.70) {
  prey <- pmin(prey, MAX_CELL)
  pred <- pmin(pred, MAX_CELL)

  # Prey reproduce; offspring disperse to Moore neighbors
  births   <- matrix(rbinom(G*G, prey, p_birth), G, G)
  new_prey <- pmin(MAX_CELL, prey + round(nbr_sum(births) / 8))

  # Local predation
  enc      <- pmin(new_prey, pred)
  eaten    <- matrix(rbinom(G*G, enc,  p_eat),   G, G)
  new_prey <- pmax(0L, new_prey - eaten)

  # Predator bookkeeping
  born     <- matrix(rbinom(G*G, eaten, p_conv),  G, G)
  died     <- matrix(rbinom(G*G, pred,  p_death), G, G)
  new_pred <- pmin(MAX_CELL, pmax(0L, pred + born - died))

  list(prey = new_prey, pred = new_pred)
}

# Initialise — random scatter
prey <- matrix(rbinom(G*G, 3L, 0.5), G, G)
pred <- matrix(rbinom(G*G, 1L, 0.3), G, G)

cat("Simulating", T_MAX, "steps on", G, "x", G, "grid...\n")
snaps <- list()
if (1L %in% SNAP) snaps[["t = 1"]] <- prey

for (tt in seq(2L, T_MAX)) {
  s    <- step(prey, pred)
  prey <- s$prey; pred <- s$pred
  if (tt %in% SNAP) snaps[[paste("t =", tt)]] <- prey
  if (sum(prey) == 0L || sum(pred) == 0L) { cat("Collapsed at", tt, "\n"); break }
}
cat("Simulation done.\n")

make_panel <- function(mat, label) {
  df <- tibble(
    x    = rep(seq_len(G), each = G),
    y    = rep(seq_len(G), times = G),
    prey = as.vector(mat)
  )
  ggplot(df, aes(x = x, y = y, fill = prey)) +
    geom_tile() +
    scale_fill_gradient(low = "#FAFAF7", high = "#2D5F8A", guide = "none") +
    coord_equal(expand = FALSE) +
    labs(title = label) +
    theme_void() +
    theme(plot.title      = element_text(size = 9, hjust = 0.5,
                                         margin = margin(b = 2)),
          plot.background = element_rect(fill = "#FAFAF7",
                                         color = "#DCDCD4", linewidth = 0.3))
}

cat("Building figure...\n")
panels <- imap(snaps, make_panel)
fig <- wrap_plots(panels, ncol = 3) +
  plot_annotation(
    title    = "Spatial Lotka-Volterra — prey density",
    subtitle = "Local Moore-neighborhood interaction. Spatial clustering and travelling waves\narise from local rules alone — inexpressible in the mean-field ODE.",
    theme    = theme(
      plot.title    = element_text(size = 11, margin = margin(b = 4)),
      plot.subtitle = element_text(size = 8,  color = "#555552"),
      plot.background = element_rect(fill = "#FAFAF7", color = NA)
    )
  )

ggsave("spatial_lv.png", fig, width = 7, height = 5.2, dpi = 150, bg = "#FAFAF7")
cat("Done: spatial_lv.png\n")
