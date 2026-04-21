# =============================================================================
# Agent-Based Modeling for Statisticians — JSM 2025
# Chapter 3: The Size of Firms, and What It Tells Us
# =============================================================================
#
# This script contains all R code from Chapter 3 of the course book.
# It is self-contained: all required packages are loaded in the setup section.
#
# Sections:
#   1. Setup
#   2. Firm size data: generate synthetic power-law data (placeholder)
#      Note: replace with US Census SUSB data for actual empirical analysis.
#   3. Power law fitting via MLE (poweRlaw package)
#
# Packages required:
#   tidyverse, poweRlaw, broom, patchwork
#
# Data note:
#   Chapter 3 uses synthetic data generated from a known power law distribution
#   via poweRlaw::rplcon(). To use real data, download the US Census Bureau
#   Statistics of US Businesses (SUSB) employment size class tables from:
#   https://www.census.gov/programs-surveys/susb.html
#   and substitute the empirical firm employment values for `firm_sizes`.
#
# GitHub: [course repository]
# =============================================================================

## ----ch3-setup----------------------------------------------------------------
# Chapter 3 Setup
# Self-contained — does not depend on prior chapters.

library(tidyverse)
library(poweRlaw)
library(broom)
library(patchwork)

theme_set(
  theme_minimal(base_family = "Source Serif 4") +
    theme(
      plot.title       = element_text(family = "Raleway", face = "plain",
                                      size = 14, margin = margin(b = 8)),
      plot.subtitle    = element_text(family = "Source Serif 4", size = 11,
                                      color = "#555555", margin = margin(b = 12)),
      axis.title       = element_text(family = "Source Serif 4", size = 10),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "#FAFAF7", color = NA)
    )
)

#' 
#' ## A Regularity That Refuses to Go Away {#sec-firmsize}
#' 
#' In virtually every measured economy, at virtually every measured time, the distribution of firm sizes by employment follows a power law. The United States in 1990, France in 2005, India in 2010 — the exponent shifts slightly, the lower bound shifts with institutional definitions of what constitutes a firm, but the power law structure persists. This is among the most robustly documented regularities in empirical economics, and it is one that standard economic models are structurally incapable of reproducing.
#' 
#' The dynamic range is the first clue that something unusual is happening. The largest firms in the US economy employ hundreds of thousands of workers; the smallest employ one. The ratio between these extremes is on the order of $10^5$ or $10^6$. A normal distribution cannot span that range without assigning negligible probability to the observed data. A log-normal can span it, and the log-normal has been proposed as an alternative; but the fit is systematically worse in the upper tail, where the data are densest in the logarithmic sense that matters for a power law.
#' 
#' A power law distribution has the form $P(\text{size} \geq x) \propto x^{-(\alpha - 1)}$ for $x \geq x_\text{min}$. In logarithmic coordinates, the complementary cumulative distribution function is a straight line with slope $-(\alpha - 1)$. On a linear scale, the distribution looks like a spike near zero and an invisible tail. The linear plot defeats the intuition; the log-log plot reveals the structure.
#' 
## ----ch3-data, fig.cap="Firm size distribution on a linear scale (left) and log-log scale (right, CCDF). The dynamic range — from single-employee firms to corporations employing hundreds of thousands — defeats any visualization on a linear scale. The log-log plot reveals the power law: a straight line across many orders of magnitude."----
# NOTE: The data below are synthetic, generated from a power law distribution
# as a placeholder. Substitute US Census Bureau SUSB microdata for real analysis.
# SUSB data available at: https://www.census.gov/programs-surveys/susb.html
# The power law exponent (alpha ≈ 2.05) is consistent with published estimates
# for the US firm size distribution (cf. Axtell 2001, Science).

set.seed(2025)  # seed for reproducibility; change to generate different samples

# Generate synthetic firm size data from a power law distribution
# rplcon: random draws from a continuous power law with lower bound xmin
n_firms    <- 5000
true_alpha <- 2.05   # exponent consistent with Axtell (2001)
xmin_true  <- 1

firm_sizes_raw <- rplcon(n_firms, xmin = xmin_true, alpha = true_alpha)
firm_df        <- tibble(employment = pmax(1L, round(firm_sizes_raw)))

# Linear scale histogram
p_linear <- ggplot(firm_df, aes(x = employment)) +
  geom_histogram(bins = 60, fill = "#2D5F8A", alpha = 0.75, color = "white") +
  labs(
    title    = "Firm size distribution: linear scale",
    subtitle = "The dynamic range makes the distribution invisible on this scale",
    x        = "Employment",
    y        = "Count"
  )

# Log-log CCDF (complementary CDF)
ccdf_df <- firm_df %>%
  arrange(employment) %>%
  mutate(
    rank = rev(seq_len(n())),
    ccdf = rank / n()
  ) %>%
  distinct(employment, .keep_all = TRUE)

p_loglog <- ggplot(ccdf_df, aes(x = employment, y = ccdf)) +
  geom_point(color = "#2D5F8A", size = 0.8, alpha = 0.6) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  labs(
    title    = "Firm size distribution: log-log CCDF",
    subtitle = "A straight line on this scale is a power law",
    x        = "Employment (log scale)",
    y        = "P(size \u2265 x) (log scale)"
  )

p_linear + p_loglog

#' 
## ----ch3-powerlaw-fit, fig.cap="Power law fit (red line) and log-normal comparison (dashed blue) on the log-log CCDF. The power law is estimated via maximum likelihood using the \\texttt{poweRlaw} package; $x_{\\min}$ is selected by minimizing the Kolmogorov-Smirnov distance between the fitted distribution and the empirical data above the threshold."----
# Fit power law via MLE using poweRlaw
# displ: discrete power law (appropriate for integer employment counts)
m_pl  <- displ$new(firm_df$employment)
est   <- estimate_xmin(m_pl)
m_pl$setXmin(est)
m_pl$setPars(estimate_pars(m_pl))

alpha_hat <- m_pl$getPars()
xmin_hat  <- m_pl$getXmin()

# Fit log-normal for comparison (same xmin for fair comparison)
m_ln <- dislnorm$new(firm_df$employment)
m_ln$setXmin(xmin_hat)
m_ln$setPars(estimate_pars(m_ln))

# Compute fitted CCDFs for plotting
x_seq <- seq(xmin_hat, max(firm_df$employment), length.out = 200)

pl_ccdf_df <- tibble(
  x    = x_seq,
  ccdf = (x_seq / xmin_hat)^(-(alpha_hat - 1)),
  dist = "Power law (MLE)"
)

# Log-normal CCDF approximation via plnorm
mu_ln    <- m_ln$getPars()[1]
sigma_ln <- m_ln$getPars()[2]
ln_ccdf_df <- tibble(
  x    = x_seq,
  ccdf = plnorm(x_seq, meanlog = mu_ln, sdlog = sigma_ln, lower.tail = FALSE) /
         plnorm(xmin_hat, meanlog = mu_ln, sdlog = sigma_ln, lower.tail = FALSE),
  dist = "Log-normal"
)

fit_df <- bind_rows(pl_ccdf_df, ln_ccdf_df)

ggplot() +
  geom_point(
    data = filter(ccdf_df, employment >= xmin_hat),
    aes(x = employment, y = ccdf),
    color = "#888888", size = 0.8, alpha = 0.5
  ) +
  geom_line(
    data = fit_df,
    aes(x = x, y = ccdf, color = dist, linetype = dist),
    linewidth = 1.0
  ) +
  scale_x_log10(labels = scales::comma) +
  scale_y_log10(labels = scales::comma) +
  scale_color_manual(values = c("Power law (MLE)" = "#C0392B", "Log-normal" = "#2D5F8A"),
                     name = NULL) +
  scale_linetype_manual(values = c("Power law (MLE)" = "solid", "Log-normal" = "dashed"),
                        name = NULL) +
  annotate("text", x = xmin_hat * 1.1, y = 0.05,
           label = paste0("x\u2098\u1d35\u2099 = ", xmin_hat,
                          "  \u03b1\u0302 = ", round(alpha_hat, 3)),
           family = "Source Serif 4", size = 3.5, hjust = 0, color = "#C0392B") +
  labs(
    title    = "Power law vs. log-normal fit",
    subtitle = paste0("Estimated exponent \u03b1\u0302 = ", round(alpha_hat, 3),
                      " (synthetic data; substitute SUSB for real analysis)"),
    x        = "Employment (log scale)",
    y        = "P(size \u2265 x) (log scale)"
  )

#' 
#' The power law fits the upper tail well. The log-normal fits less cleanly at the extreme upper end — it decays faster than the empirical distribution demands. This is not a marginal distinction: the upper tail is where the firm size distribution carries its most economically significant content. The hundred largest firms in the US employ a substantial fraction of the private sector workforce. A distribution that mismodels the upper tail mismodels the economy's employment structure in the sector where concentration, market power, and aggregate dynamics are most consequential.
#' 
#' ## What the Standard Models Cannot Say {#sec-sufficient-stats}
#' 
#' The SIR system tracks three numbers: $S$, $I$, and $R$. The dynamics are fully determined by those totals. This is not merely a modeling convenience — it is a claim about the Markov state. Under the mean-field assumption, the future trajectory of the epidemic is conditionally independent of which specific individuals are susceptible or infectious, given the compartment counts. The identity of the individuals does not matter; only their number does. Individual heterogeneity averages out under random mixing, and the law of large numbers makes the aggregate dynamics deterministic functions of population totals alone. The compartment counts are the complete state — not because individual characteristics have been shown to be irrelevant, but because the mean-field assumption has been imposed, and under that assumption they are.
#' 
#' The same logic, applied to industry dynamics, produces a model that tracks total employment, or average firm size, rather than the distribution of employment across firms. Each firm $i$ has an employment level $\theta_i = \bar\theta + \varepsilon_i$ where $\bar\theta$ is mean employment and $\varepsilon_i$ is the firm's deviation from that mean. Setting $\varepsilon_i = 0$ for all firms — what economists call the representative agent assumption — collapses the entire distribution to a point mass at $\bar\theta$. The distribution of firm sizes ceases to exist as an object of the model. Knowing that one firm employs 200,000 workers and ten thousand firms employ fewer than ten is indistinguishable, within this model, from knowing that all firms employ the mean number — not because the heterogeneity has been shown to be dynamically irrelevant, but because it has been assumed away before the analysis begins.
#' 
#' The power law distribution is then not merely a poor fit within this framework. It is a property of the non-zero $\varepsilon_i$ distribution — a property of the shape of the deviations from the mean — and a model that sets all $\varepsilon_i$ to zero has no mechanism through which distributional shape could arise. The restriction has eliminated the phenomenon's carrier before estimation begins.
#' 
#' The power law distribution is the empirical demonstration that the $\varepsilon_i$ are not zero — that the deviations are not negligible perturbations around a representative mean but the primary structure of the phenomenon. A power law has no finite mean when $\alpha \leq 2$, and even when the mean exists, it is a poor summary of the distribution's behavior. The distribution is dominated by its tail. The hundred largest firms in the US economy account for a substantial fraction of private-sector employment; they respond to shocks differently, shape credit conditions differently, and drive aggregate dynamics differently than a population of mean-sized firms would. The $\varepsilon_i = 0$ restriction produces the wrong object of analysis, not an imprecise approximation of the right one.
#' 
#' This is not a methodological complaint. It is an empirical constraint. The firm size distribution is among the most robustly documented regularities in empirical economics. It appears in every measured economy, at every measured time, in virtually every sector. A model whose language cannot express this regularity is, to that extent, describing a different economy than the one that exists.
#' 
#' The failure is linguistic before it is empirical. A model built on $\varepsilon_i = 0$ has no sentence in which "the distribution of firm sizes is a power law" can be written. The distribution is the phenomenon. When the language cannot contain the distribution, you have not approximated the phenomenon poorly. You have excluded it from the territory the model covers.
#' 
#' ## Axtell's Model: Logic Without Code {#sec-axtell}
#' 
#' Robert Axtell showed in 2001 that a remarkably simple agent-based model generates the observed US firm size distribution without calibration. The exponent emerges from the dynamics rather than being fitted to the data.
#' 
#' The model's logic is spare. Agents allocate their effort between working alone and working as part of a team. Teams are groups of agents who have chosen to coordinate their work. Within a team, returns to scale are locally increasing: a worker who joins a team adds more than their individual marginal product, up to some point. This is not an unusual assumption about firm production — most accounts of the gains from economic organization depend on something similar.
#' 
#' Agents compare their current payoff within their team to alternative arrangements: a different team, or solo work. When a better option exists, agents move. Teams form when agents find mutually beneficial collaborations; teams dissolve when enough members find better alternatives elsewhere. There is no central planner, no equilibrium condition that pins down firm size, no seed distribution. The model starts from random initial conditions and runs.
#' 
#' The result is the power law. Not fitted. Generated. The exponent that emerges from the dynamics is consistent with the empirically observed exponent across a wide range of the model's internal parameters. This robustness is the crucial point: the power law is not an artifact of a particular calibration. It is a structural consequence of the interaction rules — teams forming and dissolving under local increasing returns to scale, with agents choosing based on local information about their available alternatives.
#' 
#' The model does not explain why individual firms have the sizes they have. It explains why the distribution has the shape it has. The macro regularity — the power law — is not encoded in any individual agent's rule. No agent knows the distribution. No agent is trying to produce a power law. The distribution arises from the aggregate dynamics of agents following their individual rules, and it is not visible anywhere in those rules.
#' 
#' ## Emergence {#sec-emergence}
#' 
#' A macro-level regularity is emergent if it satisfies two conditions. It cannot be derived analytically from the micro-level rules — there is no algebraic path from the individual agent's decision problem to the macro distribution. And it arises robustly from the dynamics of agent interaction — not an artifact of specific parameter values or initial conditions, but a structural feature that persists across a wide range of both.
#' 
#' The power law firm size distribution is emergent in this sense. No analysis of an individual agent's effort allocation problem reveals the exponent. The exponent is not a property of any individual; it is a property of the aggregate dynamics that arise from the interaction of many individuals following local rules. And it is robust: Axtell showed that the exponent is insensitive to a wide range of parameter choices, which means it is structural rather than calibrated.
#' 
#' That robustness is more than a convenience result. Axtell's model was not fitted to the power law. The exponent emerged from the dynamics and was then compared to the data. This is not a subtle distinction. Equilibrium models of industry dynamics had the firm size distribution in front of them for decades — the empirical regularity was documented in the 1950s, and the power law form was well established by the 1990s — and could not generate the exponent without external calibration, because the exponent is not a property of any equilibrium. It is a property of the path: the motion of teams forming and dissolving, agents switching affiliations, distributions arising from trajectories that no equilibrium condition pins down. Equilibrium analysis specifies the destination; the distribution lives in the journey.
#' 
#' A model that generates a known regularity without having been calibrated to it has done something that descriptive fitting cannot. It exposes the mechanism. The mechanism can then be interrogated: what changes in the interaction rules would change the exponent? What perturbations to the returns-to-scale structure shift the distribution? These questions only make sense in a language that contains the generative process. A model that describes the distribution after the fact cannot answer them, because it contains no process — only a fit.
#' 
#' The phenomenon is unreachable by any model built on $\varepsilon_i = 0$ — in principle, with any parameter choices. Such a model has no dynamics in the relevant sense: it has an equilibrium at which one firm employs a fixed number of workers. There is no process of team formation and dissolution. There is no mechanism through which a distribution could be generated, because the model's language contains no concept of a distribution of firm sizes. The $\varepsilon_i = 0$ restriction has collapsed exactly the dimension of heterogeneity from which the regularity is made.
#' 
#' Emergence, precisely defined, is a methodological constraint. If the phenomenon is emergent — if no analytical derivation from the micro rules can reach it — then the model must be dynamic, heterogeneous, and process-based. Not because agent-based modeling is philosophically preferable to equilibrium analysis. Because it is the only language in which the generative mechanism can be written. The sufficiency assumption has collapsed exactly the $\varepsilon_i$ dimension from which the regularity is made. No relaxation of the equilibrium condition, no refinement of the production function, no additional sector recovers the power law from a model that sets $\varepsilon_i = 0$ for firms, because the distribution is a property of the non-zero $\varepsilon_i$ tail — a tail the model has eliminated before the analysis begins.
#' 
#' ---
#' 
#' # The Possibility Space of Dynamics {#ch4}
#' 
