# Claude Code Specification: Agent-Based Modeling for Statisticians
## JSM Short Course — Full Day
## Version 2

---

## Project Overview

This specification generates two parallel sets of R Markdown course materials for a full-day JSM short course titled **"Agent-Based Modeling for Statisticians."** The course moves from ODE-based models (Lotka-Volterra, SIR) through agent-based versions of the same, to Axtell's firm size empirics, and concludes with a philosophical argument about expressive resolution, the structure of modeling assumptions, and policy humility.

All materials must be production-ready R Markdown. The course text is a single unified document with high production value. The slides are separated by section and built in Xaringan. A shared theme scaffold is used across both.

All R code uses **Tidyverse + deSolve + ggplot2** conventions throughout. No base R graphics. LaTeX math via MathJax. Code chunks within a chapter may depend on earlier chunks in the same chapter but never on chunks from a prior chapter — each chapter opens with a self-contained setup chunk.

---

## Stylistic Exemplar

**This is the most important section of this specification. Read it before writing a single word of prose.**

The repository will contain a file at the path:

```
style/spectral_hands.tex
```

This is a paper by the course author. Before writing any prose, read it in full. It is the stylistic target for the course book. Study it at the sentence level. Notice:

- How technical vocabulary is introduced through argument rather than definition
- How the prose moves between registers — historical, mathematical, philosophical — without signaling the transitions
- How wit operates through juxtaposition and etymology rather than jokes
- How the argument builds so that conclusions feel retrospectively inevitable
- The rhythm of the sentences: varied length, occasional abruptness, the periodic long sentence that earns its length

The course book should sound like this paper. Not an imitation — the subject matter differs — but the same mind, the same confidence, the same pleasure in ideas.

### Prose Register

The prose is **precise and playful**. These are not in tension. Precision means every technical term is doing real work and could not be replaced by a vaguer one. Playfulness means the prose is visibly enjoying the argument — through unexpected angles of approach, etymological wit, the occasional sentence that arrives somewhere the reader did not expect.

The prose is **not**:
- Dry technical exposition
- Performed enthusiasm
- Hedged academic prose that qualifies every claim into meaninglessness
- Conversational in the sense of informal or imprecise

The argument has **Mandelbrotian structure**: the conclusion of each section feels inevitable in retrospect. The reader finishes a section feeling they could not have arrived anywhere else. This is achieved not by stating conclusions early but by making each step feel necessary — the next move is the only move available given what has just been shown.

### Absolutely Forbidden Phrases and Constructions

The following are banned. Their appearance anywhere in the prose is an error requiring correction:

- "close the loop" / "comes full circle"
- "unpack" (as in "let us unpack this")
- "dive into" / "deep dive"
- "it's worth noting that"
- "at the end of the day"
- "in other words" (use it sparingly; prefer to simply restate more precisely)
- "as we have seen" (show don't tell — if the reader has seen it, they know)
- "importantly," as a sentence opener
- "interesting" as a content-free intensifier
- "powerful" applied to mathematical tools or frameworks
- Any sentence of the form "In this section, we will..."
- Any sentence of the form "Having established X, we now turn to Y"
- Rhetorical questions that answer themselves in the next sentence

These constructions are the grammatical equivalent of a shrug. The prose should never shrug.

---

## Repository Structure

```
abm-statisticians/
├── README.md
├── _theme/
│   ├── custom.css              # Shared CSS for course book (HTML output)
│   ├── xaringan-theme.R        # xaringanthemer scaffold, sourced by all slide decks
│   └── fonts.css               # Google Fonts import block
├── style/
│   └── spectral_hands.tex      # Stylistic exemplar — READ BEFORE WRITING PROSE
├── book/
│   ├── abm_course_book.Rmd     # Single unified course textbook
│   └── refs.bib                # BibTeX references
├── slides/
│   ├── 01_ode_foundations.Rmd
│   ├── 02_abm_contrast.Rmd
│   ├── 03_axtell_empirics.Rmd
│   └── 04_philosophical_conclusion.Rmd
└── code/
    ├── ch01_ode_foundations.R
    ├── ch02_abm_contrast.R
    ├── ch03_axtell_empirics.R
    └── ch04_philosophical_conclusion.R
```

The `code/` directory contains the same R code as the book chunks, fully commented, for distribution via GitHub. These are the files students take away.

---

## Visual Identity & Theme Scaffold

### Philosophy

The visual identity should feel like a serious intellectual artifact — something between a well-designed academic monograph and a piece of scientific journalism. It should signal that the author takes both the mathematics and the prose seriously. It should not look like a default R Markdown document, a corporate slide deck, or a generic data science tutorial.

The aesthetic target is **refined typographic seriousness with quiet distinctiveness.** Not loud. Not playful. Not corporate. Think: a beautifully typeset book you find in the mathematics section of a good independent bookstore.

### Fonts

All fonts loaded from Google Fonts.

- **Body text:** Source Serif 4 (weights 400, 400 italic, 600)
- **Headings:** Raleway (weights 300, 500, 700)
- **Code:** Fira Code (with ligatures enabled via `font-variant-ligatures: contextual`)

### Color Palette (Placeholder — to be refined after content is complete)

Define all colors as CSS variables so they can be changed globally in one place.

```css
:root {
  --color-bg:         #FAFAF7;   /* warm off-white, not pure white */
  --color-text:       #1C1C1E;   /* near-black, slightly warm */
  --color-heading:    #1C1C1E;
  --color-accent:     #2D5F8A;   /* a measured steel blue — placeholder */
  --color-accent-alt: #8A4A2D;   /* warm rust — placeholder for secondary accent */
  --color-rule:       #DCDCD4;   /* subtle horizontal rules */
  --color-code-bg:    #F0F0EA;   /* slightly warm code block background */
  --color-link:       #2D5F8A;
}
```

These are placeholders. The color scheme should be revisited once content is drafted. The key constraint is that it must work for both extended reading (book) and projection (slides).

### Course Book HTML Output Parameters

```yaml
---
title: "Agent-Based Modeling for Statisticians"
subtitle: "A Short Course"
author: "John Lynham"
date: "Joint Statistical Meetings, 2025"
output:
  html_document:
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: true
    toc_depth: 3
    number_sections: true
    theme: null           # disable default Bootstrap theme entirely
    css: ["../_theme/fonts.css", "../_theme/custom.css"]
    highlight: null       # code highlighting handled in custom.css
    self_contained: true  # single portable HTML file
    mathjax: "default"
    fig_caption: true
    fig_width: 8
    fig_height: 5
    df_print: paged
    code_folding: show
    includes:
      in_header: "../_theme/header.html"
---
```

### Xaringan Slide Parameters

```yaml
---
title: "[Section Title]"
subtitle: "Agent-Based Modeling for Statisticians"
author: "John Lynham"
date: "JSM 2025"
output:
  xaringan::moon_reader:
    css: ["../_theme/xaringan-custom.css", "default-fonts"]
    lib_dir: libs
    nature:
      highlightStyle: github
      highlightLines: true
      countIncrementalSlides: false
      ratio: "16:9"
      navigation:
        scroll: false
    self_contained: false
---
```

### xaringanthemer Scaffold (`_theme/xaringan-theme.R`)

```r
library(xaringanthemer)

style_mono_accent(
  # Base colors — placeholders, to be refined
  base_color         = "#2D5F8A",
  white_color        = "#FAFAF7",
  black_color        = "#1C1C1E",
  background_color   = "#FAFAF7",
  header_color       = "#1C1C1E",
  text_color         = "#1C1C1E",
  link_color         = "#2D5F8A",
  text_slide_number_color = "#AAAAAA",

  # Typography
  header_font_google  = google_font("Raleway", "300, 500, 700"),
  text_font_google    = google_font("Source Serif 4", "400, 400i, 600"),
  code_font_google    = google_font("Fira Code"),

  # Sizing
  text_font_size      = "1.05rem",
  header_h1_font_size = "2.2rem",
  header_h2_font_size = "1.7rem",
  header_h3_font_size = "1.3rem",
  code_font_size      = "0.85rem",

  # Output
  outfile = "_theme/xaringan-custom.css"
)
```

This file is run once to generate the CSS. It is not sourced at render time.

---

## The Argument of the Course

This section describes the intellectual arc that the prose must carry. Every chapter, section, and slide exists in service of this argument. It should be invisible as structure and felt only as inevitability.

### The Central Claim

Every modeling choice is a choice of resolution — temporal, spectral, and expressive. The ODE, the representative agent, stationarity, equilibrium, regularization penalties, sampling frequency — all restrict the language in which the modeler works. Each restriction makes some phenomena expressible and others not. A phenomenon that requires distinctions the model's language cannot make is not a failure of fit. It is exterior to the model's dogma entirely.

This is not a counsel of despair about modeling. It is a demand for precision about what each model can and cannot say — and therefore about what conclusions may and may not be drawn from it.

### The Devastating Example

The mean-field assumption is the hinge on which the entire argument turns. It is the single restriction that, when stated precisely, makes visible everything the ODE cannot see. It should arrive at the end of Chapter 1 with full weight — stated not as a criticism but as a fact about the model's language. Everything in Chapters 2, 3, and 4 is the demonstration of what lies outside that language.

### Answering the Critics Without Appearing to

The standard objections to ABM — that it multiplies assumptions, that it can produce any prediction — must be answered without ever appearing defensive. The strategy is to answer them before they are raised, in the language the audience already speaks.

**The "multiplies assumptions" objection** is answered by the reduction result in Chapter 2. The ABM does not add assumptions to the ODE — it relaxes a restriction the ODE imposes silently. Statisticians understand this immediately: the ODE is the degenerate special case where the distribution of agent characteristics and interaction outcomes collapses to a point mass. An ODE is an ABM with a very strong prior.

**The "can produce any prediction" objection** is answered by the sparsity point. The nominal parameter space of an ABM may be large, but the effective dimensionality of the calibrated structure is low — most parameter combinations produce qualitatively identical dynamics, and the interesting behavior lives on a low-dimensional manifold. This is regularization. The critic who worries about parameter proliferation is confusing nominal dimensionality with effective dimensionality. A statistician who has used LASSO understands the distinction immediately.

**The deeper unification**: Bayesian priors are regularization tools — this is not an analogy but a mathematical identity. A Gaussian prior on coefficients produces ridge regression exactly. A Laplace prior produces LASSO exactly. The posterior mode under these priors *is* the regularized estimator. Further, L2 regularization is the limiting case of training on Gaussian-perturbed data; the Bayesian, the frequentist regularizer, and the distributionally robust optimizer are solving the same problem from different angles. Every modeling assumption is a prior. Every prior restricts the expressive language. The question is always whether the restriction is warranted for the phenomenon at hand.

**The Simon near-decomposability point**: different models fit well at different temporal resolutions because each resolution has its natural phenomena. This is not merely a practical observation — it is a theorem about the spectral structure of complex systems. A quarterly model is not wrong about the annual dynamics it was built to describe. It is linguistically incapable of expressing sub-quarterly phenomena. The error is applying it at the wrong resolution, or failing to ask what resolution the policy question actually requires. Stationarity is a temporal regularity prior: it asserts that the data-generating process lies within a neighborhood of the estimated distribution. When the system is capable of bifurcation, that neighborhood does not contain the true future distribution. The stationarity prior is miscalibrated in exactly the way a Gaussian prior is miscalibrated when the true coefficients are sparse.

---

## Course Structure & Learning Objectives

### Block 1: ODE Foundations (8:30–10:00)
**Slides file:** `01_ode_foundations.Rmd`
**Book chapter:** Chapter 1

**Learning objectives:**
1. Derive Lotka-Volterra from verbal biological assumptions, identifying each parameter precisely.
2. Derive SIR from first principles, with explicit attention to the mass-action assumption.
3. Understand the qualitative dynamics of both systems: neutral cycling in Lotka-Volterra, epidemic threshold and herd immunity in SIR.
4. Articulate the mean-field assumption precisely — a prior that collapses the distribution of interactions to a point mass, not a neutral modeling default.
5. Solve and visualize both systems in R using `deSolve` and `ggplot2`.

---

### Block 2: Agent-Based Contrast (10:15–12:00)
**Slides file:** `02_abm_contrast.Rmd`
**Book chapter:** Chapter 2

**Learning objectives:**
1. Implement agent-based versions of Lotka-Volterra and SIR in R using Tidyverse data structures.
2. Show the reduction result explicitly: ABM output converges to ODE output under random mixing and large population — the ODE is a limiting case, not a separate paradigm.
3. Show what lies outside the ODE's language: extinction events, network topology effects, spatial structure.
4. Implement network-based SIR using `igraph` / `tidygraph`; demonstrate how contact network topology changes epidemic dynamics in ways the ODE cannot express.
5. Display a pre-rendered `gganimate` animation of spatial Lotka-Volterra — shown as demonstration, code available in materials.
6. Articulate the sparsity point: the effective dimensionality of the calibrated ABM is low; this is regularization in a sense statisticians already understand.

---

### Block 3: Axtell Firm Size (12:30–3:15)
**Slides file:** `03_axtell_empirics.Rmd`
**Book chapter:** Chapter 3

**Note:** No live model execution. R code is used only for visualizing empirical distributions and fitting power law models.

**Learning objectives:**
1. Understand the firm size power law as an empirical regularity that is cross-country stable and temporally robust.
2. Fit and visualize power law distributions in R; compare to lognormal and exponential alternatives.
3. Understand why this distribution is not merely poorly fit but linguistically inexpressible by representative agent models — the representative agent is the mean-field assumption applied to the distribution of firms.
4. Understand the qualitative logic of Axtell's model: agents, returns to scale, team formation and dissolution as the generative mechanism.
5. Define emergence precisely: a macro regularity exterior to the language of its micro rules.

**R packages:** `poweRlaw`, `ggplot2`, `tidyverse`, `broom`

---

### Block 4: The Possibility Space of Dynamics (3:30–5:00)
**Slides file:** `04_philosophical_conclusion.Rmd`
**Book chapter:** Chapter 4

**Learning objectives:**
1. Articulate the unified framework: every modeling choice is a restriction on expressive resolution — temporal, spectral, linguistic.
2. Understand the mean-field failure mode: network topology and heterogeneity are exterior to its language.
3. Understand the naive extrapolation failure mode: regime change and bifurcation are exterior to a stationarity prior — and stationarity is a prior, with all that implies about miscalibration.
4. Understand the unwarranted equilibrium failure mode: path dependence and lock-in are exterior to an equilibrium prior.
5. Connect regularization, Bayesian priors, and distributional robustness as three descriptions of the same operation: restricting effective model complexity by restricting the language.
6. Apply the Simon near-decomposability point: models fit at particular temporal resolutions; the error is resolution mismatch, not model richness.
7. Articulate the statistician's checklist question: at what resolution is this model working, and is the phenomenon I care about expressible at that resolution?

---

## Chapter-by-Chapter Content Specification

---

### Chapter 1: The Differential Equations of Living Systems

#### Prose Style Notes

Read `style/spectral_hands.tex` before writing this chapter. The opening of this chapter should have the quality of the opening of that paper — a provocative observation that makes the reader feel the terrain shifting underfoot before any formal apparatus has been introduced.

The chapter earns its formalism. The Lotka-Volterra system does not appear until the reader feels why a differential equation is the right instrument for this problem. The mean-field assumption does not appear as a criticism — it appears as a precise description of what the model is doing, stated with enough clarity that the reader can see both its utility and its cost.

Do not write: "In this section, we will derive..." Write the derivation. The section header is enough announcement.

#### Opening

Open with the observation that the simplest living systems defeat the intuitions we bring to them. Not because they are complicated — the rules governing a population of rabbits and foxes are not complicated — but because feedback does something to time that our unaided intuition cannot follow. A population of rabbits with unlimited food does not grow forever. An epidemic does not infect everyone it could. These outcomes are not accidents. They are the signatures of feedback, and the differential equation is the instrument built to read them.

Establish that the goal is not differential equations per se but rather to look at two classical models with fresh eyes, attending to the assumptions built into their structure — because those assumptions will become the subject of the rest of the day.

#### Section 1.1: Predators, Prey, and the Geometry of Cycles

Derive Lotka-Volterra from scratch. Start with verbal biological assumptions stated precisely:

- Prey grow exponentially absent predators
- Predators decline exponentially absent prey
- Encounters between predators and prey occur at a rate proportional to the product of their populations
- Each encounter reduces prey and, with some efficiency, increases predators

The product term $\beta NP$ deserves a sentence of its own before the equation appears. It says that the rate of encounters is proportional to the product of population sizes. This is the first hint of what will later be named explicitly.

$$\frac{dN}{dt} = \alpha N - \beta N P$$
$$\frac{dP}{dt} = \delta N P - \gamma P$$

Identify each parameter verbally. Derive the nullclines. The trajectories are closed orbits — the system cycles forever without damping or growth. Dwell on this. It is geometrically beautiful and physically strange: a pendulum that never loses energy, a clock that never runs down. The phase portrait is the right place to linger — the reader is seeing something genuinely odd about the world, and the prose should honor that.

Implement in R with `deSolve`. Produce:
1. Time series of N and P on shared axes (tidy format via `pivot_longer`, two colors)
2. Phase portrait showing closed orbits for several initial conditions

#### Section 1.2: The Mean-Field Assumption

Before SIR, name what has been hiding in the product term.

The expression $\beta NP$ says that every predator is equally likely to encounter every prey individual at every moment. The population is perfectly mixed. Space does not exist. Individuals have no persistent relationships, no locations, no neighborhoods. Every interaction is drawn at random from the full population, at every instant.

This is the mean-field assumption. It is not stated as an assumption in most presentations of Lotka-Volterra. It is embedded in the functional form. It is doing the work that makes the closed-orbit result possible — and it is doing considerably more work than that, work whose full extent will become apparent in Chapter 2.

Name it. Define it precisely. Note that it is a prior — it assigns all probability mass to a single interaction structure, the fully mixed population, and zero probability to everything else. Then move on. The seed is planted.

#### Section 1.3: Epidemics and the Threshold Phenomenon

Derive SIR from verbal assumptions. A closed population. Each individual is Susceptible, Infectious, or Recovered and immune. Infectious individuals contact susceptibles at a rate proportional to $SI$ — the mean-field assumption again, now governing human contact rather than predator-prey encounter. Infectious individuals recover at a constant per-capita rate.

$$\frac{dS}{dt} = -\beta S I$$
$$\frac{dI}{dt} = \beta S I - \gamma I$$
$$\frac{dR}{dt} = \gamma I$$

Derive $\mathcal{R}_0 = \beta N / \gamma$ from the sign of $dI/dt$ at $t=0$. The epidemic threshold is not a numerical fact to memorize — it is a consequence of the model's structure, visible in the algebra. A reader who has seen only the number should see the derivation and recognize it as something they have always been owed.

Note — with deliberate understatement — that the years since 2020 have given most people in the room considerably more opinions about this model than they might have anticipated forming.

Implement in R with `deSolve`. Produce:
1. Time series of S, I, R (tidy format, `pivot_longer`)
2. Parameter sweep over $\mathcal{R}_0$ showing variation in epidemic curves

#### Section 1.4: What the Language Can Say

Close with a short synthetic section. Collect the shared assumptions of both models:

- Populations are continuous — no individuals, no discreteness
- Populations are homogeneous — every individual is equivalent
- Mixing is perfect and instantaneous — mass action throughout
- Space does not exist
- History does not matter — the state at $t$ fully determines the state at $t + dt$

These are not flaws. Models that make these assumptions have been scientifically productive for over a century. The point is that they are a language, and every language has an expressive boundary. The phenomena that lie outside this boundary are not anomalies to be explained away. They are the subject of the next chapter.

End with a sentence that opens toward Chapter 2 — not a signpost ("next we will see...") but a held breath, the feeling of a door not yet opened.

---

#### Chapter 1 Setup Chunk

```r
# Chapter 1 Setup
# All packages and global settings for this chapter.
# This chunk is self-contained — it does not depend on any prior chapter.

library(tidyverse)
library(deSolve)
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

palette_ch1 <- c(
  prey        = "#2D5F8A",
  predator    = "#8A4A2D",
  susceptible = "#2D5F8A",
  infectious  = "#C0392B",
  recovered   = "#27AE60"
)
```

---

### Chapter 2: Agents, Networks, and the Geometry of Interaction

#### Prose Style Notes

This chapter is where the argument turns. The writing should have the quality of a demonstration — the same world seen through a different instrument, and notice what was invisible before. The reduction result is the rhetorical center: show it before the audience thinks to ask for it, and they will trust everything that follows.

The sparsity and regularization point should arrive naturally, as if the connection were obvious — because to a statistician, once it is named, it is obvious. The prose should give the reader the pleasure of recognition, not the sensation of being taught.

#### Section 2.1: What Is an Agent?

An agent is an entity with a state, a set of rules governing how that state updates, and a neighborhood — a set of other agents it can interact with. The neighborhood is the structural innovation. In the ODE, every agent's neighborhood is the entire population, weighted uniformly. In the ABM, the neighborhood is local, structured, and potentially heterogeneous across agents and across time.

The ABM is not "more realistic" than the ODE in any simple sense. It is a different language — one in which the interaction structure is a first-class object rather than a fixed assumption. What you can say in this language that you cannot say in the ODE's language will become clear shortly.

#### Section 2.2: An Agent-Based Lotka-Volterra

Implement a non-spatial ABM version of Lotka-Volterra in R. Use a `tibble` to represent the population with columns for agent ID, type (prey/predator), and alive status. At each timestep: each prey reproduces with probability $p_{birth}$; predator-prey pairs drawn by random pairing result in prey death and predator reproduction with probability $p_{eat}$; each predator dies with probability $p_{death}$.

Show two results:

**The reduction result**: under random pairing with large populations, ABM time series converges to the ODE solution. Show this as an overlay — multiple stochastic ABM runs as translucent ribbons, ODE solution as a solid line. The ODE is not a different model. It is what this model becomes when you take the mean-field prior seriously.

**What the ODE cannot see**: with small populations, the ABM produces extinction events. The ODE orbits forever. The ABM can fall to zero, and stay there. The ODE's language has no word for this.

The sparsity point belongs here, in a brief paragraph: the ABM has more nominal parameters than the ODE, but the dynamics are not more complex in the effective sense — most of the parameter space produces qualitatively identical behavior, and the interesting structure is low-dimensional. A statistician who has used regularization to recover a sparse solution from a high-dimensional regression has already solved this problem in a different guise. The critic who objects to ABM parameter proliferation is counting nominal parameters. They should be counting effective dimensions.

#### Section 2.3: An Agent-Based SIR

Implement an agent-based SIR in two versions.

**Version 1 — Random mixing**: each infectious agent contacts a random subset of the population at each timestep. Show convergence to ODE under large $N$. This is the reduction result for epidemics.

**Version 2 — Network SIR**: use `igraph` to generate two contact networks with the same mean degree — an Erdős-Rényi random graph and a Barabási-Albert scale-free graph. Run SIR on each. The Erdős-Rényi produces dynamics similar to random mixing. The scale-free network produces a qualitatively different epidemic: faster initial spread through hubs, different final size, potential endemic persistence.

Visualize the network with `ggraph`, nodes colored by compartment at a key timestep. The visual is the argument: the same $\mathcal{R}_0$, the same parameters, and the epidemic is a different animal entirely. The ODE cannot express this difference because the ODE's language has no representation for the contact network. It is not that the ODE gets the wrong answer. It is that the question — what does topology do to epidemic dynamics? — cannot be asked in the ODE's language.

#### Section 2.4: The Reduction Theorem

State the result cleanly in prose, without excessive formalism:

As population size grows, as mixing becomes increasingly random, and as individual heterogeneity averages out across the population, the ABM converges in distribution to the ODE. The ODE is not wrong — it is a limiting case. It is what the ABM becomes when the mean-field prior is exact. Every ABM that includes random mixing as a special case contains the ODE as a theorem.

This framing dissolves the apparent competition between the two paradigms. There is no competition. There is a richer language and one of its sublanguages. The question is always whether the phenomena of interest are expressible in the sublanguage — and for many of the most interesting social and biological phenomena, they are not.

#### Section 2.5: Spatial Extension (Reference Module)

Mark this section explicitly as an extension for self-study. It is not covered in the live session but is fully implemented in the distributed materials.

Implement a spatial grid-based Lotka-Volterra in R. Agents occupy cells on a grid. Interactions are local — agents interact only with their Moore neighborhood (8 adjacent cells). Show spiral wave patterns and parameter regimes where spatial structure sustains populations that the mean-field version drives to extinction.

Include a `gganimate` animation rendered to gif. The animation is the argument. Spiral waves are a dynamical regime that is simply inexpressible in the mean-field language — they require the spatial language to exist at all.

The code is fully implemented and commented. In the live session, show the animation without walking through the code. The audience has already understood the principle; the animation is confirmation.

---

#### Chapter 2 Setup Chunk

```r
# Chapter 2 Setup
# Self-contained — does not depend on Chapter 1.

library(tidyverse)
library(deSolve)
library(igraph)
library(tidygraph)
library(ggraph)
library(gganimate)
library(patchwork)

theme_set(
  theme_minimal(base_family = "Source Serif 4") +
    theme(
      plot.title       = element_text(family = "Raleway", face = "plain",
                                      size = 14, margin = margin(b = 8)),
      axis.title       = element_text(family = "Source Serif 4", size = 10),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "#FAFAF7", color = NA)
    )
)

palette_ch2 <- c(
  prey        = "#2D5F8A",
  predator    = "#8A4A2D",
  susceptible = "#2D5F8A",
  infectious  = "#C0392B",
  recovered   = "#27AE60"
)
```

---

### Chapter 3: The Size of Firms, and What It Tells Us

#### Prose Style Notes

This chapter is the most empirical and the most argumentative. There is no model to run. The writing should have the quality of a detective story — here is a regularity in the world, here is what cannot explain it, here is what is required. The power law is the devastating example: a phenomenon that is not merely poorly fit by standard models but is inexpressible within their language.

The statistical work should be done with appropriate rigor. The naive log-log plot is not enough. Use maximum likelihood estimation via `poweRlaw`, report goodness-of-fit, compare to lognormal. The audience are statisticians; show them the statistics.

#### Section 3.1: A Regularity That Refuses to Go Away

In virtually every measured economy, the distribution of firm sizes by employment follows a power law over many orders of magnitude. The largest firms employ hundreds of thousands; the smallest employ one. The distribution between these extremes is not normal, not lognormal, not exponential. It is a power law, and it has been so for as long as anyone has measured it, across countries with radically different institutions, legal systems, and histories.

Present the empirical data. Show the histogram on a linear scale first — the dynamic range defeats it, and the defeat is instructive. Then show the log-log plot, and let the linear relationship speak. Fit a power law using maximum likelihood via `poweRlaw`. Compare to a lognormal alternative using a likelihood ratio test. The power law wins, or comes close enough that the point is made.

#### Section 3.2: What the Standard Models Cannot Say

A representative agent model produces a distribution with a characteristic scale — a typical firm size around which variation is distributed. An ODE model of industry dynamics produces smooth trajectories toward equilibrium configurations. Neither produces a power law, and neither can, because neither contains the mechanism that generates it.

The representative agent is the mean-field assumption applied to the distribution of firms. It collapses the entire heterogeneity of the firm population to a point mass — one firm, standing in for all firms, interacting with one consumer, standing in for all consumers. This is not an approximation that becomes exact in the limit of large populations. It is a restriction that eliminates the phenomenon entirely. The power law firm size distribution is not an equilibrium property of a representative agent economy. It is a dynamical property of a heterogeneous agent economy with local interaction. It is exterior to the representative agent's language.

This is not a methodological debating point. It is an empirical constraint. A model that cannot reproduce the firm size distribution is, to that extent, wrong about the economy it claims to describe — regardless of how well it fits aggregate moments.

#### Section 3.3: Axtell's Model — Logic Without Code

Describe the qualitative logic of Axtell's firm formation model:

Agents allocate effort between solitary work and team membership. Teams form when agents find collaborations that increase their returns under local increasing returns to scale. Teams dissolve when agents find better alternatives. No central planner. No equilibrium assumption. No representative agent.

The power law emerges from the dynamics of team formation and dissolution — purely endogenous, not calibrated. More strikingly, the power law exponent is not sensitive to most parameter choices. It is a structural consequence of the interaction rules, a macro regularity that is exterior to the micro rules in exactly the sense that makes it interesting.

#### Section 3.4: Emergence

Define emergence carefully: a macro-level regularity that is not present in the micro-level rules and cannot be derived analytically from them, but arises robustly from the dynamics of agent interaction. The word is often used loosely. Here it has a precise meaning: the power law is not in the rules. It is generated by the rules. No amount of analysis of the individual agent's decision problem will reveal it. It requires the dynamics.

This is the payoff of the ABM framework. Not that it is more realistic than the ODE in some vague sense. It is a language capable of expressing phenomena — emergence, path dependence, heterogeneity-driven macro structure — that the ODE's language cannot reach. The firm size distribution is the canonical social science example because the data are unambiguous, the generative mechanism is clean, and the contrast with standard models is stark.

---

#### Chapter 3 Setup Chunk

```r
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
      axis.title       = element_text(family = "Source Serif 4", size = 10),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "#FAFAF7", color = NA)
    )
)
```

---

### Chapter 4: The Possibility Space of Dynamics

#### Prose Style Notes

This is the philosophical culmination. The writing should be the most assured — the argument has been built across three chapters and can now be stated plainly. It is not aggressive. It is the calm confidence of someone who has shown their work.

The Girard point — that a paradox is not a contradiction but a phenomenon exterior to a language's expressive capacity — should be stated once, cleanly, without attribution, in terms the audience can immediately apply to everything they have seen during the day. It should arrive as a crystallization, not an introduction.

There is almost no new code. What R there is serves to illustrate abstract arguments: a bifurcation diagram, a visualization of multiple possible trajectories from the same initial condition.

#### Section 4.1: Resolution

A model does not merely approximate the world. It defines a language, and that language has an expressive boundary. A phenomenon that requires distinctions the model cannot make is not a failure of fit — it is outside the language entirely. When a modeling tradition calls something a paradox or an anomaly, it is often naming a phenomenon that is simply exterior to its dogma: real, reproducible, and inexpressible in the available formal vocabulary.

This observation has teeth. The mean-field assumption makes network topology and agent heterogeneity inexpressible. The representative agent assumption makes the firm size distribution inexpressible. A stationarity prior makes regime change and bifurcation inexpressible — not unlikely, but grammatically forbidden within the model's language. An equilibrium assumption makes path dependence inexpressible. These are not failures of fit that more data or better estimation can remedy. They are restrictions on what questions the model can ask.

Every modeling choice is a choice of resolution — temporal, spectral, expressive. The ODE, the representative agent, stationarity, equilibrium, regularization penalties, the sampling frequency of a time series — all are restrictions on the language. Each restriction makes some phenomena expressible and others not. The question a careful modeler asks is not only "does my model fit?" but "at what resolution is my model working, and is the phenomenon I care about expressible at that resolution?"

#### Section 4.2: The Regularization Unification

A Gaussian prior on regression coefficients produces ridge regression, exactly — the posterior mode under a Gaussian prior is the ridge estimator. A Laplace prior produces LASSO, exactly. The statistician who uses LASSO is placing a Laplace prior on their coefficients whether they think of it that way or not. The Bayesian and the frequentist regularizer are solving the same problem.

The connection runs further. L2 regularization is the limiting case of training on Gaussian-perturbed data. The distributionally robust optimizer who minimizes worst-case loss over a Wasserstein ball around the empirical distribution recovers regularized estimators as special cases. The Bayesian prior, the regularization penalty, and the distributional perturbation are three descriptions of the same restriction on expressive complexity.

Every modeling assumption examined in this course is a prior of this kind. The mean-field assumption is a prior that assigns all probability mass to one interaction structure. The representative agent is a prior that assigns all probability mass to one firm type. Stationarity is a prior that asserts the data-generating process lies within a particular neighborhood of the estimated distribution. Each restricts the language. Each makes some phenomena expressible and others not. The ABM critic who objects to parameter proliferation is demanding a stronger prior. The question is whether that prior is warranted.

#### Section 4.3: Temporal Resolution and Near-Decomposability

Complex adaptive systems are, to a good approximation, nearly decomposable in the sense Herbert Simon described: subsystems interact tightly at fast time scales and loosely at slow ones. The firm coordinates internally at time scales too fast for prices to clear. The economy coordinates across firms at time scales over which transaction costs can be overcome. Each level has its characteristic frequency.

A model fit at a particular temporal resolution is a low-pass filter. It recovers structure at frequencies below the resolution's Nyquist limit and makes faster structure inexpressible. A quarterly macroeconomic model is not wrong about the annual dynamics it was designed to capture. It is linguistically incapable of expressing sub-quarterly phenomena. This is not a data limitation. It is a resolution mismatch — applying a coarse-resolution language to a fine-resolution phenomenon.

Stationarity is the temporal analogue of the mean-field assumption. It asserts that the spectral structure of the data-generating process is stable across time — that the past is informative about the future in a particular, strong sense. When the system is capable of bifurcation, this prior is miscalibrated in exactly the way a Gaussian prior is miscalibrated when the true signal is sparse: the language simply does not have the words for what is about to happen.

Show this with a simple example: a logistic growth process observed only in its exponential phase. Naive extrapolation predicts unbounded growth. The ODE knows about the carrying capacity because the modeler specified it. The time series model does not know, and cannot know from the data alone in the exponential phase, that a ceiling exists. The turn is exterior to the language built from the early data.

Generalize: any system capable of bifurcation will defeat stationarity-based extrapolation at the bifurcation point. This is not a statistical point about forecast intervals. It is a structural point about the limits of inductive inference from time series in nonlinear systems.

#### Section 4.4: The Possibility Space

The ABM framework does not produce better point forecasts. It is not a superior extrapolation engine. What it offers is a map of the possibility space — the range of qualitatively distinct behaviors the system is capable of, the conditions under which each is realized, and the locations of the bifurcation points that separate them.

A policy advisor working from a point forecast is navigating with a single trajectory. A policy advisor working from a possibility space map is navigating with a chart that shows not only where the current trajectory leads but which turns exist, which conditions trigger them, and which interventions have leverage at the critical points. The map does not eliminate uncertainty. It characterizes it structurally rather than suppressing it.

This reframes what modeling is for. Not false precision — "the economy will grow at 2.3% next year" — but honest cartography: these are the regimes, these are the boundaries between them, these are the variables that govern which regime the system occupies.

#### Section 4.5: The Statistician's Checklist

Close with what this means for the practicing statistician — which is to say, for everyone in the room.

The statistician's instinct toward honest uncertainty quantification is exactly the right instinct. Confidence intervals, posterior distributions, prediction intervals — these are all ways of refusing to report false precision. The framework developed in this course extends that instinct from parameter uncertainty to structural uncertainty: uncertainty not only about the values of model parameters but about which qualitative regime the system occupies and which it will move toward.

The checklist question is this: at what resolution is this model working, and is the phenomenon I care about expressible at that resolution? It applies to any regression, any time series model, any compartmental epidemic model, any production function. The ABM is not the answer to every question it raises. It is evidence that the resolution choice is a choice, that the language restriction is a restriction, and that richer expressive resources exist when the phenomenon demands them.

The tools are harder. The answers are less clean. They are more likely to be true.

---

#### Chapter 4 Setup Chunk

```r
# Chapter 4 Setup
# Self-contained — does not depend on prior chapters.

library(tidyverse)
library(deSolve)
library(patchwork)

theme_set(
  theme_minimal(base_family = "Source Serif 4") +
    theme(
      plot.title       = element_text(family = "Raleway", face = "plain",
                                      size = 14, margin = margin(b = 8)),
      axis.title       = element_text(family = "Source Serif 4", size = 10),
      legend.position  = "bottom",
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "#FAFAF7", color = NA)
    )
)
```

---

## Slide Deck Specifications

Each slide deck mirrors the corresponding book chapter in content but is authored independently. Slides are not excerpts from the book — they are a different genre. The book argues; the slide shows.

### General Slide Principles

- One main idea per slide. No crowding.
- Equations appear large and centered, not inline with dense prose.
- Code chunks on slides: 10–15 lines maximum. Full code is in the book and on GitHub.
- Use incremental reveals (`--`) to build arguments step by step.
- Each deck opens with a title slide and a brief roadmap slide.
- Each deck closes with a summary slide and a transition to the next block.
- Speaker notes (`???`) on every substantive slide — key points and anticipated questions, not a script.
- Forbidden phrases apply to slide prose equally. Slides should sound like the book, compressed.

### Slide Deck 01: ODE Foundations
~35–45 slides, 90 minutes.

Key slides:
- Title + roadmap
- "Feedback defeats intuition" — the motivating observation
- Lotka-Volterra assumptions, built incrementally
- Parameter identification
- Phase portrait
- "The product term $\beta NP$" — the mean-field assumption named precisely, without fanfare
- SIR derivation, incremental
- $\mathcal{R}_0$ derivation from algebra
- SIR time series
- Parameter sweep over $\mathcal{R}_0$
- Summary: a language and its boundary
- Transition: "What if individuals were actually individual?"

### Slide Deck 02: ABM Contrast
~45–55 slides, 105 minutes.

Key slides:
- Title + roadmap
- "What is an agent?" — state, rules, neighborhood
- ABM Lotka-Volterra rules, verbally
- Reduction result: ABM ribbons over ODE line
- "The ODE cannot see this" — extinction events
- ABM SIR, random mixing
- "Now give them an address" — introducing network structure
- Erdős-Rényi vs scale-free network visualization (`ggraph`)
- Epidemic curves compared across topologies
- "Same $\mathcal{R}_0$. Different world."
- The reduction theorem stated
- Sparsity: nominal vs effective dimensionality
- [ANIMATION] Spatial Lotka-Volterra — no code walkthrough
- Summary
- Transition to Axtell

### Slide Deck 03: Axtell Firm Size
~40–50 slides, 165 minutes.

Key slides:
- Title + roadmap
- "A regularity that refuses to go away"
- Linear scale histogram — the dynamic range problem
- Log-log plot — the power law
- MLE fit and goodness-of-fit
- Lognormal comparison
- "What the representative agent predicts" — characteristic scale, one firm
- "The representative agent is the mean-field assumption for firms"
- Axtell's model: agent rules verbally
- Team formation logic
- Exponent robustness — not calibrated, structural
- Emergence defined precisely
- Summary
- Transition to afternoon break

### Slide Deck 04: The Possibility Space of Dynamics
~35–45 slides, 90 minutes.

Key slides:
- Title + roadmap
- "A model defines a language" — the Girard point, unnamed
- Three restrictions and what they make inexpressible
- Regularization unification: prior = penalty = perturbation
- Bayesian-LASSO identity stated
- Stationarity as a temporal prior
- Logistic growth: what extrapolation cannot see
- Bifurcation: the language cannot see the turn
- Near-decomposability and temporal resolution
- "The checklist question"
- Possibility space vs point forecast
- Closing: the statistician's instinct, extended
- Final slide: GitHub, contact

---

## GitHub README Specification

The README should:

1. Introduce the course in 2–3 sentences — what it argues, not just what it covers
2. Software requirements: R version, all packages with versions
3. Repository structure explained
4. Instructions for rendering the book and each slide deck
5. Note on the `code/` directory as the primary student take-away
6. Note that `_theme/` is a placeholder scaffold, intentionally left for customization
7. Note that `style/spectral_hands.tex` is a stylistic exemplar included for reference

---

## Package Dependencies

```r
install.packages(c(
  "tidyverse",      # core data manipulation and visualization
  "deSolve",        # ODE solvers
  "igraph",         # network generation and manipulation
  "tidygraph",      # tidy API for igraph
  "ggraph",         # network visualization with ggplot2
  "gganimate",      # animation
  "gifski",         # gif renderer for gganimate
  "poweRlaw",       # power law fitting and hypothesis testing
  "broom",          # tidy model outputs
  "patchwork",      # combining ggplot2 panels
  "xaringan",       # slide framework
  "xaringanthemer"  # xaringan theming
))
```

---

## Implementation Notes for Claude Code

1. **Read `style/spectral_hands.tex` before writing any prose.** This is not optional. The prose quality of the course book depends on it.

2. **Generate all files in the repository structure.** Do not omit any file.

3. **The course book is a complete, renderable R Markdown document.** All prose from the chapter specifications above is written in full. Placeholder text is not acceptable. The prose is the course.

4. **All R code chunks are complete and executable.** No pseudocode. No `# ... rest of implementation`. Every chunk runs.

5. **The theme scaffold is a placeholder.** Generate it completely but include a comment block at the top of `custom.css` and `xaringan-theme.R` noting that colors and font weights are placeholders for post-content refinement.

6. **Slide decks are complete Xaringan documents** — all slides, speaker notes, and incremental reveal syntax as indicated.

7. **The spatial Lotka-Volterra (Chapter 2, Section 2.5) includes a complete, runnable `gganimate` implementation.** This is the most technically demanding code in the course. Take care with the grid data structure and frame logic.

8. **Use `set.seed()` at the top of every chunk involving randomness.** Comment that the seed is for reproducibility and may be changed.

9. **All plots are production quality** — proper axis labels, informative titles, clean legends, consistent use of the chapter color palette.

10. **The `code/` directory R scripts are the most heavily commented files in the repository.** Every non-obvious line has a comment. These are what students take away.

11. **Do not invent empirical data for Chapter 3.** Use US Census Bureau SUSB data or generate synthetic data from a known power law distribution using `poweRlaw::rplcon()` as a placeholder, with a clear comment that real SUSB data should be substituted.

12. **The forbidden phrases list in the Stylistic Exemplar section applies throughout.** Scan every paragraph of prose before finalizing. If any forbidden phrase appears, rewrite the sentence from scratch.

13. **The argument of the course is stated in "The Argument of the Course" section above.** Every prose paragraph should serve that argument or be cut. The course has no filler.
