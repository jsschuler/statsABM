# abmcourse: Agent-Based Modeling for Statisticians

This repository contains the R package and course materials for a one-day short
course on agent-based modeling (ABM) aimed at practicing statisticians. The course
builds each model twice — once as a system of ODEs (system dynamics) and once as an
agent-based model — so that students can compare the two paradigms directly. Models
cover predator-prey dynamics (Lotka-Volterra), epidemic spread (ISR compartmental
model), social-network epidemics with super-spreaders, predator-prey with a grass
layer, and firm-size distributions (Axtell).

## Installation

From a local clone:

```r
# Install dependencies first
install.packages(c("R6", "deSolve", "igraph", "ggplot2", "patchwork",
                   "dplyr", "tidyr", "poweRlaw", "cli", "devtools"))

# Install the package
devtools::install()
```

Or directly from GitHub (replace with the actual repository URL):

```r
devtools::install_github("your-org/abmcourse")
```

## Running a demo

Each demo is a self-contained R script. Run any of them from the package root:

```bash
Rscript demo/demo_lotka_volterra.R
Rscript demo/demo_isr.R
Rscript demo/demo_predator_prey.R
Rscript demo/demo_pandemic.R
Rscript demo/demo_axtell.R
```

Figures are saved to `figures/` (created automatically if it does not exist) as
`.png` files at 150 dpi.

## Course activities

Each demo script contains a clearly marked activity block:

```r
## ---- ACTIVITY N: Title -----------------------------------------------------
## Instructions: see the course document, Section X.Y.
```

Search any demo script for `ACTIVITY` to jump to the student starting point. Full
instructions are in the course document (`vignettes/abm_for_statisticians.pdf`).

## Compiling the course document and slide deck

The course document and slides are LaTeX/knitr `.Rnw` files compiled independently
from the package. From the package root:

```bash
# Course document
Rscript -e "knitr::knit('vignettes/abm_for_statisticians.Rnw')"
pdflatex abm_for_statisticians.tex
pdflatex abm_for_statisticians.tex   # second pass for references
mv abm_for_statisticians.* vignettes/

# Slide deck
Rscript -e "knitr::knit('vignettes/abm_slides.Rnw')"
pdflatex abm_slides.tex
pdflatex abm_slides.tex
mv abm_slides.* vignettes/
```

Both documents share a single `references.bib` and call the same package functions,
so figures are visually consistent across the two outputs. The slide deck uses a dark
navy theme; the course document uses a white theme.

## Axtell firm-size data

Rob Axtell's original firm-size simulation data files are **not included** in this
repository and must be obtained directly from the author. The package includes US
Census Bureau Statistics of US Businesses (SUSB) data as a stand-in
(`data/census_firm_sizes.rda`), which displays the same power-law structure.

To regenerate the Census data from source:

```bash
Rscript data-raw/census_firm_sizes.R
```

## Package structure

```
R/               Package source: SD models, ABM models, plotting/stats utilities
chunks/          External knitr chunk files (one per module, light + dark variants)
demo/            Standalone demo scripts with student activity blocks
vignettes/       Course document and slide deck (.Rnw) plus references.bib
data/            Cleaned Census firm-size data (census_firm_sizes.rda)
data-raw/        Script to fetch/clean the Census data
tests/           testthat unit tests
```
