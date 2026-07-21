# Agent-Based Models for Statisticians

Materials for a one-day short course on agent-based models, mean-field models, emergence, and model resolution. The repository contains the participant course book, four slide decks, the data used in the examples, and fully reproducible R source code.

## Start Here

For the course itself, you do not need to build anything.

- Read or download the [course book (PDF)](book/abm_for_statisticians.pdf).
- Use the [course book (HTML)](book/abm_course_book.html) if you prefer a searchable version with copyable code.
- Open the slide decks in a browser:
  1. [ODE Foundations](slides/01_ode_foundations.html)
  2. [Agent-Based Contrast](slides/02_abm_contrast.html)
  3. [Firm Size and Emergence](slides/03_axtell_empirics.html)
  4. [The Possibility Space of Dynamics](slides/04_philosophical_conclusion.html)

The rendered HTML files are self-contained. After downloading or cloning the repository, they can be opened locally without an internet connection.

## Before the Course

### Minimum setup

To read the materials and participate in the discussion activities, bring a laptop with:

- a current web browser;
- a PDF reader; and
- a local copy of this repository.

Clone the repository from a terminal:

```bash
git clone https://github.com/jsschuler/statsABM.git
cd statsABM
```

If you do not use Git, select **Code → Download ZIP** on GitHub and extract the archive.

### Optional R setup

Install R and one of the following editors if you want to run or modify the examples. You only need one editor:

- [RStudio Desktop](https://posit.co/download/rstudio-desktop/)
- [Positron](https://positron.posit.co/download)
- [Visual Studio Code](https://code.visualstudio.com/Download)

The course can still be followed without executing the code.

The examples use these R packages:

```r
install.packages(c(
  "broom",
  "deSolve",
  "gganimate",
  "ggraph",
  "igraph",
  "knitr",
  "patchwork",
  "poweRlaw",
  "rmarkdown",
  "showtext",
  "tidygraph",
  "tidyverse",
  "xaringan"
))
```

Verify the core setup before the course:

```r
library(tidyverse)
library(deSolve)
library(igraph)
library(poweRlaw)

cat("R setup is ready.\n")
```

NetLogo is not required. The course book uses R translations of the referenced NetLogo models.

## Participating During the Course

Keep the HTML course book open for searching and copying code. The four activities are embedded in boxed sections in the book:

1. **Predict Before Computing** — sketch predator–prey dynamics before solving the equations.
2. **Model Autopsy** — identify which questions require state or structure absent from mean-field SIR.
3. **Calibration or Mechanism?** — distinguish mechanism evidence from calibration success.
4. **Choose the Minimum Sufficient Model** — defend the least expressive model capable of answering a scientific question.

The activities emphasize prediction, interpretation, and model choice. They do not require participants to complete a lengthy programming exercise during class.

If the repository was cloned earlier, update it before the course:

```bash
git pull
```

## Repository Guide

| Location | Contents |
|---|---|
| `book/abm_for_statisticians.pdf` | Participant-facing course book |
| `book/abm_course_book.html` | Searchable course book with copyable code |
| `book/abm_course_book.Rmd` | Complete book source |
| `slides/` | Four rendered slide decks and their R Markdown sources |
| `data/axtell/` | Data used in the firm-size examples |
| `_theme/` | Shared fonts and visual styles |

## Rebuilding the Materials

Rebuilding is optional and requires R, Pandoc, LaTeX with XeLaTeX, and Ghostscript.

Build the PDF course book:

```bash
cd book
bash build.sh
```

Render the HTML course book from the repository root:

```r
rmarkdown::render(
  "book/abm_course_book.Rmd",
  output_format = "html_document"
)
```

Render an individual slide deck:

```r
rmarkdown::render("slides/01_ode_foundations.Rmd")
```

The generated files are already committed so participants do not need the publishing toolchain.

## Getting Help

If setup fails before the course, open a GitHub issue and include:

- your operating system;
- your R version, if the problem involves R;
- the command you ran; and
- the complete error message.

Please do not include passwords, access tokens, or other private information in an issue.
