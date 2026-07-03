# Task: Add Missing Scholarly References to Agent-Based Modeling Course Notes

You are working in a LaTeX project for the manuscript:

> Agent-Based Modeling for Statisticians
> John S. Schuler

Your goal is to make the manuscript publication-quality by inserting missing citations and building a complete BibTeX database.

## Requirements

### 1. Do NOT rewrite prose

Do not substantially edit the writing.

Only:

- insert `\cite{...}` or `\citep{...}` as appropriate
- add explanatory citations where a claim currently lacks attribution
- correct obvious attribution errors
- create a `.bib` file

Do not otherwise change wording unless absolutely necessary.

---

### 2. Prefer original sources

Whenever possible cite the foundational work rather than textbooks.

Examples:

Lotka-Volterra
- Lotka (1925)
- Volterra (1926)

SIR
- Kermack & McKendrick (1927)

Power laws
- Clauset, Shalizi & Newman (2009)
- Newman (2005)

Firm size
- Axtell (2001)
- Gibrat (1931) where historically appropriate
- Simon & Bonini (1958) where appropriate

Networks
- Erdős & Rényi (1959, 1960)
- Barabási & Albert (1999)

Complex systems
- Simon (1962)
- Holland (1992)
- Epstein & Axtell (1996)

Agent-based modeling
- Grimm et al. (2005)
- Railsback & Grimm
- Gilbert

Mean field
- standard statistical physics references if appropriate

Maximum entropy
- Jaynes (1957)

Regularization
- Hoerl & Kennard (1970)
- Tibshirani (1996)

Bayesian interpretation of regularization
- appropriate original sources

Mean-field games
- Lasry & Lions
- Huang, Caines & Malhamé

Distributionally robust optimization
- appropriate foundational papers

Nyquist theorem
- Nyquist (1928)
- Shannon (1949) only when discussing sampling theorem.

---

### 3. Every substantive factual claim should be supported

Look for places discussing

- historical origin
- theorem
- empirical regularity
- biological fact
- economics
- statistics
- machine learning
- network science

and add citations.

Avoid citation spam.

Generally one citation per paragraph is sufficient unless multiple distinct ideas appear.

---

### 4. Check every chapter

Specifically inspect

## Chapter 1

Lotka
Volterra
mean field
SIR
R0
herd immunity
continuous ODE assumptions

## Chapter 2

agent-based models
network epidemics
Erdős-Rényi
Barabási-Albert
spatial ABMs
reduction arguments
extinction in stochastic populations

## Chapter 3

Zipf law
firm-size distributions
Axtell
power-law estimation
representative-agent criticism
emergence

## Chapter 4

Simon
regularization
ridge
lasso
Bayesian interpretation
maximum entropy
distributionally robust optimization
mean-field games
near decomposability
Nyquist
stationarity

---

### 5. Verify mathematical claims

Some statements are stronger than standard literature.

For example

- "ODE is the ABM under regularization"
- "Reduction theorem"
- "maximum entropy interpretation"
- "Bayesian identity"
- "convexity"

For these:

- determine whether literature directly supports them
- if not, cite the closest supporting literature
- if they appear to be the author's own synthesis, leave them uncited rather than inventing support

Do NOT fabricate precedent.

---

### 6. Produce a high-quality BibTeX database

Create

```
references.bib
```

Requirements:

- valid BibTeX
- DOI whenever available
- journal
- pages
- volume
- publisher
- ISBN for books if available
- URLs only if DOI unavailable

Use stable citation keys like

```
Lotka1925
Volterra1926
Kermack1927
Simon1962
Axtell2001
Barabasi1999
Clauset2009
Jaynes1957
Hoerl1970
Tibshirani1996
Lasry2007
```

---

### 7. Produce a citation report

Create

```
CITATION_REPORT.md
```

containing

# Added citations by chapter

For each chapter list

- paragraph
- claim
- citation added
- reason

Also include

## Possible unsupported claims

List statements that appear to be original contributions of this manuscript and therefore should probably remain uncited.

---

### 8. Be conservative

Never invent references.

If uncertain, leave a TODO comment.

Prefer fewer correct citations over many weak ones.

---

### 9. Final output

Return

- modified tex files
- references.bib
- CITATION_REPORT.md

with no other substantive changes.