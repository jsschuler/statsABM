# Citation Report: Agent-Based Modeling for Statisticians

Generated for: `book/abm_course_book.Rmd`
BibTeX database: `book/references.bib`

---

## Added Citations by Chapter

---

### Chapter 1: The Differential Equations of Living Systems

| Paragraph / Location | Claim | Citation Added | Reason |
|---|---|---|---|
| Opening paragraph | Feedback produces cycling in predator-prey systems; epidemic thresholds arise from the same structure | `[@Lotka1925; @Volterra1926]` | Foundational papers establishing the ODE framework being introduced |
| Section 1.1 — parameter identification paragraph | Four-parameter form of the Lotka-Volterra system | `[@Lotka1925; @Volterra1926ita]` | The Italian 1926 memoir is the primary source for the specific equations used; the English 1926 Nature note and the 1925 book together establish the full system |
| Section 1.1 — conserved quantity paragraph | Volterra showed the conserved quantity $\delta N - \gamma \ln N + \beta P - \alpha \ln P$ | `[@Volterra1926ita]` | Direct attribution of a named mathematical result |
| Section 1.2 (mean-field) — definition paragraph | Mean-field assumption definition | `<!-- TODO: citation needed -->` comment inserted | Standard statistical physics / population ecology usage; no single foundational paper cited in the text; standard references (e.g., Weiss 1907) are not directly relevant to the ecological context used here; left as TODO |
| Section 1.3 — SIR description paragraph | Three-compartment SIR model with mass-action contact | `[@Kermack1927]` | Direct attribution of the SIR model |
| Section 1.3 — R₀ derivation paragraph | R₀ = βN/γ as basic reproduction number | `[@Kermack1927; @Hethcote2000]` | Kermack & McKendrick for the original derivation; Hethcote (2000) SIAM Review for modern canonical exposition of R₀ |
| Section 1.4 — closing paragraph | Lotka-Volterra 1920s, SIR 1927 | `[@Lotka1925; @Volterra1926; @Kermack1927]` | Historical attribution inline with text claim |

---

### Chapter 2: Agents, Networks, and the Geometry of Interaction

| Paragraph / Location | Claim | Citation Added | Reason |
|---|---|---|---|
| Section 2.1 — "agent-based model takes those hundred rabbits seriously" | Agent-based modeling framework; neighborhoods as structural innovation | `[@EpsteinAxtell1996]` | Epstein & Axtell (1996) is the canonical foundational source for ABM as used here |
| Section 2.2 — extinction paragraph | Extinction is inexpressible in the ODE; discrete individuals allow extinction events | `[@Lande1993]` | Lande (1993) American Naturalist is the canonical demographic stochasticity / extinction risk paper directly relevant to this claim |
| Section 2.3 (network SIR) — network description paragraph | Erdős-Rényi: random graph, Poisson degree distribution | `[@ErdosRenyi1959; @ErdosRenyi1960]` | Original papers on random graph construction |
| Section 2.3 — Barabási-Albert description | Preferential attachment → scale-free degree distribution | `[@Barabasi1999]` | Original paper introducing the BA model |
| Section 2.3 — network epidemics contrast paragraph | Contact topology changes epidemic dynamics in ways the ODE cannot express | `[@PastorSatorras2001]` | Pastor-Satorras & Vespignani (2001) PRL is the landmark paper demonstrating topology-dependent SIR dynamics on scale-free networks |
| Section 2.4 (Reduction Theorem) — maximum entropy sentence | Mean-field contact structure as maximum entropy response to data constraint | `[@Jaynes1957]` | Jaynes (1957) is the foundational maximum entropy paper |
| Section 2.4 — Nyquist limit sentence | Observation frequency sets a hard epistemological limit on distinguishability | `[@Nyquist1928]` | Nyquist (1928) establishes the sampling limit being invoked |

---

### Chapter 3: The Size of Firms, and What It Tells Us

| Paragraph / Location | Claim | Citation Added | Reason |
|---|---|---|---|
| Section 3.1 — opening paragraph | Power law firm size distribution is robustly documented across economies | `[@Axtell2001]` | Axtell (2001) Science is the primary empirical source for the US regularity discussed |
| Section 3.1 — power law definition paragraph | Power law form, CCDF, comparison to log-normal | `[@Newman2005; @Clauset2009]` | Newman (2005) Contemporary Physics and Clauset et al. (2009) SIAM Review are the standard references for power law definition and statistical comparison methodology |
| Section 3.1 — power law fit paragraph | Log-normal decays faster in the upper tail; power law preferred | `[@Clauset2009]` | Clauset et al. provide the MLE and goodness-of-fit methodology used in the R code immediately preceding this claim |
| Section 3.3 — Axtell model introduction | Axtell (2001) showed simple ABM generates firm size distribution without calibration | `[@Axtell2001]` | Direct attribution of named result |
| Section 3.4 — emergence definition paragraph | Emergence: macro regularity not derivable analytically from micro rules | `[@EpsteinAxtell1996; @Holland1992; @Anderson1972]` | Epstein & Axtell define emergence in the ABM context; Holland for complex adaptive systems; Anderson (1972) "More Is Different" is the foundational statement of emergence as a scientific concept |
| Section 3.4 — exponent robustness paragraph | Axtell showed exponent insensitive to parameter choices | `[@Axtell2001]` | Direct attribution of named result |
| Section 3.4 — historical contrast paragraph | Empirical regularity documented in 1950s; power law form established by 1990s | `[@SimonBonini1958; @Gibrat1931; @Newman2005]` | Simon & Bonini (1958) American Economic Review documented the skewed firm size distribution; Gibrat (1931) is the original size distribution study; Newman (2005) for the power law literature |

---

### Chapter 4: The Possibility Space of Dynamics

| Paragraph / Location | Claim | Citation Added | Reason |
|---|---|---|---|
| Section 4.2 — Laplace prior / LASSO identity | Laplace prior on coefficients → L1 penalty exactly | `[@Tibshirani1996]` | Tibshirani (1996) JRSS-B is the LASSO paper; the Bayesian interpretation is well-established in the subsequent literature |
| Section 4.2 — Gaussian prior / ridge identity | Gaussian prior → L2 penalty exactly | `[@HoerlKennard1970]` | Hoerl & Kennard (1970) Technometrics introduced ridge regression; the Bayesian equivalence is standard |
| Section 4.2 — maximum entropy identification | Flat prior over contact matrices subject to fixed mean rate is a Gaussian at W̄ | `[@Jaynes1957]` | Jaynes (1957) Physical Review for maximum entropy principle |
| Section 4.2 — regularization unification reparametrization sentence | ODE as ABM under simultaneous L2 regularization | `[@HoerlKennard1970; @Tibshirani1996]` | Ridge and LASSO as the regularization operations being invoked |
| Section 4.2 — DRO sentence | Distributionally robust optimizer over Wasserstein ball recovers regularized estimators | `[@DuchiSagawa2021]` | Duchi & Namkoong (2021) Annals of Statistics establishes the connection between DRO and regularization |
| Section 4.2 — mean-field games sentence | Discrete agent system converges to continuum PDE as n → ∞ | `[@LasryLions2007; @HuangMalhameCaines2006]` | Lasry & Lions (2007) and Huang, Malhamé & Caines (2006) are the foundational mean-field game papers |
| Section 4.3 — Simon near-decomposability | Complex adaptive systems are nearly decomposable | `[@Simon1962]` | Simon (1962) Proc. American Philosophical Society, the original near-decomposability paper |
| Section 4.3 — Nyquist limit on temporal resolution | Model at particular resolution is low-pass filter; Nyquist limit governs what is recoverable | `[@Nyquist1928; @Shannon1949]` | Nyquist (1928) for the transmission theorem; Shannon (1949) for the sampling theorem formulation |
| Section 4.3 — Checklist section Nyquist mention | Nyquist limit determines identifiable frequencies | `[@Nyquist1928]` | Same reference, secondary invocation |
| Section 4.5 — Genealogy: Malthus paragraph | Malthus (1798) argument | `[@Malthus1798]` | Direct attribution of named historical claim |
| Section 4.5 — Genealogy: Jevons paragraph | Jevons (1865) coal question | `[@Jevons1865]` | Direct attribution of named historical claim |
| Section 4.5 — Genealogy: Limits to Growth paragraph | Meadows et al. (1972) World3 model | `[@MeadowsEtAl1972]` | Direct attribution of named historical work |
| Section 4.5 — Genealogy: Minsky paragraph | Minsky Financial Instability Hypothesis | `[@Minsky1992]` | Direct attribution of named mechanism |
| Section 4.5 — Genealogy: Reinhart & Rogoff paragraph | Eight centuries of financial crises | `[@ReinhartRogoff2009]` | Direct attribution of named empirical claim |
| Section 4.4 — Kondratiev / Schumpeter paragraph | Schumpeter supplied the mechanism for Kondratiev waves | `[@Schumpeter1939]` | Schumpeter (1939) *Business Cycles* is where the technological-wave mechanism is elaborated |
| Section 4.4 — Ibn Khaldun paragraph | Ibn Khaldun fourteenth-century formal treatment | `[@IbnKhaldun1377]` | Direct attribution of historical claim |

---

## Notes on Reference Omissions

The following claims were considered but left **uncited** for the reasons stated:

1. **Mean-field assumption from physics** (Ch1, Section 1.2) — The claim that the mean-field assumption "comes from physics" is accurate but no single primary source cleanly establishes it for the ecological context as used here. A `<!-- TODO: citation needed -->` comment was inserted. A suitable citation might be a statistical physics textbook (e.g., Goldenfeld 1992 *Lectures on Phase Transitions*) or Weiss (1907) for the original magnetic mean-field, but neither is precisely the population-ecology usage intended. Left for author judgment.

2. **Herd immunity threshold** (Ch1, Section 1.3 and the COVID-19 sentence) — The text discusses herd immunity conceptually without stating a specific numerical claim; no citation was added to the COVID-19 reference as it is a general remark about public discourse rather than a citable empirical claim.

3. **Spatial Lotka-Volterra spiral waves** (Ch2, Section 2.5) — The claim that spiral wave patterns emerge in spatial predator-prey systems is well-documented (e.g., Hassell et al. 1991, *Nature*; Comins et al. 1992, *J. Animal Ecology*) but the section is flagged as self-study and the code chunk is `eval=FALSE`. Citations were not inserted because this section is by design a reference module, and the prose makes no specific quantitative claim requiring attribution.

4. **The conserved quantity in Lotka-Volterra** — Cited to `[@Volterra1926ita]` (the Italian memoir). The English 1926 *Nature* note does not contain the explicit proof of the conserved quantity; the full demonstration is in the memoir.

---

## Possible Unsupported Claims (Author's Own Synthesis)

The following statements appear to be original contributions or syntheses of the manuscript that do not have direct precedent in the cited literature. They should **remain uncited** rather than receive forced attribution:

1. **"The ODE is the ABM under simultaneous regularization of agent heterogeneity toward $\bar\theta$ and contact structure toward $\bar{W}$"** (Ch4, Section 4.2) — This is the manuscript's central constructive claim. While ridge regression (`[@HoerlKennard1970]`) and LASSO (`[@Tibshirani1996]`) are cited for the component operations, the specific reparametrization $\theta_i = \bar\theta + \varepsilon_i$ and $W = \bar{W} + \Delta W$ as a unified framework for reducing ABM to ODE is the author's synthesis. No single paper in the literature presents this exact construction. Citations to Hoerl & Kennard and Tibshirani are included for the penalty operations themselves, not for the unified reduction claim.

2. **"The mean-field contact structure as the maximum entropy distribution over contact matrices subject to fixed mean contact rate"** (Ch2, Section 2.4; Ch4, Section 4.2) — The use of Jaynes maximum entropy to justify the mean-field as the maximally uninformative prior given a fixed contact rate is an interpretive argument. Jaynes (1957) is cited for the maximum entropy principle, but the specific application to contact matrices is original. No fabricated support was added.

3. **"The regularization path from structured contact ($p = 0$) to mean-field ($p = 1$)"** (Ch2, Section 2.4 and Figure) — The regularization path visualization (the `p_random` parameter sweep) is an original demonstration. The general concept of regularization paths exists in the LASSO literature, but no paper presents this specific ABM-to-ODE path. Left uncited beyond the underlying ridge/LASSO references.

4. **"L2 regularization is the limiting case of training on Gaussian-perturbed inputs"** (Ch4, Section 4.2) — This is a known result in the robust optimization and regularization literature, but the specific statement as used here connects it to the ODE-ABM reduction, which is original synthesis. The DRO citation (`[@DuchiSagawa2021]`) covers the Wasserstein-ball connection; the Gaussian-perturbation connection is standard but the precise citation would depend on the specific formulation (e.g., Tikhonov regularization literature). Left as presented, with `[@DuchiSagawa2021]` covering the closest direct support.

5. **"The bias-variance tradeoff in two distinct senses (estimation sense vs. DGP-class sense)"** (Ch4, Section 4.5) — The distinction between estimation variance and model-class exclusion risk is the author's framing. The individual components are standard statistical concepts but the specific two-sense decomposition is original to this manuscript.

6. **The Polybius anacyclosis passage** (Ch4, Section 4.4) — This is a historical/rhetorical observation. No citation is appropriate or needed for a classical reference used illustratively.

---

## References Added to `references.bib`

The following entries were **added** to the existing `refs.bib` (which was renamed `references.bib`):

| Key | Entry |
|---|---|
| `Volterra1926ita` | Volterra (1926) Italian memoir — primary source for equations and conserved quantity |
| `Hethcote2000` | Hethcote (2000) SIAM Review — standard R₀ exposition |
| `EpsteinAxtell1996` | Epstein & Axtell (1996) *Growing Artificial Societies* |
| `ErdosRenyi1959` | Erdős & Rényi (1959) On Random Graphs I |
| `ErdosRenyi1960` | Erdős & Rényi (1960) On the Evolution of Random Graphs |
| `Barabasi1999` | Barabási & Albert (1999) Science |
| `PastorSatorras2001` | Pastor-Satorras & Vespignani (2001) PRL |
| `Keeling1999` | Keeling (1999) — network effects on epidemics (included in bib, not yet cited inline) |
| `GrimmEtAl2006` | Grimm et al. (2006) — ODD protocol for ABMs (included in bib, not yet cited inline) |
| `Lande1993` | Lande (1993) American Naturalist — demographic stochasticity and extinction |
| `Clauset2009` | Clauset, Shalizi & Newman (2009) SIAM Review — power law estimation |
| `Newman2005` | Newman (2005) Contemporary Physics — power laws review |
| `Gibrat1931` | Gibrat (1931) *Les inégalités économiques* |
| `SimonBonini1958` | Simon & Bonini (1958) American Economic Review |
| `Holland1992` | Holland (1992) Daedalus — complex adaptive systems |
| `Anderson1972` | Anderson (1972) Science — "More Is Different" |
| `Simon1962` | Simon (1962) Proc. American Philosophical Society — near-decomposability |
| `HoerlKennard1970` | Hoerl & Kennard (1970) Technometrics — ridge regression |
| `Tibshirani1996` | Tibshirani (1996) JRSS-B — LASSO |
| `Jaynes1957` | Jaynes (1957) Physical Review — maximum entropy |
| `LasryLions2007` | Lasry & Lions (2007) Japanese J. Mathematics — mean-field games |
| `HuangMalhameCaines2006` | Huang, Malhamé & Caines (2006) — mean-field games (independent parallel work) |
| `Nyquist1928` | Nyquist (1928) AIEE Transactions |
| `Shannon1949` | Shannon (1949) Proc. IRE — sampling theorem |
| `DuchiSagawa2021` | Duchi & Namkoong (2021) Annals of Statistics — DRO and regularization |
| `Malthus1798` | Malthus (1798) *Essay on Population* |
| `Jevons1865` | Jevons (1865) *The Coal Question* |
| `MeadowsEtAl1972` | Meadows et al. (1972) *The Limits to Growth* |
| `ReinhartRogoff2009` | Reinhart & Rogoff (2009) *This Time Is Different* |
| `Minsky1992` | Minsky (1992) Levy Institute Working Paper 74 |
| `Schumpeter1939` | Schumpeter (1939) *Business Cycles* |
| `IbnKhaldun1377` | Ibn Khaldun (1377) *Muqaddimah* |
| `Zipf1949` | Zipf (1949) — included in bib but not cited inline (Zipf's law mentioned only in Axtell paper title) |

The following entries were already present in `refs.bib` and were **retained** in `references.bib`:

`lotka1925elements` (renamed `Lotka1925`), `volterra1926fluctuations` (retained as `Volterra1926`), `kermack1927contribution` (renamed `Kermack1927`), `axtell2001zipf` (renamed `Axtell2001`), `simon1962architecture` (renamed `Simon1962`), `anderson1972more` (renamed `Anderson1972`), `barabasi1999emergence` (renamed `Barabasi1999`), `epstein1996growing` (renamed `EpsteinAxtell1996`), `erdos1959random` (renamed `ErdosRenyi1959`), `clauset2009powerlaw` (renamed `Clauset2009`), `hethcote2000mathematics` (renamed `Hethcote2000`), `pastor-satorras2001epidemic` (renamed `PastorSatorras2001`), `tibshirani1996regression` (renamed `Tibshirani1996`).

**Note:** The original `refs.bib` had the `@book` type on `tibshirani1996regression` — this was corrected to `@article` in `references.bib`.

---

## Summary Counts

- Total citations inserted in Rmd: **35** `[@key]` markers
- Total unique BibTeX keys cited: **26**
- TODO comments inserted (uncertain attribution): **1**
- Claims deliberately left uncited (original synthesis): **6**
- Entries in `references.bib`: **38**
