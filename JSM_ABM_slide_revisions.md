# JSM 2026 ABM Course — Surgical Slide Revisions

These revisions are limited to high-value technical clarifications and small wording changes. The overall architecture, sequence, and argument remain unchanged.

---

## 1. `01_ode_foundations.html`

### A. Product-term interpretation

**Current wording**

> It says that every predator is equally likely to encounter every prey individual at every moment.  
> Every fox is, at every instant, in contact with every rabbit. Simultaneously. Uniformly.

**Replace with**

> It treats potential predator–prey partners as exchangeable conditional on the aggregate state, so the expected encounter rate factors as the product \(NP\). Space, persistent relationships, and local neighborhoods do not enter the model.

This preserves the mean-field point without implying literal simultaneous universal contact.

---

### B. Mean-field definition

**Current wording**

> The mean-field assumption: every individual in the population interacts with every other individual with equal probability at every moment.

**Replace with**

> The mean-field assumption: conditional on the aggregate state, interaction partners are exchangeable and encounters are sufficiently mixed that aggregate interaction rates depend only on population totals.

The following discussion of absent space, neighborhoods, and persistent relationships can remain.

---

### C. SIR incidence convention

Block 1 uses

\[
\frac{dS}{dt}=-\beta SI,
\qquad
\mathcal R_0=\frac{\beta N}{\gamma},
\]

while Block 2 uses frequency-dependent incidence \(\beta SI/N\).

Add beneath the Block 1 SIR equations:

> **Convention.** Here \(\beta\) is a per susceptible–infectious pair transmission coefficient, so \(\mathcal R_0=\beta N/\gamma\). In Block 2, the agent implementation uses the frequency-dependent convention \(\beta SI/N\), for which \(\mathcal R_0=\beta/\gamma\).

Alternatively, use one convention throughout.

---

### D. SIR phase-space speaker note

**Current note**

> Unlike Lotka–Volterra, SIR is not conservative in its phase space — there is no closed orbit. Trajectories spiral inward toward the disease-free equilibrium.

**Replace with**

> Unlike Lotka–Volterra, SIR has no closed orbits. The infectious population rises and then declines as susceptibles are depleted, and trajectories approach the disease-free set.

“Spiral inward” is generally not the right description for the standard SIR system.

---

## 2. `02_abm_contrast.html`

### A. Rate–probability correspondence

On the ABM Lotka–Volterra rules or code slide, add:

> The per-step probabilities are discrete-time analogues of continuous-time rates. Exact conversion depends on the timestep and update scheme; for a rate \(\lambda\), a common conversion is \(p=1-e^{-\lambda\Delta t}\).

Avoid presenting \(p_{\text{birth}}=\alpha\), \(p_{\text{kill}}=\beta\), and \(p_{\text{death}}=\gamma\) as exact identities unless \(\Delta t\) and the event construction make them exact.

---

### B. ODE–ABM relationship

**Current visible wording**

> The ODE is the ABM's expectation under the mean-field assumption.  
> The ABM is the ODE with the mean-field assumption relaxed.

**Replace with**

> Under homogeneous agents, random mixing, compatible event-rate scaling, and a large-population limit, the normalized ABM converges to the mean-field ODE over finite time horizons.  
> Relaxing those restrictions permits dynamics that the mean-field ODE does not represent.

The sentence

> An ODE is an ABM under maximal regularization.

can remain as the course’s interpretive framework, but it should be presented as a conceptual synthesis rather than a universal theorem.

---

### C. Extinction slide

**Current wording**

> The ODE's language has no word for extinction. The absorbing state at zero is outside its expressive range.

**Replace with**

> **The deterministic ODE cannot reach extinction from a positive initial condition.**  
> Zero is an invariant boundary: a population remains extinct if it starts at zero, but an interior Lotka–Volterra trajectory cannot hit zero in finite time. A finite stochastic ABM can hit zero with positive probability, after which extinction is absorbing.

This is the precise representational difference: extinction is inaccessible from the positive interior in the deterministic ODE but accessible in the finite stochastic model.

---

### D. Network comparison claim

Where the slides imply that no choice of \(\beta\) and \(\gamma\) can reproduce the network difference, use:

> Recalibrating aggregate parameters may mimic a selected epidemic curve or summary, but a single mean-field parameterization cannot generally preserve topology-dependent behavior across seed locations, initial conditions, interventions, and counterfactuals.

This avoids overstating non-equivalence while preserving the structural point.

---

### E. Erdős–Rényi versus Barabási–Albert slide

**Current wording**

> The same \(\mathcal R_0\) applies to both.

**Replace with**

> The same transmission and recovery parameters, and approximately the same mean degree, are used for both networks.

A network epidemic threshold generally depends on more than mean degree, so identical \(\mathcal R_0\) should not be asserted without defining the network-specific quantity.

---

### F. “Scale-free” wording

Where convenient, replace unqualified uses of

> scale-free network

with

> preferential-attachment network with a heavy-tailed degree distribution

or

> Barabási–Albert preferential-attachment network.

---

## 3. `03_axtell_empirics.html`

### A. Power-law claim

**Current wording**

> The distribution between these extremes is not normal. Not lognormal. Not exponential.

**Replace with**

> The distribution is strongly right-skewed and heavy-tailed. Power-law and lognormal descriptions are serious alternatives that must be compared rather than assumed.

This is consistent with the later Vuong comparison.

---

### B. “No characteristic size”

**Current wording**

> A scale designed for distributions with a characteristic size is the wrong instrument for a distribution with no characteristic size. That last phrase is the definition of a power law.

**Replace with**

> A linear scale is poorly suited to a distribution spanning several orders of magnitude. A power law is scale-free in the specific sense that rescaling preserves its functional form; this is stronger and more precise than saying merely that there is no typical observation.

---

### C. Vuong-test interpretation

The output reports:

- test statistic: \(-1.686\)
- two-sided \(p\)-value: \(0.0919\)

If the package convention is that positive values favor the power law, then the negative statistic favors the lognormal, although the comparison is not decisive at conventional levels.

**Replace the accompanying interpretation with**

> Under the package’s sign convention, the negative statistic favors the lognormal, but the two-sided test does not decisively distinguish the two models at conventional significance levels. The simulated sample therefore supports a heavy-tailed distribution while illustrating that power-law and lognormal tails may be difficult to distinguish empirically.

Also change the printed label from

> One-sided p-value

to

> Two-sided p-value

unless a genuinely one-sided value is calculated.

---

### D. Aggregate-state models

**Current wording**

> The distribution an aggregate-state model produces is bell-shaped around a characteristic scale.

**Replace with**

> A model that represents the firm population only through a single aggregate or representative state cannot itself recover the full cross-sectional firm-size distribution. Richer equilibrium models with heterogeneous firms can generate dispersion and, in some cases, heavy tails; the relevant contrast here is with models that remove the distribution from the state space.

This avoids making an unnecessarily broad claim about all equilibrium or aggregate economic models.

---

### E. Axtell result

**Current wording**

> The exponent is not a property of any equilibrium. It is a property of the path.

**Replace with**

> In Axtell’s model, the exponent is generated by decentralized entry, exit, joining, and leaving dynamics rather than imposed as a calibration target or equilibrium condition.

---

### F. Emergence definition

Where emergence is defined, use:

> A macro-level regularity is emergent in the operational sense used here when it is not imposed as a target in the micro-rules, arises from their interaction, and persists under meaningful perturbations of parameters, initial conditions, and implementation choices.

Then add:

> Emergence establishes generativity and robustness, not uniqueness of mechanism. Competing mechanisms may reproduce the same regularity.

---

## 4. `04_philosophical_conclusion.html`

### A. Restriction summary

**Current wording**

> Stationarity assumption → Regime change and bifurcation are inexpressible.  
> Equilibrium assumption → Path dependence and lock-in are inexpressible.

**Replace with**

> **Fixed-regime stationarity**  
> → Unmodeled changes in the transition law are outside the model.

> **Unique, path-independent equilibrium closure**  
> → Lock-in and history-dependent equilibrium selection are outside the model.

This names the specific restrictions responsible for the exclusions.

---

### B. “Every assumption is a prior”

**Current wording**

> Every assumption is a prior.

**Replace with**

> Every structural assumption restricts model space and can often be represented as a prior, penalty, or hard constraint once a probabilistic formulation is specified.

This preserves the unifying idea without claiming a literal Bayesian prior in every case.

---

### C. Priors and regularization

**Current wording**

> Bayesian priors and regularization penalties are the same operation.

**Replace with**

> For common likelihoods and priors, posterior-mode estimation is equivalent to penalized optimization: Gaussian priors yield ridge-type penalties and Laplace priors yield LASSO-type penalties. The broader analogy in this course is that structural assumptions likewise restrict the effective model space.

---

### D. Nominal versus effective dimensionality

**Current wording**

> A model with 50 parameters where 45 produce qualitatively identical dynamics has effective dimensionality of ~5.

**Replace with**

> A model may have many nominal parameters while its behavior of scientific interest varies mainly along a much lower-dimensional set of combinations. Effective dimensionality should therefore be studied rather than inferred from the raw parameter count.

Avoid assigning a numerical effective dimension without a defined metric.

---

### E. Stationarity slide

**Current wording**

> Time series modeling rests on stationarity.

**Replace with**

> Many classical time-series methods rely on some form of stability in the data-generating process, such as stationarity, local stationarity, or a fixed transition law.

Then replace

> Stationarity asserts that the data-generating process lies within a neighborhood of the estimated distribution.

with

> A fixed-regime model assumes that the transition law governing the forecast period remains sufficiently close to the law learned from the observed period.

---

### F. Bifurcation claim

**Current wording**

> Any system capable of bifurcation will defeat stationarity-based extrapolation at the bifurcation point.

**Replace with**

> A model estimated entirely within one regime will generally fail to anticipate a bifurcation unless the changing control parameter and the relevant nonlinear mechanism are represented in the model.

---

### G. Confidence intervals

**Current wording**

> Wider confidence intervals do not help. They are still centered on the wrong regime.

**Replace with**

> Wider intervals quantify uncertainty within the assumed regime. They do not repair omission of the mechanism that generates a different regime, although sufficiently broad predictive distributions may still cover some post-change outcomes without explaining them.

---

### H. Logistic ODE versus logistic map

Retain the comparison, but avoid saying they have the “same structure” without qualification.

**Suggested wording**

> The continuous logistic ODE and the discrete logistic map share a quadratic density-feedback term, but they are different dynamical systems. The discrete update rule can generate period doubling and chaos; the continuous one-dimensional autonomous ODE cannot.

---

### I. Historical genealogy

No rewrite is required. For delivery, mark a subset of the genealogy slides as optional so the final block preserves time for:

- the possibility-space framework;
- the distinction between fit and expressibility;
- the practical model-selection checklist;
- closing synthesis.

---

## 5. Global terminology edits

Use these terms consistently:

- **Accessible extinction from a positive interior state**, not “zero is outside the state space.”
- **Exchangeable/randomly mixed contacts**, not literal simultaneous universal contact.
- **Convergence under stated scaling conditions**, not “the ODE is the exact expectation.”
- **Frequency-dependent incidence** for \(\beta SI/N\).
- **Density-dependent incidence** for \(\beta SI\).
- **Heavy-tailed preferential-attachment network**, where stronger “scale-free” terminology is unnecessary.
- **Generative compatibility**, not unique mechanism identification.
- **Fixed-regime stationarity**, when the argument concerns unmodeled structural change.
- **Unique, path-independent equilibrium closure**, when the argument concerns excluded lock-in or history dependence.

---

## 6. Priority order if time is extremely limited

1. Fix the extinction slide.
2. Make the SIR incidence convention consistent.
3. Replace “ODE is the ABM’s expectation” with the convergence statement.
4. Correct the Vuong-test sign and p-value label.
5. Soften the universal claims about equilibrium and aggregate-state models.
6. Qualify stationarity and equilibrium in Block 4.
7. Replace literal universal-contact language with exchangeability/mixing language.
8. Mark part of the historical genealogy as optional for delivery.
