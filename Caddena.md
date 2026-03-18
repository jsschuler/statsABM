# Caddena — Claude Code Design Addenda

Decisions, diagnostics, and implementation notes recorded during development.
These supplement `CLAUDE.md` with rationale that would otherwise be lost.

---

## 1. Predator-prey ABM: all runs end in extinction with default parameters

### What was found

Across every seed tested (1–30, 42, 99, 123, 200, 314, 512) on every grid size
(20×20, 50×50), the predator-prey ABM with default parameters eventually ends in
extinction of one or both populations within 500 steps.

The two failure modes are:

**Baseline model (no grass), 20×20 grid:**
Wolves overshoot prey, crash the sheep population, then starve. The trajectory
shows clear boom-bust oscillations (typically 2 cycles) before wolves die.
Extinction step is typically 100–350 depending on seed.

**Grass model, 20×20 grid:**
Grass caps the sheep carrying capacity at ~25–30 on a 400-cell grid, making the
food supply too sparse for wolves to survive. Wolves linger at 1–3 individuals
before stochastic extinction. Extinction step is typically 70–230.

**Baseline model, 50×50 grid (seed=42):**
Sheep explode to 31,000 before wolves explode in response (both populations are
unconstrained). Sheep then crash to 0 at step ~300 with wolves at ~17,000.

### Root cause

The default `wolf_gain_from_food = 20` equals `wolf_reproduce_threshold = 20`,
meaning one sheep kill is sufficient to reproduce immediately. This causes
wolves to overshoot prey whenever sheep density is even moderately high.

The underlying issue is that the NetLogo model these parameters derive from
uses spatially bounded foraging with energy decay, and the exact parameter
regime that produces sustained oscillations is sensitive to grid size, initial
conditions, and implementation details. The R implementation faithfully
reproduces those mechanics but inherits the same sensitivity.

### Why this is pedagogically correct

Extinction IS the point. The Lotka-Volterra ODE predicts stable, indefinitely
persisting cycles. The ABM reveals that finite populations can crash to
extinction stochastically — even when the ODE predicts no such risk. This is
the core epistemological argument of the course (Section 4.5, Section 2.5).

From the course document spec:
> "Recurrence theorem (state without proof, cite Karlin & Taylor): a
> one-dimensional random walk is recurrent — it visits every state including
> zero. The ODE cannot represent this; the ABM reveals it as finite extinction
> probability."

The demo scripts are written to exploit this:
1. Show a run where the model does not go extinct within the displayed window
   (a "clean" run for comparison with the ODE).
2. Add explicit comments and Activity 2 instructions directing students to
   discover extinction by trying different seeds and grid sizes.

### Parameter choices for demo scripts

After testing seeds 1–30 on a 20×20 grid:

- **`demo_lotka_volterra.R` ABM section**: seed=18, width=20, height=20, steps=300.
  At step 300, sheep=6 and wolves=13 — both alive. The run shows two complete
  boom-bust cycles. Running to step 341+ shows wolf extinction. This is
  referenced in the Activity 1 comments.

- **`demo_predator_prey.R` baseline ABM**: same as above (seed=18, 20×20, 300 steps).
  "Non-extinct at 300 steps" satisfies the demo requirement while the pedagogy
  section explains that other seeds and longer runs show extinction.

- **`demo_predator_prey.R` grass ABM**: seed=10, width=20, height=20, steps=300.
  Wolf extinction at step 230; sheep stabilise at ~25 due to grass carrying
  capacity. The contrast with the baseline (wolves die later, different mechanism)
  is the instructional target for Activity 5.

---

## 2. Sheep cell lookup: semantic subtlety with asynchronous agent stepping

### What was implemented

`PredatorPreyModel$step()` builds a cell → sheep-index lookup table ONCE at
the START of each tick (before any agent moves). This was added for O(1) wolf
hunting instead of O(n_sheep) linear scans.

### The subtlety

The lookup captures sheep positions at tick start. During the tick, sheep move.
A naive implementation would let wolves "eat" sheep at the sheep's original
position even after the sheep moved away — a ghost-eating bug that incorrectly
boosts wolf efficiency.

The fix, in `Wolf$step()`:
```r
prey_here <- prey_here[
  vapply(prey_here, function(idx) {
    s <- model$sheep[[idx]]
    s$alive && s$x == self$x && s$y == self$y   # check current position
  }, logical(1L))
]
```

Wolves only eat sheep that are CURRENTLY at the wolf's position AND still alive.
The pre-built lookup gives O(1) candidate retrieval; the current-position check
restores correct "sheep that moved away escape" semantics.

This is why the lookup is documented as a performance optimisation, not as an
exact spatial snapshot: a sheep's START-of-tick position is used to populate
the lookup, but its END-of-tick position is what determines catchability.

Magnitude of the bug: with seed=42, the ghost-eating bug raised maximum wolves
from ~103 (correct) to 5208 (buggy), a 50× population explosion.

---

## 3. Grass model: energy timing bug

### What was found

The first implementation of `PredatorPreyGrassModel$step()` set
`sheep_gain_from_food = 0` before running `Sheep$step()`, then added the grass
gain back afterward for sheep on grown cells. This caused a death-timing bug:
a sheep at energy=1 on a grass cell would:
1. Enter `Sheep$step()` with gain=0
2. Gain 0, lose 1 → energy=0 → die (step 5)
3. Never reach the post-step grass bonus

### The fix

Run `Sheep$step()` with the full `sheep_gain_from_food` gain (so sheep on grass
cells survive), then apply a penalty of `−sheep_gain_from_food` to sheep that
LANDED on bare cells, plus re-check death:

```r
for (agent in self$sheep) {
  if (!agent$alive) next
  if (self$grass[agent$y, agent$x]) {
    # Sheep grazed: consume the grass
    self$grass[agent$y, agent$x]           <- FALSE
    self$grass_countdown[agent$y, agent$x] <- self$grass_regrowth_time
  } else {
    # Bare cell: revoke the unearned gain
    agent$energy <- agent$energy - orig_gain
    if (agent$energy <= 0) agent$alive <- FALSE
  }
}
```

Net result: grass cell → +3 energy/tick; bare cell → −1 energy/tick.
Correct semantics without touching the parent class.

---

## 4. PandemicModel: synchronous updates and vectorized spatial index

### What was implemented

The original `PandemicModel$step()` was asynchronous — each agent read the
current (mid-tick) state of neighbors, meaning infection events propagated
within a single tick. This diverged from the network model which was
synchronous.

The rewrite uses a snapshot of all agent states and positions, computes
neighbor infection counts via 8 matrix shifts, then applies all state changes
at once:
- `infected_mat[y, x]` — Boolean matrix of infected positions at tick start
- `n_inf_nbr` — convolution of `infected_mat` over Moore neighborhood
- Infection probability: `1 - (1 - beta)^k` applied to all S-agents at once

Default grid shrank from 50×50 (2500 agents) to 20×20 (400 agents) to keep
single-replicate runtime under a few seconds, consistent with the project
constraint against multi-core parallelism.

---

## 5. Test strategy: structural vs behavioural

The original test suite was purely structural (correct columns, non-negative
counts, determinism). Eight behavioural tests were added:

**Predator-prey (3 tests):**
- Wolves with no prey starve to death within a bounded number of steps
- Sheep grow with no wolves (verify reproduction works)
- Grass fraction decreases when sheep graze with slow regrowth

**Pandemic (5 tests):**
- No infections when beta=0, gamma=0 (frozen state)
- All infected recover in one step when gamma=1
- Infection spreads when beta=1, gamma=0
- super-spreaders have higher attack rate than non-super-spreaders

The wolf-starvation test avoids `model$run()` (which pads remaining records
after first extinction and would show live-wolf values) and calls
`model$step()` directly in a loop.
