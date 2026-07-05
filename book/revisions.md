# Revision Notes for *Agent-Based Modeling for Statisticians*  
## Pass 4: Final Copyediting and Consistency Fixes

Source: revised manuscript, *Agent-Based Modeling for Statisticians: A Short Course*. :contentReference[oaicite:0]{index=0}

## Overall Assessment

This revision is substantially cleaner. The PDF text extraction problem is fixed, the notation is mostly consistent, the \(1/(n-1)\) normalization has been incorporated, and Chapter 4 now leads more often with state variables, transition rules, dependence structures, and model-family language before returning to the language metaphor.

The manuscript now reads like a serious JSM short-course handout rather than an AI-assisted philosophical thunderstorm wearing a statistics badge. Progress, somehow.

The remaining issues are small but worth fixing.

---

## 1. Replace Remaining “Parametric Family” Language

### Issue

Chapter 1 now correctly uses:

    structural model-family choice

But Chapter 4 still says:

    The first is the parametric family — the structural commitment that determines what phenomena are in scope.

This is not disastrous, but it reintroduces the older wording. For a statistics audience, “parametric family” may sound narrower than the intended claim. The point is structural representation, not merely parametric specification.

### Suggested Revision

Replace:

    The first is the parametric family — the structural commitment that determines what phenomena are in scope.

with:

    The first is the model family — the structural commitment that determines what phenomena are in scope.

### Alternative

    The first is the structural model family — the commitment that determines what state variables, transition rules, and dependence structures are available.

### Rationale

This makes Chapter 4 consistent with the revised Chapter 1 and better matches the manuscript’s central argument. No need to let “parametric family” sneak back in wearing a fake mustache.

---

## 2. Smooth the “Richer Language / Sublanguage” Sentence

### Issue

Section 2.4 currently says something like:

    System dynamics and agent-based models are a richer language and one of its sublanguages, related by two named restrictions rather than by a vague appeal to complexity or realism.

The idea is good, but the grammar is wobbly. It is not immediately clear which thing is the richer language and which thing is the sublanguage.

### Suggested Revision

Use:

    The ABM is the richer language; the ODE is the sublanguage recovered under homogeneity and mean-field contact. They are related by named restrictions rather than by a vague appeal to complexity or realism.

### Alternative

    Agent-based and system-dynamics models can be related as a richer language and one of its sublanguages: the ODE is recovered when agent heterogeneity and structured contact are regularized away.

### Rationale

The first option is sharper and clearer. It also lands the reduction principle more directly. Fewer grammar fumes for the reader to inhale.

---

## 3. Change “All Data Is” to “All Data Are”

### Issue

Section 4.3 begins:

    All data is time-indexed.

This will annoy some statisticians and copyeditors. “Data” can be treated as a mass noun in modern usage, but in a formal statistics handout, “data are” is safer.

### Suggested Revision

Replace:

    All data is time-indexed.

with:

    All data are time-indexed.

### Rationale

This is small, but it avoids a predictable irritation. Statisticians have few joys; correcting “data is” is one of them. Do not feed them.

---

## 4. Standardize “Barabási-Albert”

### Issue

The manuscript uses both:

    Barabási-Albert

and:

    Barabasi-Albert

The accented form is preferable when referring to the named model.

### Suggested Revision

Search for:

    Barabasi-Albert

and replace with:

    Barabási-Albert

### Places to Check

- Section 2.3.2 text
- Figure 2.4 caption
- Figure 2.6 caption
- Any code-generated plot titles
- Code appendix comments or captions

### Rationale

This is a small consistency issue, but the manuscript is now polished enough that small inconsistencies stand out. Like wearing one sneaker to a dissertation defense.

---

## 5. Check Remaining \(1/n\) References

### Issue

Most of the \(1/n\) notation has been corrected to \(1/(n-1)\) under the no-self-contact, row-normalized convention. Still, a final search should verify that no old \(1/n\) references remain where they should be \(1/(n-1)\).

### Search Terms

Search for:

    1/n

and check whether each instance is:

1. A valid asymptotic simplification.
2. A general normalization unrelated to the contact matrix.
3. An outdated mean-field contact weight that should be \(1/(n-1)\).

### Suggested Rule

For the contact matrix with no self-contact and row sums normalized to one:

    W̄_ij = 1/(n - 1), i ≠ j
    W̄_ii = 0

Use \(1/n\) only if the text explicitly allows self-contact, uses a different normalization, or states that it is using an asymptotic approximation.

### Rationale

Normalization inconsistencies are the kind of tiny thing readers notice when they are trying to decide whether to trust the larger argument. Yes, this is unfair. Welcome to math-adjacent prose.

---

## 6. Check Remaining “Language” Metaphor Density

### Issue

Chapter 4 is much improved because it now pairs language/vocabulary claims with state-space and transition-rule language. Still, the metaphor appears often enough that one more pass would help.

### Keep the Metaphor When It Marks a Major Point

Good uses:

    A model does not merely approximate the world. It defines a language, and that language has an expressive boundary.

    The right question is not “can I fit this better?” but “does this model’s structure contain the phenomenon I need to reason about?”

These are strong and should remain.

### Prefer Technical Framing Inside Dense Sections

Instead of:

    the model has no vocabulary for topology

prefer:

    the model has no state variable or parameter through which topology can enter

Then, if useful:

    In the model’s terms, topology has no place to enter.

### Rationale

The metaphor is central, but it works best after the formal point has landed. Let state space, transition rules, and dependence structures do the heavy lifting. Let the metaphor arrive afterward with a little cape.

---

## 7. Preserve the Strongest Current Changes

These latest changes are strong and should remain.

### Keep: PDF Text Extraction Fix

Words like `Differential`, `affect`, `offspring`, and `firm` now extract correctly. This matters for searchability, accessibility, and professional polish.

### Keep: Structural Model-Family Language

    The mean-field assumption is a structural model-family choice.

This is exactly the right wording.

### Keep: Nyquist Softening

    This is an identifiability limit analogous to the sampling limits formalized by Nyquist (1928), not merely a data scarcity problem.

This is much safer than the earlier version.

### Keep: Chapter 4 Technical Reframing

    It requires a state variable, transition rule, or dependence structure the model does not contain.

This is excellent. It gives the philosophical claim a precise modeling interpretation.

### Keep: Regularization Path Softening

    From this perspective, they can be viewed as members of a common modeling family indexed by regularization strength.

This is much more defensible than saying they are simply the same model.

---

## 8. Final Priority Checklist

Highest priority:

1. Replace the remaining “parametric family” phrase in Chapter 4.
2. Rewrite the “richer language / sublanguage” sentence in Section 2.4.
3. Change “All data is time-indexed” to “All data are time-indexed.”
4. Standardize all instances of “Barabási-Albert.”
5. Search for remaining \(1/n\) contact-matrix references.

Medium priority:

6. Do one final pass for dense clusters of language/vocabulary/grammar metaphors.
7. Check figure captions for consistency with the body text.
8. Confirm all code appendix notation matches the main text.
9. Search for “sufficiency” and “sufficient” to ensure none are being used loosely.
10. Search for “parametric” to ensure it appears only where intended.

Lower priority:

11. Consider adding a very short preface note explaining that Chapter 2.5 and the code appendix are optional/self-study for the live JSM session.
12. Consider adding a one-paragraph “How to use this handout” note for short-course participants.

---

## Suggested Minimal Patch List

For a fast final revision, make these exact edits:

1. Replace:

       The first is the parametric family

   with:

       The first is the model family

2. Replace the Section 2.4 sentence about system dynamics and ABMs with:

       The ABM is the richer language; the ODE is the sublanguage recovered under homogeneity and mean-field contact. They are related by named restrictions rather than by a vague appeal to complexity or realism.

3. Replace:

       All data is time-indexed.

   with:

       All data are time-indexed.

4. Replace all instances of:

       Barabasi-Albert

   with:

       Barabási-Albert

5. Search for all instances of:

       1/n

   and verify whether each should be:

       1/(n - 1)

6. Search for:

       sufficient
       sufficiency
       parametric
       grammar
       vocabulary
       language

   and inspect whether each use is intentional and precise.

---

## Bottom Line

This draft is close. The manuscript now has the right balance: technical enough for statisticians, philosophical enough to be distinctive, and careful enough not to invite easy attacks from the little reviewer goblins who live inside notation.

The remaining work is final copyediting and consistency checking. The big argument is there. The handout now looks like it belongs at JSM, which is irritating because that means this whole project might actually work.