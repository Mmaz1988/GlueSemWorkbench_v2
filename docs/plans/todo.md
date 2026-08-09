# ToDo for glueSemWB

#Fixes/Bugs

- LFGxDRT mode still builds discharged-argument lambda wrappers with the GSWB AST inside `combinePremises(...)` (`new SemFunction((SemAtom) p.getSemTerm(), temp)`). This can flatten or overwrite provenance before the LFGx AST sees it. Replace this discharge-expansion path with the LFGxDRT AST equivalent.
  **Verified 2026-08-09: only half-fixed.** Each of `LLProver1/2/3.java` has
  two `combinePremises` overloads. The `(functor, argument, proofBuilder)`
  overload was fixed — it now branches on `Settings.LFGXDRT` and calls
  `wrapLfgAbstractionBody(...)` (`LLProver1.java:1309-1310`,
  `LLProver2.java:453-454`, `LLProver3.java:1215-1216`). The
  `(Premise, Premise)` overload without a `proofBuilder` still unconditionally
  builds the GSWB `SemFunction` wrapper (`LLProver1.java:1538-1539`,
  `LLProver2.java:685`, `LLProver3.java:1427`) — **and this overload is not
  dead code**: `prover/categoryGraph/History.calculateSolutions()` calls
  `ensureLfgxDrtSemantic(p)` and then this exact overload, so the bug is still
  live on the LFGxDRT solution-calculation path.

#Additional functions:

- add more lexicon items:
    - more modifiers
    - non-quantifying determiners
    - etc...

- add anaphors, tense/aspect etc.


# Structure/modularization

- why are we using streams for bound variables (e.g in the constructor of LLFormula)


# Implementing Lev's (2007) system:
- create category graph (done) including indices
- detect SCCs (Kosaraju algorithm, can be found online)
- create histories (Premises with indices of parent premises)
- use histories to determine order of combination
- implement optimizations
