# ToDo for glueSemWB

#Fixes/Bugs

- LFGxDRT mode still builds discharged-argument lambda wrappers with the GSWB AST inside `combinePremises(...)` (`new SemFunction((SemAtom) p.getSemTerm(), temp)`). This can flatten or overwrite provenance before the LFGx AST sees it. Replace this discharge-expansion path with the LFGxDRT AST equivalent.

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
