# ToDo for glueSemWB

#Fixes/Bugs

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
