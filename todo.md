# ToDO for glueSemWB

- Change scope handling from Determiner class to Verb class or somewhere it makes sense...

#Pre-release TODO:
- write README containing basic instructions
- check compatibility with CoreNLP library and add it to instructions

#Fixes/Bugs

#Additional functions:

- add more lexicon items:
    - more modifiers
    - non-quantifying determiners
    - etc...

- add anaphors, tense/aspect etc.


#Structure/modularization

- restructure betaConversion?

- Make scope handling an extra class, instead of a static field in Determiner class

- why are we using streams for bound variables (e.g in the constructor of LLFormula)

# Modifiers
According to Gupta & Lamping the form S -o M is sufficient when doing compilations (except
for pronouns which we don't treat anyways).


# Implementing Lev's (2007) system:
- create category graph (done) including indices
- detect SCCs (Kosaraju algorithm, can be found online)
- create histories (Premises with indices of parent premises)
- use histories to determine order of combination
- implement optimizations