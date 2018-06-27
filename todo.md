# ToDO for gluePaP

#Fixes/Bugs

- intransitve verbs not working

- variable names not assigned properly (some duplicates)


#Additional functions:

- interface for XLE

- add modifiers, anaphors, tense/aspect etc.

- modifier/skeleton distinction for more efficient parsing
    - for modifiers of modifiers Gupta & Lamping's system might not work. Lev might be needed (see below)

#Structure/modularization

- SentenceMeaning.java: unabhängig von syntaktischem Input machen. Sollte Objekte  (oder einfach Strings) entgegen nehmen anhand derer die resources erstellt werden. Die Objekte/Strings werden dann in Klassen erzeugt die für die jeweilige Schnittstelle (UD, XLE etc) spezifisch sind.

- restructure betaConversion?

- Make scope handling an extra class, instead of a static field in Determiner class

- why are we using streams for bound variables (e.g in the constructor of LLFormula)

# Implementing Lev's (2007) system:
- create category graph (done) including indices
- detect SCCs (Kosaraju algorithm, can be found online)
- create histories (Premises with indices of parent premises)
- use histories to determine order of combination
- implement optimizations