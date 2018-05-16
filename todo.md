# ToDO for gluePaP

#Fixes/Bugs

1. intransitve verbs not working

2. variable names not assigned properly (some duplicates)


#Additional functions:

1. interface for XLE

2. add modifiers, anaphors, tense/aspect etc.

3. modifier/skeleton distinction for more efficient parsing

#Structure/modularization

- SentenceMeaning.java: unabhängig von syntaktischem Input machen. Sollte Objekte  (oder einfach Strings) entgegen nehmen anhand derer die resources erstellt werden. Die Objekte/Strings werden dann in Klassen erzeugt die für die jeweilige Schnittstelle (UD, XLE etc) spezifisch sind.

- restructure betaConversion?

- Make scope handling an extra class, instead of a static field in Determiner class

- why are we using streams for bound variables (e.g in the constructor of LLFormula)