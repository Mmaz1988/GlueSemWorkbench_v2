# Compilation

These instructions work in a bash shell on a MacOs 10.13.2, with java 10.0.1. Should work on any Linux, with a recent java.

Ensure you have Stanford CoreNLP jars downloaded, say in dir `/path/to/corenlp`. This should have many jars, including `stanford-corenlp-3.8.0.jar`.

Then:
```
$ javac -cp ".:/path/to/corenlp/stanford-corenlp-3.8.0.jar" Main.java
```

To run, the system will need more files from the corenlp directory. So you want to tell the system where to find all these jars: 
```
$ java -cp ".:/path/to/corenlp/*" Main
The Glue Semantics Workbench
copyright 2018 Moritz Messmer & Mark-Matthias Zymla

Starting interactive dependency mode...

Enter sentence to be analyzed or enter 'quit' to exit the program.
```

Now entering a simple sentence like `John smiles` will give you an adequate response:
```
John smiles.
[main] INFO edu.stanford.nlp.parser.lexparser.LexicalizedParser - Loading parser from serialized file edu/stanford/nlp/models/lexparser/englishPCFG.ser.gz ... done [0.6 sec].
[nsubj(smiles-2, John-1), root(ROOT-0, smiles-2)]
John/NNP This is a subject
smiles/VBZ has arity 1
[g : j[0], (g ⊸ f) : λx_e.smiles(x)[1]] => null
Searching for valid proofs...
Agenda: [g : j[0], (g ⊸ f) : λx_e.smiles(x)[1]]
Combining premises (g ⊸ f) : λx_e.smiles(x)[1] and g : j[0]
-->f : smiles(j)[0, 1]
Found valid deduction(s): 
f : smiles(j)[0, 1]
Done!

Enter sentence to be analyzed or enter 'quit' to exit the program.
```
