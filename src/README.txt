The Glue Semantics Workbench
Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
This file is part of the Glue Semantics Workbench
The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
without any warranty.
You should have received a copy of the GNU General Public License along with the source code.
If not, please visit http://www.gnu.org/licenses/ for more information.

****ABOUT****
This software project started out as a mini-project by Mark-Matthias Zymla and Moritz Messmer in cooperation with
Richard Crouch, Tracy Holloway King and Miriam Butt with the intention to revive the old glue prover used by XLE.
We want to provide a state-of-the-art platform for glue semantics that is both easy to use and extensible
due to its modular structure. Our program provides modules for reading syntactic input from Stanford Core NLP
dependency parses and Prolog f-structure files created by the Xerox linguistic environment
 (http://ling.uni-konstanz.de/pages/xle/doc/xle_toc.html). The linear logic prover is based on Mark Hepple's
 chart prover with ideas from Gupta & Lamping (2008). The lexicon module provides some basic classes for creating
 glue premises for verbs, nouns, determiners and adjectives and other modifiers.
 Lexical entries can also be entered manually by using the default manual-entry mode (see below).

****DISTRIBUTION****
For using the dependency parser, the Stanford CoreNLP tools are required, preferably in version 3.8.0 from
2017-06-09. The CoreNLP tools can be downloaded here: https://stanfordnlp.github.io/CoreNLP/history.html.
We recommend distributing them as part of the JAR file or adding a note on how to install and run the dependency
parser along with any distribution.

****USAGE****
When running the program (from a JAR file for example) it initiates the manual entry mode by default, prompting the user
to open a text file containing glue formulas. Check the "glue formula syntax" section for more information on how to
enter formulas.
For running the program in interactive dependency mode, add "-dp" as a program argument.
For running it in LFG mode, in which it reads a Prolog f-structure file created by XLE, add "-lfg" as a program argument.

    **GLUE FORMULA SYNTAX**
    Glue formulas can be entered manually into a plain text file (".txt") to be read in by the default mode.
    Each formula needs to be in a separate line.
    Each formula must contain a semantic side and a glue side separated by a colon (whitespaces around the colon are optional).
    The semantic side of a formula may have any form and may contain any (unicode) character except ":"
    The glue side of a formula must be a valid implicationa linear logic formula, with each implication put in parantheses.
    LL atoms must be single letters: lower-case letters (a-z) will be recognized as LL constants, while upper-case letters will be recognized as LL
    variables. LL atoms can (and should) be suffixed with an underscore and a letter denoting their type ("e" or "t").
    Linear implication operators are written as "-o", LL quantifiers (especially the "universal quantifier") are not necessary
    for the introduction of LL variables.


For more information please contact
moritzmessmer@gmail.com      or
mark-matthias.zymla@uni-konstanz.de