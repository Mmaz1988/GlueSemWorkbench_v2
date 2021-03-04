# About
This software project started out as a mini-project by Mark-Matthias Zymla and Moritz Messmer in cooperation with
Richard Crouch, Tracy Holloway King and Miriam Butt with the intention to revive the old glue prover used by XLE.
We want to provide a state-of-the-art platform for glue semantics that is both easy to use and extensible
due to its modular structure.

The system contains two prover:

1) A linear logic prover is based on Mark Hepple's
 chart prover and improved with suggestions made in Lev (2007)
2) A basic implementation of Lev's own linear logic prover

## Licensing
The Glue Semantics Workbench
Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
This file is part of the Glue Semantics Workbench
The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
without any warranty.
You should have received a copy of the GNU General Public License along with the source code.
If not, please visit http://www.gnu.org/licenses/ for more information.

# Usage
You will need a Java runtime environment (version 8 or higher) to run the program. We recommend to use OpenJDK which can be downloaded at [OpenJDK](https://jdk.java.net/13/).
To run the Workbench in a terminal window (Terminal for Unix systems or Powershell/cmd for Windows) enter
   java -jar glueSemanticsWorkbench.jar [parameters]
When running the program without parameters it initiates the manual entry mode by default, prompting the user
to open a text file containing glue formulas. Check the "glue formula syntax" section for more information on how to
enter formulas.
- For running the program in interactive dependency mode, add _-dp_ as a program argument.
- For running it in LFG mode, in which it reads a Prolog f-structure file created by XLE, add _-lfg_ as a program argument.

## Additional parameters
- For disabling automatic betareduction in dependency and LFG mode, add the parameter _-noreduce_
- For using Blackburn & Bos-style Prolog as output format, add the parameter _-prolog_

## Glue formula syntax
Glue formulas can be entered manually into a plain text file (".txt") to be read in by the default mode.
Each formula needs to be in a separate line.
Each formula must contain a semantic side and a glue side separated by a colon (whitespaces around the colon are optional).
The semantic side of a formula may have any form and may contain any (unicode) character except ":"
The glue side of a formula must be a valid implicationa linear logic formula, with each implication put in parantheses.
LL atoms must be single letters: lower-case letters (a-z) will be recognized as LL constants, while upper-case letters will be recognized as LL
variables. LL atoms can (and should) be suffixed with an underscore and a letter denoting their type ("e" or "t").
Linear implication operators are written as "-o", LL quantifiers (especially the "universal quantifier") are not necessary
for the introduction of LL variables.

# Troubleshooting
- When loading the dependency mode, a message states "Could not initialize dependency parser".

This usually means that the Stanford CoreNLP JAR file could not be found.
It needs to be in the same directory as the glueSemWorkbench JAR file. The JAR file can be optained [here](https://stanfordnlp.github.io/CoreNLP/history.html)

# Contact
For more information please contact
moritzmessmer(at)gmail.com      or
mark-matthias.zymla(at)uni-konstanz.de
