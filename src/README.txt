The Glue Semantics Workbench
Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
This file is part of the Glue Semantics Workbench
The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
without any warranty.
You should have received a copy of the GNU General Public License along with the source code.
If not, please visit http://www.gnu.org/licenses/ for more information.

****ABOUT
This software project started out as a mini-project by Mark-Matthias Zymla and Moritz Messmer in cooperation with
Richard Crouch, Tracy Holloway-King and Miriam Butt with the intention to revive the old glue prover used by XLE.
We wanted to provide a state-of-the-art platform for glue semantics that is both easy to use and easy to extend
due to its modular structure.


****DISTRIBUTION
For using the dependency parser, the Stanford CoreNLP tools are required, preferably in version 3.8.0 from
2017-06-09. The CoreNLP tools can be downloaded from here: https://stanfordnlp.github.io/CoreNLP/history.html.
We recommend distributing them as part of the JAR file or adding a note on how to install and run the dependency
parser.

****USAGE
When running the program (from a JAR file for example) it initiates the interactive dependency parser mode by default.
If it should be run in LFG mode, in which it reads a Prolog f-structure file created by XLE, add "lfg" as a program argument.

For more information please contact
moritz.messmer@uni-konstanz.de      or
mark-matthias.zymla@uni-konstanz.de