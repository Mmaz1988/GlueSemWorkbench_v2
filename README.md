# About
This software project started out as a mini-project by Mark-Matthias Zymla and Moritz Messmer in cooperation with
Richard Crouch, Tracy Holloway King and Miriam Butt with the intention to revive the old glue prover used by XLE.
We want to provide a state-of-the-art platform for glue semantics that is both easy to use and extensible
due to its modular structure.

The system contains two linear logic provers:

1) A linear logic prover is based on Mark Hepple's
 chart prover and improved with suggestions made in Lev (2007)
2) A basic implementation of Lev's own linear logic prover. Generally provides the same solutions as the first prover, but is usually more efficient. However, it only works with propositional glue!

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

# Command line arguments 

The following table presents the possible command line arguments. Input and output file have to be specified first.
Omitting an input file allows you to run a file chooser. When no output file is specified the output is written to the console.

| `command line argument` | `effect` |
| ------------- | ------------- | 
| `-i [path/to/file]`  | `used to specify an input file (list of premises; .txt file)` |
| `-o [path/to/file]` | `specify an output file (.txt)` |
| `-pr [0/1]` | `0 for Hepple-style algorithm (default); 1 for Lev-style algorithm` |
| `-debugging` | `this argument can be used to output some additional debugging information` |
| `-outputStyle [0..3]` | `sets the output style to plain string (0), Prolog (1), JSON(2)*, or NLTK(3)` |
| `-parseSem` | `semantic parsing for the left-hand (semantic) side of a meaning constructor` |
| `-noreduce` | `toggles beta-reduction of semantic representations` |
| `-go` | `Only use the glue side in the output of the derivation` |
| `-s` | `Only the solution of the derivation is written to the output file` |
| `-test [sem_expr]` | `Parses a string containing a semantic expression and returns the expected result (use without any other parameters)` |
| `-onlyMeaningSide` | `Only the meaning side of the solutions are output. Partial solutions and derivations are not output. `|
| `-explainFail` | `Include an explanation of failing cases (potantially non-cobining premises) in the output. This option only works with Hepple-style algorithm.  ` |
| `-readStdIn` | `Read the input from Stdin. If a filename is given by -i, the file is ignored` |
| `-writeStdOut` | `Write the output to Stdout. If a filename is given by -o, the file is ignored` |
| `-assureGlueParsing` | `Assure glue parsing. Skip the set of premises in case of a parse error in that set.` |

*in development.

The NLTK output option formats the semantic side output suitable to be parsed by the NLTK Drs parser.

## Glue formula syntax
- Glue formulas can be entered manually into a plain text file (".txt").
- Each formula needs to be in a separate line.
- Each formula must contain a semantic side and a glue side separated by a colon (whitespaces around the colon are optional).
- The semantic side of a formula may have any form and may contain any (unicode) character except ":" (when semantic parsing is active, the input is more restrictive)
- The glue side of a formula must be a valid implicationa linear logic formula, with each implication put in parantheses.
- LL atoms must be single letters: lower-case letters (a-z) will be recognized as LL constants, while upper-case letters will be recognized as LL variables
- LL atoms can (and should) be suffixed with an underscore and a letter denoting their type ("e" or "t").
- Linear implication operators are written as "-o", LL quantifiers (especially the "universal quantifier") are not necessary
for the introduction of LL variables.

# Info

The inbuilt syntax/semantics interface for XLE and UD was removed, since the required libraries bloated the project and haven't been useful for people interested in the project. We recommend two ways for specifying the syntax/semantics interface:

- [XLE+Glue](https://github.com/Mmaz1988/xle-glueworkbench-interface)
- The rewrite system for linguistic annotations found [here](https://github.com/Mmaz1988/abstract-syntax-annotator-web). 


# Contact
For more information please contact     
mark-matthias.zymla(at)uni-konstanz.de        or     
moritzmessmer(at)gmail.com      

