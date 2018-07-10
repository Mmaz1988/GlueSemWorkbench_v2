/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.lexicon;

import glueSemantics.synInterface.dependency.LexVariableHandler;
import glueSemantics.linearLogic.LLAtom;
import glueSemantics.linearLogic.LLFormula;
import glueSemantics.linearLogic.LLTerm;
import glueSemantics.semantics.SemAtom;
import glueSemantics.semantics.SemFunction;
import glueSemantics.semantics.SemPred;
import glueSemantics.semantics.SemType;
import glueSemantics.synInterface.dependency.LexicalParserException;

public class Noun extends LexicalEntry {


    public LexType lexType;

    public Noun(LexType type, String identifier, String main) throws LexicalParserException {

        this.identifier = identifier;


        //f is standard variable for complete f-structure
        //g is standard variable for subject
        //h is standard variable for object
        this.lexType = type;

        switch (this.lexType) {
            case N_NNP:

                this.setLlTerm(new LLAtom(identifier, LLTerm.Type.E, LLAtom.LLType.CONST, true));

                this.setSem(new SemAtom(SemAtom.SemSort.CONST, main.substring(0, 1).toLowerCase(),
                        SemType.AtomicType.E));
                break;

            case N_NN:

                /*Linear Logic*/

                //generating consumer
                LLAtom agentRes = new LLAtom(identifier, LLTerm.Type.E, LLAtom.LLType.CONST, false);

                //generate semantics
                LLAtom fsem = new LLAtom(identifier,
                        LLTerm.Type.T, LLAtom.LLType.CONST, true);

                this.setLlTerm(new LLFormula(agentRes, fsem, true));

                /*Semantics*/

                SemAtom agentVar = new SemAtom(SemAtom.SemSort.VAR,
                        //binding variable
                        LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                        SemType.AtomicType.E
                );

                SemFunction nounSem = new SemFunction(agentVar, new SemPred(main, agentVar));
                this.setSem(nounSem);

                break;

                default:
                    throw new LexicalParserException("Noun type not implemented yet");


        }
    }


}

