/*
 * Copyright 2018 Mark-Matthias Zymla & Moritz Messmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package glueSemantics.lexicon;

import glueSemantics.synInterface.dependency.LexVariableHandler;
import glueSemantics.linearLogic.LLAtom;
import glueSemantics.linearLogic.LLFormula;
import glueSemantics.linearLogic.LLTerm;
import glueSemantics.semantics.lambda.SemAtom;
import glueSemantics.semantics.lambda.SemFunction;
import glueSemantics.semantics.lambda.SemPred;
import glueSemantics.semantics.lambda.SemType;
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

