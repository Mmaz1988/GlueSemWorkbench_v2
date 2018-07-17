/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.lexicon;

import glueSemantics.semantics.lambda.SemAtom;
import glueSemantics.semantics.lambda.SemFunction;
import glueSemantics.semantics.lambda.SemPred;
import glueSemantics.semantics.lambda.SemType;
import glueSemantics.synInterface.dependency.LexVariableHandler;
import glueSemantics.linearLogic.LLAtom;
import glueSemantics.linearLogic.LLFormula;
import glueSemantics.linearLogic.LLTerm;
import glueSemantics.synInterface.dependency.LexicalParserException;

import java.util.*;

public class Verb extends LexicalEntry {

    LexType lexType;


    public Verb(LinkedHashMap<String,LexicalEntry> subCatFrame, String lemma) throws LexicalParserException {



    this.lexType = lexTypeFromSubCat(subCatFrame);

        //f is standard variable for complete f-structure
        //g is standard variable for subject
        //h is standard variable for object

        switch (this.getLexType()) {
            case V_INTR:
                //Parentheses necessary for variable scope!
                {
                LexicalEntry agent = subCatFrame.get("agent");

                /*Linear Logic*/
                LLAtom agentRes;
                if (Determiner.getterScope() != null)
                {
                    agentRes = new LLAtom(Determiner.getScope("subj"), LLTerm.Type.E, LLAtom.LLType.CONST, false);
                } else
                {
                    agentRes = new LLAtom(agent.identifier, LLTerm.Type.E, LLAtom.LLType.CONST, false);
                }

                LLAtom fsem = new LLAtom(LexVariableHandler.returnNewVar(LexVariableHandler.variableType.LLatomT),
                        LLTerm.Type.T, LLAtom.LLType.CONST,true);

                 this.setLlTerm(new LLFormula(agentRes,fsem,true ));


                /*Semantics*/

                SemAtom agentVar = new SemAtom(SemAtom.SemSort.VAR,
                        //binding variable
                        LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                        SemType.AtomicType.E
                        );

                SemFunction verbSem = new SemFunction(agentVar,new SemPred(lemma,agentVar));

                this.setSem(verbSem);

                break;
            }

            case V_TRANS: {


                LexicalEntry agent = subCatFrame.get("agent");
                LexicalEntry patient = subCatFrame.get("patient");
                LLAtom agentRes;
                LLAtom patientRes;

                /*Linear Logic*/

                //generating consumer
                if (Determiner.getScope("subj") != null)
                {
                    agentRes = new LLAtom(Determiner.getScope("subj"), LLTerm.Type.E, LLAtom.LLType.CONST, false);
                } else
                {
                    agentRes = new LLAtom(agent.identifier, LLTerm.Type.E, LLAtom.LLType.CONST, false);
                }

                if (Determiner.getScope("obj") != null)
                {
                    patientRes = new LLAtom(Determiner.getScope("obj"), LLTerm.Type.E, LLAtom.LLType.CONST, false);
                }
                else {
                    patientRes = new LLAtom(patient.identifier, LLTerm.Type.E, LLAtom.LLType.CONST, false);
                }
                //generate semantics
                LLAtom fsem = new LLAtom(LexVariableHandler.returnNewVar(LexVariableHandler.variableType.LLatomT),
                        LLTerm.Type.T, LLAtom.LLType.CONST, true);

                LLFormula firstArg = new LLFormula(patientRes, fsem, true);

                this.setLlTerm(new LLFormula(agentRes, firstArg, true));


                /*Semantics*/

                SemAtom agentVar = new SemAtom(SemAtom.SemSort.VAR,
                        //binding variable
                        LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                        SemType.AtomicType.E
                );

                SemAtom patientVar = new SemAtom(SemAtom.SemSort.VAR,
                        //binding variable
                        LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),
                        SemType.AtomicType.E
                );


                SemFunction verbSem = new SemFunction(agentVar,
                                            new SemFunction(patientVar,
                                                    new SemPred(lemma,agentVar,patientVar)));

                this.setSem(verbSem);

                break;
            }
            default:
                throw new LexicalParserException("Verb type not implemented yet");

        }
    }


    //Setter and Getter methods

    public LexType getLexType() {
        return lexType;
    }

    public void setLexType(LexType lexType) {
        this.lexType = lexType;
    }

    /*
    public String getLlFormula() {
        return llFormula;
    }

    public void setLlFormula(String llFormula) {
        this.llFormula = llFormula;
    }
*/

    //trivial version of generating LexType from SubCatFrame
    public LexType lexTypeFromSubCat(LinkedHashMap<String,LexicalEntry> subCat)
    {
        if (subCat.size() == 1)
        {
            return LexType.V_INTR;
        }
        if (subCat.size() == 2 && subCat.containsKey("patient"))
        {
            return LexType.V_TRANS;
        }

        return null;
    }
}


