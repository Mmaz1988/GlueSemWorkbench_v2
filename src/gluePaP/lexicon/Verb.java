package gluePaP.lexicon;

import gluePaP.glue.LexVariableHandler;
import gluePaP.linearLogic.LLAtom;
import gluePaP.linearLogic.LLFormula;
import gluePaP.linearLogic.LLTerm;
import gluePaP.semantics.*;

import java.util.*;

public class Verb extends LexicalEntry {

    LexType lexType;


    public Verb(LinkedHashMap<String,LexicalEntry> subCatFrame, String lemma) {



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
                if (Determiner.getterScope() != null)
                {
                    agentRes = new LLAtom(Determiner.getScope("subj"), LLTerm.Type.E, LLAtom.LLType.CONST, false);
                } else
                {
                    agentRes = new LLAtom(agent.identifier, LLTerm.Type.E, LLAtom.LLType.CONST, false);
                }

                if (Determiner.getterScope() != null)
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
            case V_COMP:



                break;
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


