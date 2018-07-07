package glueSemantics.lexicon;

import glueSemantics.glue.LexVariableHandler;
import glueSemantics.linearLogic.LLAtom;
import glueSemantics.linearLogic.LLFormula;
import glueSemantics.linearLogic.LLTerm;
import glueSemantics.semantics.SemAtom;
import glueSemantics.semantics.SemFunction;
import glueSemantics.semantics.SemPred;
import glueSemantics.semantics.SemType;

public class Noun extends LexicalEntry {


    public LexType lexType;

    public Noun(LexType type, String identifier, String main) {

        this.identifier = identifier;

        //StringJoiner sj = new StringJoiner(" ");

        //f is standard variable for complete f-structure
        //g is standard variable for subject
        //h is standard variable for object
        this.lexType = type;

        switch (this.lexType) {
            case N_NNP:

                //this.llFormula = identifier + "_e";
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


        }
    }


}

