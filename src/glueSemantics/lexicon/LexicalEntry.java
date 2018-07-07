package glueSemantics.lexicon;

import glueSemantics.linearLogic.LLTerm;
import glueSemantics.semantics.SemRepresentation;

public class LexicalEntry {

    String identifier;

    public LLTerm getLlTerm() {
        return llTerm;
    }

    public void setLlTerm(LLTerm llTerm) {
        this.llTerm = llTerm;
    }

    public SemRepresentation getSem() {
        return sem;
    }

    public void setSem(SemRepresentation sem) {
        this.sem = sem;
    }

    private LLTerm llTerm;
    private SemRepresentation sem;

    public enum LexType {

        //Verbs
        V_INTR,
        V_TRANS,
        V_DTRAN,
        V_COMP,
        V_XCOMP,

        //Nouns
        N_NN,
        N_NNP,
        N_DP,

        //Determiner
        DET,
        //TODO: modifiers
        MOD

    }

}
