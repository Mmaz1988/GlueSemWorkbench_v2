package gluePaP.lexicon;

import gluePaP.linearLogic.LLTerm;
import gluePaP.semantics.SemRepresentation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

public class LexicalEntry {

    HashSet<String> usedVariables = new HashSet<>();
    public String llFormula;
    public LLTerm llTerm;
    public SemRepresentation sem;

    //TODO maybe we want to remove this at some point?
    public Integer llId = 0;

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

    public String assignID()
    {
        this.llId++;
        return this.llId.toString();
    }

    //basic way for returning an unused variable out of an available set of variables


}
