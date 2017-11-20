package gluePaP.lexicon;

import gluePaP.semantics.SemRepresentation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

public class LexicalEntry {

    HashSet<String> usedVariables = new HashSet<>();
    public String llFormula;
    public SemRepresentation sem;

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


    //basic way for returning an unused variable out of an available set of variables


}
