package gluePaP.lexicon;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

public class LexicalEntry {

    HashSet<String> usedVariables = new HashSet<>();
    public String llFormula;

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
    public String returnNewVar()
    {
        List<String> variables = new ArrayList<>(Arrays.asList("Y","Z"));

        if (usedVariables.isEmpty())
        {
            return "X";
        } else
        {
        for (String var : variables)
        {
            if (!usedVariables.contains(var))
            {
                return var;
            }
        }
        }
        return null;
    }

}
