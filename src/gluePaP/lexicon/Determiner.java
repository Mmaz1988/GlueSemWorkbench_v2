package gluePaP.lexicon;

import gluePaP.glue.LexVariableHandler;

public class Determiner extends LexicalEntry{
    public LexicalEntry.LexType lexType;

    enum QuantType
    {
        ALL,
        EX
    }

    public Determiner(String identifier)
    {
        this.lexType = LexType.DET;

        LexVariableHandler handler = new LexVariableHandler();

        String i = identifier + "_e";
        String var = handler.returnNewVar() + "_t";

        this.llFormula = "A" +var + ".(" + i + " -o " + var + ") -o " + var;
    }

}
