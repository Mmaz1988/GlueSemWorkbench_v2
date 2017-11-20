package gluePaP.lexicon;

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

        String i = identifier + "_e";
        String var = returnNewVar() + "_t";

        this.llFormula = "A" +var + ".(" + i + " -o " + var + ") -o " + var;
    }

}
