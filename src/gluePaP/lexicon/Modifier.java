package gluePaP.lexicon;

public class Modifier extends LexicalEntry {
    public LexType lexType;


    public Modifier(String identifier)
    {
        this.lexType = LexType.MOD;
        //identifier is provided by dependency in SentenceMeaning
        //this.llFormula = "(" + identifier + "_e" + " -o " + identifier + "_e" + ")";

        // (g -o f)

    }
}
