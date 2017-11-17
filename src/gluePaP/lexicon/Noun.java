package gluePaP.lexicon;

public class Noun implements LexicalEntry {


    public LexType lexType;
    public String formula;

    public Noun(LexType type, String lemma) {

        //StringJoiner sj = new StringJoiner(" ");

        //f is standard variable for complete f-structure
        //g is standard variable for subject
        //h is standard variable for object
        this.lexType = type;

        switch (this.lexType) {
            case N_NNP:

                this.formula = Character.toString(lemma.charAt(0)).toLowerCase() + "_e";

                break;



        }
    }
}

