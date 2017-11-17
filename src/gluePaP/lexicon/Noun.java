package gluePaP.lexicon;

public class Noun extends LexicalEntry {


    public LexType lexType;


    public Noun(LexType type, String identifier) {

        //StringJoiner sj = new StringJoiner(" ");

        //f is standard variable for complete f-structure
        //g is standard variable for subject
        //h is standard variable for object
        this.lexType = type;

        switch (this.lexType) {
            case N_NNP:

                this.llFormula = identifier + "_e";

                break;

            case N_NN:
                this.llFormula = identifier + "_e";

                break;



        }
    }
}

