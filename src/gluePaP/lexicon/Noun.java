package gluePaP.lexicon;

public class Noun implements LexicalEntry {


    LexType lexType;
    String formula;

    public Noun(LexType type, String lemma) {

        //StringJoiner sj = new StringJoiner(" ");

        //f is standard variable for complete f-structure
        //g is standard variable for subject
        //h is standard variable for object

        switch (type) {
            case N_NNP:

                this.formula = Character.toString(lemma.charAt(0)).toLowerCase();

                break;



        }
    }
}

