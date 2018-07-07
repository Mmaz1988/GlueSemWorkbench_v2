package glueSemantics.glue;

import edu.stanford.nlp.ling.IndexedWord;

public class Tuple {

    final IndexedWord right;
    final String left;

    public Tuple(String left,IndexedWord right){
        this.left = left;
        this.right = right;


    }

    @Override
    public String toString() {
        return "(" + left + ": " + right.toString() + "--" + right.index() + ")";
    }
}
