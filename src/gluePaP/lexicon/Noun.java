package gluePaP.lexicon;

import edu.stanford.nlp.ling.IndexedWord;
import gluePaP.linearLogic.LLAtom;
import gluePaP.linearLogic.LLTerm;
import gluePaP.semantics.SemAtom;
import gluePaP.semantics.SemType;

public class Noun extends LexicalEntry {


    public LexType lexType;


    public Noun(LexType type, String identifier, IndexedWord main) {

        this.identifier = identifier;

        //StringJoiner sj = new StringJoiner(" ");

        //f is standard variable for complete f-structure
        //g is standard variable for subject
        //h is standard variable for object
        this.lexType = type;

        switch (this.lexType) {
            case N_NNP:

                //this.llFormula = identifier + "_e";
                this.llTerm = new LLAtom(identifier, LLTerm.Type.E, LLAtom.LLType.CONST,true);

                this.sem = new SemAtom(SemAtom.SemSort.CONST,main.value().substring(0,1).toLowerCase(),
                        SemType.AtomicType.E);
                break;

            case N_NN:
               //this.llFormula = identifier + "_e";

                break;



        }
    }
}

