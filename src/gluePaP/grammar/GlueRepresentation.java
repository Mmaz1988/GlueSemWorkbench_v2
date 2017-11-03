package gluePaP.grammar;

import edu.stanford.nlp.trees.GrammaticalStructure;
import gluePaP.linearLogic.LLTerm;
import gluePaP.semantics.SemRepresentation;

public class GlueRepresentation {

    private final GrammaticalStructure gs;
    public LLTerm glueSide;
    public SemRepresentation meaningSide;



    public GlueRepresentation(GrammaticalStructure gs)
    {
        this.gs = gs;


    }
}
