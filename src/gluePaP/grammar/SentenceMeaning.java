package gluePaP.grammar;

import edu.stanford.nlp.trees.GrammaticalStructure;

import java.util.List;

public class SentenceMeaning {
    private final GrammaticalStructure dependencyStructure;
    public List<GlueRepresentation> gluePremises;


    public SentenceMeaning(GrammaticalStructure parsedSentence)
    {
        this.dependencyStructure = parsedSentence;
    }
}
