package gluePaP.glue;

import edu.stanford.nlp.trees.GrammaticalStructure;
import edu.stanford.nlp.trees.TypedDependency;

import java.util.List;

// Sentence meaning is a set of glue representations (i.e. a set that represents the available premises)

public class SentenceMeaning {
    private final GrammaticalStructure dependencyStructure;
    public List<GlueRepresentation> gluePremises;


    public SentenceMeaning(GrammaticalStructure parsedSentence)
    {
        this.dependencyStructure = parsedSentence;

        for (TypedDependency structure : dependencyStructure.typedDependencies())
        {



        }



    }





}
