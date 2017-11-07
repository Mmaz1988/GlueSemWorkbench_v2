package gluePaP.glue;

import edu.stanford.nlp.ling.IndexedWord;
import edu.stanford.nlp.trees.GrammaticalStructure;
import edu.stanford.nlp.trees.TypedDependency;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

// Sentence meaning is a set of glue representations (i.e. a set that represents the available premises)

public class SentenceMeaning {
    private final GrammaticalStructure dependencyStructure;
    private final LinkedHashMap<IndexedWord,List<Tuple>> dependencyMap;
    public List<GlueRepresentation> gluePremises;


    public SentenceMeaning(GrammaticalStructure parsedSentence)
    {
        this.dependencyStructure = parsedSentence;

        this.dependencyMap = generateDependencyMap();






   //     System.out.println(generateDependencyMap());

    }


    // generates a HashMap for search purposes
    public LinkedHashMap<IndexedWord,List<Tuple>> generateDependencyMap()
    {
        LinkedHashMap<IndexedWord,List<Tuple>> dependencyMap = new LinkedHashMap<>();

        for (TypedDependency structure : dependencyStructure.typedDependencies())
        {
            //new entry if no key for the respective pred is available
            if (dependencyMap.get(structure.gov()) == null)

            {
                List<Tuple> values = new ArrayList<>();
                values.add(new Tuple(structure.reln().toString(),structure.dep()));
                dependencyMap.put(structure.gov(), values);
            }
                else
                    {
                        dependencyMap.get(structure.gov()).add(new Tuple(structure.reln().toString(),structure.dep()));
            }
        }

        return dependencyMap;
    }




    public boolean hasDependency(String dependency,IndexedWord word)
    {
         for (Tuple tuple : dependencyMap.get(word))
         {
             if (dependency == tuple.left)
             {
                 return true;
             }
         }
         return false;
    }

}
