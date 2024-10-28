package glueSemantics.parser;

import glueSemantics.semantics.MeaningConstructor;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

import java.util.LinkedHashMap;
import java.util.List;

public class LexicalEntries {
    public LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries;
    public Graph<String, DefaultEdge> multiStageGraph;

    public  LexicalEntries(LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries)
    {
        this.lexicalEntries = lexicalEntries;
    }
    public LexicalEntries(LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries, Graph<String, DefaultEdge> multiStageGraph)
    {
        this.lexicalEntries = lexicalEntries;
        this.multiStageGraph = multiStageGraph;
    }
}
