package webservice.rest.dtos;

import java.util.LinkedHashMap;
import java.util.List;

public class GswbSolution {

    public String solution;
    public String id;
    public List<Integer> sourceIndices;
    public LinkedHashMap<String, Object> graph;

    public GswbSolution() {}

    public GswbSolution(String solution, String id)
    {
    this.solution = solution;
    this.id = id;
    }

    public GswbSolution(String solution, String id, List<Integer> sourceIndices)
    {
    this.solution = solution;
    this.id = id;
    this.sourceIndices = sourceIndices;
    }

    public GswbSolution(String solution, String id, List<Integer> sourceIndices, LinkedHashMap<String, Object> graph)
    {
        this.solution = solution;
        this.id = id;
        this.sourceIndices = sourceIndices;
        this.graph = graph;
    }
}
