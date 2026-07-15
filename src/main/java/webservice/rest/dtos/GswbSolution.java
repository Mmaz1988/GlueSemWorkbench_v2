package webservice.rest.dtos;

import java.util.LinkedHashMap;

public class GswbSolution {

    public String solution;
    public String id;
    public Integer sourceIndex;
    public LinkedHashMap<String, Object> graph;

    public GswbSolution() {}

    public GswbSolution(String solution, String id)
    {
    this.solution = solution;
    this.id = id;
    }

    public GswbSolution(String solution, String id, Integer sourceIndex)
    {
    this.solution = solution;
    this.id = id;
    this.sourceIndex = sourceIndex;
    }

    public GswbSolution(String solution, String id, Integer sourceIndex, LinkedHashMap<String, Object> graph)
    {
        this.solution = solution;
        this.id = id;
        this.sourceIndex = sourceIndex;
        this.graph = graph;
    }
}
