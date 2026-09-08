package webservice.rest.dtos;

import java.util.LinkedHashMap;
import java.util.List;

public class GswbSolution {

    public String solution;
    public String id;
    public Integer sourceIndex;
    public LinkedHashMap<String, Object> graph;
    public String semantic;
    public String anaphoraMapping;
    public List<AnaphoraRelationDto> anaphoraRelations;
    public String proofId;
    public String solutionKey;
    public String mcSetId;
    public GswbSemanticAnalysis semanticAnalysis;
    public LinkedHashMap<String, List<String>> synSemMapping = new LinkedHashMap<>();

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

    public GswbSolution(String solution, String id, Integer sourceIndex,
                        LinkedHashMap<String, Object> graph, String semantic) {
        this(solution, id, sourceIndex, graph);
        this.semantic = semantic;
    }

}
