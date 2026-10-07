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
    /** The accommodation sites of a PCDRS branch, P:[p(x3,x1),...]: the presupposition of x3 in
     *  the box of x1. Empty when nothing is accommodated. */
    public String presuppositionMapping;
    public String proofId;
    public String solutionKey;
    public String mcSetId;
    public GswbSemanticAnalysis semanticAnalysis;
    public LinkedHashMap<String, List<String>> synSemMapping = new LinkedHashMap<>();
    /** For a solution of a proof: insitu modifiers outscoped by something to their left (only
     *  ever non-zero after the relaxed retry). Null for solutions not read off a proof. */
    public Integer insituViolations;
    /** For a solution of a proof: scope pairs whose outer element comes later in the sentence
     *  than the inner one. A proof's solutions come ordered by these two counts. */
    public Integer surfaceOrderViolations;

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
