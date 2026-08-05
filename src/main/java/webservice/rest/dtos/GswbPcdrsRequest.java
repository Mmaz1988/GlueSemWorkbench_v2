package webservice.rest.dtos;

import java.util.LinkedHashMap;

public class GswbPcdrsRequest {
    public SemanticModel semanticModel = SemanticModel.LFGXDRT;
    public String semantic;
    public String semanticSvg;
    public LinkedHashMap<String, Object> graph;
    public String parentSolutionId;
    public String rootSolutionId;
    public String branchId;
    public String originalSemantic;
    public String pcdrsSemantic;
    public String collapsedSemantic;
    public String normalizedSemantic;
    public LinkedHashMap<String, Object> mergedStructure;

    public GswbPcdrsRequest() {
    }
}
