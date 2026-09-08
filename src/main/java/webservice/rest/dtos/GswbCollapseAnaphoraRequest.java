package webservice.rest.dtos;

import java.util.LinkedHashMap;
import java.util.List;

public class GswbCollapseAnaphoraRequest {
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
    public String anaphoraMapping;
    /** The structured mapping computed once by /generate_pcdrs, reused here so the same
     *  binding is applied consistently to `semantic` regardless of which check/branch it
     *  came from -- must be supplied explicitly since `semantic` is re-parsed from scratch
     *  and carries no mapping of its own. */
    public List<AnaphoraRelationDto> anaphoraRelations;

    public GswbCollapseAnaphoraRequest() {
    }
}
