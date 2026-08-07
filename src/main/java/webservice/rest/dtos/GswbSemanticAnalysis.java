package webservice.rest.dtos;

import java.util.LinkedHashMap;

/** Canonical semantic-analysis representation returned by GSWB. */
public class GswbSemanticAnalysis {
    public String syntacticOrigin;
    public String semId;
    public String semString;
    public LinkedHashMap<String, Object> structure;
    public LinkedHashMap<String, Object> graph;
    public String semType;

    public GswbSemanticAnalysis() {
    }

    public GswbSemanticAnalysis(String syntacticOrigin, String semId, String semString,
                                LinkedHashMap<String, Object> structure,
                                LinkedHashMap<String, Object> graph, String semType) {
        this.syntacticOrigin = syntacticOrigin;
        this.semId = semId;
        this.semString = semString;
        this.structure = structure;
        this.graph = graph;
        this.semType = semType;
    }
}
