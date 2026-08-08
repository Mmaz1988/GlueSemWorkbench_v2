package webservice.rest.dtos;

import java.util.LinkedHashMap;

/** Canonical semantic-analysis representation returned by GSWB. */
public class GswbSemanticAnalysis {
    public String syntacticOrigin;
    public String semId;
    public String semString;
    /** Reserved for a linguistic structure supplied by a coordinator; GSWB does not populate it. */
    public LinkedHashMap<String, Object> structure;
    /** Canonical LFGxDRT semantic graph. */
    public LinkedHashMap<String, Object> graph;
    public String semType;
    /** Rendered DRS SVG for this solution, when available (LFGxDRT display mode only). */
    public String svg;

    public GswbSemanticAnalysis() {
    }

    public GswbSemanticAnalysis(String syntacticOrigin, String semId, String semString,
                                LinkedHashMap<String, Object> structure,
                                LinkedHashMap<String, Object> graph, String semType) {
        this(syntacticOrigin, semId, semString, structure, graph, semType, null);
    }

    public GswbSemanticAnalysis(String syntacticOrigin, String semId, String semString,
                                LinkedHashMap<String, Object> structure,
                                LinkedHashMap<String, Object> graph, String semType, String svg) {
        this.syntacticOrigin = syntacticOrigin;
        this.semId = semId;
        this.semString = semString;
        this.structure = structure;
        this.graph = graph;
        this.semType = semType;
        this.svg = svg;
    }
}
