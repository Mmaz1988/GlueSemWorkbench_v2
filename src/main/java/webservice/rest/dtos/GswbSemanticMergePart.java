package webservice.rest.dtos;

import java.util.LinkedHashMap;

/** One ordered semantic analysis used by the semantic sequence merge. */
public class GswbSemanticMergePart {
    /** Canonical semantic ID of this parent analysis. */
    public String id;
    /** Source sentence or sequence identity, when available. */
    public String sentenceId;
    /** Transport/proof ID; not used as semantic identity. */
    public String solutionId;
    public String proofId;
    /** Canonical syntax analysis that produced this semantic analysis. */
    public String syntacticOrigin;
    public String mcSetId;
    public String semantic;
    /** Canonical LFGxDRT graph. This is the merge input. */
    public LinkedHashMap<String, Object> graph;
    public LinkedHashMap<String, Object> provenance = new LinkedHashMap<>();

    public GswbSemanticMergePart() {
    }
}
