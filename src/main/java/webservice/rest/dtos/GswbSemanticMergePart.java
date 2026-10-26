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
    /** Added to every {@code SRC iN} of {@link #graph}, and to its {@code sourceIndex}, before
     *  the merge: a sentence's reading derived on its own, appended to a discourse, has its
     *  indices where the sequence has them. The value is the part's {@code sourceIndexOffset}
     *  from LiGER's {@code sequenceParts}; 0 (the default) for a part already numbered in the
     *  sequence, such as the discourse it is appended to. */
    public int sourceIndexOffset;
    public LinkedHashMap<String, Object> provenance = new LinkedHashMap<>();

    public GswbSemanticMergePart() {
    }
}
