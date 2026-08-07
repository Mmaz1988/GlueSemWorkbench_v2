package webservice.rest.dtos;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

public class GswbSequenceMergeRequest {
    public String id;
    public List<GswbSequencePart> parts = new ArrayList<>();

    /** Parallel semantic text values for the corresponding graph entries. */
    public List<String> semantics = new ArrayList<>();
    /** Canonical semantic graph payloads when provenance is supplied separately. */
    public List<LinkedHashMap<String, Object>> graphs = new ArrayList<>();
    public String parentSolutionId;
    public String rootSolutionId;
    public String branchId;
    public String originalSemantic;
    public String solutionKey;
    public String mcSetId;

    public GswbSequenceMergeRequest() {
    }
}
