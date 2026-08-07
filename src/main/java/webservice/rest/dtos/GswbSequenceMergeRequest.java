package webservice.rest.dtos;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

public class GswbSequenceMergeRequest {
    public String id;
    /** Ordered semantic parents. Syntax structures are merged by LiGER separately. */
    public List<GswbSemanticMergePart> parts = new ArrayList<>();

    /** Legacy graphs-only transport; analysis clients should use parts. */
    public List<String> semantics = new ArrayList<>();
    /** Legacy parallel graph transport; analysis clients should use parts. */
    public List<LinkedHashMap<String, Object>> graphs = new ArrayList<>();
    public String parentSolutionId;
    public String rootSolutionId;
    public String branchId;
    public String originalSemantic;
    public String solutionKey;
    public String mcSetId;
    public boolean resolveDrs = true;

    public GswbSequenceMergeRequest() {
    }
}
