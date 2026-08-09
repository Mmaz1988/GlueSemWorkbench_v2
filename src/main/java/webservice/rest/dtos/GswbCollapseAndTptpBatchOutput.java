package webservice.rest.dtos;

import java.util.LinkedHashMap;
import java.util.List;

public class GswbCollapseAndTptpBatchOutput {
    public String parentSolutionId;
    public String anaphoraMapping;
    public List<AnaphoraRelationDto> anaphoraRelations;
    /** Keyed by the requesting GswbTptpBatchItem.name, one entry per item, in request order. */
    public LinkedHashMap<String, GswbTptpBatchResult> results;

    public GswbCollapseAndTptpBatchOutput() {
    }
}
