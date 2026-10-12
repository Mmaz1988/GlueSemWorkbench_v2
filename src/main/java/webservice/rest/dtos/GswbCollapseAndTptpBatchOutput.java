package webservice.rest.dtos;

import java.util.LinkedHashMap;

public class GswbCollapseAndTptpBatchOutput {
    public String parentSolutionId;
    /** Keyed by the requesting GswbTptpBatchItem.name, one entry per item, in request order. */
    public LinkedHashMap<String, GswbTptpBatchResult> results;

    public GswbCollapseAndTptpBatchOutput() {
    }
}
