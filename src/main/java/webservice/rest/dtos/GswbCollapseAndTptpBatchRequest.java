package webservice.rest.dtos;

import java.util.List;

/** One reading's reasoning items: "sequence" (the PCDRS, its A: and P: inline), "context" and
 *  the four checks, which get the sequence's reading (I11). */
public class GswbCollapseAndTptpBatchRequest {
    public String parentSolutionId;
    public List<GswbTptpBatchItem> items;
    public boolean typed;

    public GswbCollapseAndTptpBatchRequest() {
    }
}
