package webservice.rest.dtos;

import java.util.List;

public class GswbCollapseAndTptpBatchRequest {
    public String parentSolutionId;
    /** The structured mapping computed once by /generate_pcdrs, applied identically to every
     *  item below -- see GswbCollapseAnaphoraRequest.anaphoraRelations for why this must be
     *  supplied explicitly rather than re-derived per item. */
    public List<AnaphoraRelationDto> anaphoraRelations;
    public List<GswbTptpBatchItem> items;
    public boolean typed;

    public GswbCollapseAndTptpBatchRequest() {
    }
}
