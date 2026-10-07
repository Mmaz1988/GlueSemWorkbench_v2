package webservice.rest.dtos;

import java.util.List;

public class GswbCollapseAndTptpBatchRequest {
    public String parentSolutionId;
    /** The structured mapping computed once by /generate_pcdrs, applied identically to every
     *  item below -- see GswbCollapseAnaphoraRequest.anaphoraRelations for why this must be
     *  supplied explicitly rather than re-derived per item. */
    public List<AnaphoraRelationDto> anaphoraRelations;
    /** The branch's accommodations from /generate_pcdrs, applied to every item that does not
     *  carry its own P:[...], before the anaphora mapping. Anchored on referents, so they fit the
     *  items that wrap the sentence into a check. */
    public List<PresuppositionRelationDto> presuppositionRelations;
    public List<GswbTptpBatchItem> items;
    public boolean typed;

    public GswbCollapseAndTptpBatchRequest() {
    }
}
