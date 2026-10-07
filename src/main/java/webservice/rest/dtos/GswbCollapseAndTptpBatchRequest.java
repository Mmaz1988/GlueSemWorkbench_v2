package webservice.rest.dtos;

import java.util.List;

public class GswbCollapseAndTptpBatchRequest {
    public String parentSolutionId;
    /** The structured mapping computed once by /generate_pcdrs, applied identically to every
     *  item below -- see GswbCollapseAnaphoraRequest.anaphoraRelations for why this must be
     *  supplied explicitly rather than re-derived per item. */
    public List<AnaphoraRelationDto> anaphoraRelations;
    /** The branch's accommodation sites from /generate_pcdrs (P:[...]), applied to every item that
     *  does not carry its own, before the anaphora mapping. Sites are named by referents, so the
     *  mapping fits the items that wrap the sentence into a check. */
    public String presuppositionMapping;
    public List<GswbTptpBatchItem> items;
    public boolean typed;

    public GswbCollapseAndTptpBatchRequest() {
    }
}
