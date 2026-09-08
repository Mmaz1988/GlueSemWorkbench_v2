package webservice.rest.dtos;

public class GswbTptpBatchResult {
    public String tptp;
    public String semantic;
    /**
     * Set when this item was translated, but not as requested: the anaphora mapping could not
     * be applied and was dropped so the item would still translate. Null on a clean result.
     * Clients must surface it -- a degraded item is otherwise indistinguishable from a
     * resolved one, since both come back with usable TPTP.
     */
    public String degraded;

    public GswbTptpBatchResult() {
    }

    public GswbTptpBatchResult(String tptp, String semantic) {
        this.tptp = tptp;
        this.semantic = semantic;
    }

    public GswbTptpBatchResult(String tptp, String semantic, String degraded) {
        this.tptp = tptp;
        this.semantic = semantic;
        this.degraded = degraded;
    }
}
