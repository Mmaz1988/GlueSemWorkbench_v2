package webservice.rest.dtos;

import java.util.List;

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
    /**
     * Pronouns in this item the mapping did not bind, as they are named in {@link #semantic}.
     * Their ant(x) marker was dropped, so each is a new referent, equal to no other. Empty on a
     * fully resolved item. Clients must surface it like {@link #degraded}.
     */
    public List<String> unresolvedPronouns = List.of();

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
