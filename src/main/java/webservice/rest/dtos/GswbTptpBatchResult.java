package webservice.rest.dtos;

import java.util.List;
import java.util.Map;

/** One batch item collapsed and translated, with the reading it was collapsed with (I11). */
public class GswbTptpBatchResult {
    /** Empty if the item's collapse rejected the reading. */
    public String tptp;
    /** The collapsed DRS. */
    public String semantic;
    /** Why the collapse rejected the reading; null otherwise. Clients must surface it. */
    public String rejection;
    /** The reading this item was collapsed with: the sequence's own, a check's rebased one, or
     *  the context's part of it. */
    public String anaphoraMapping;
    public String presuppositionMapping;
    /** For a check: the sequence's state labels used by the reading -> this check's. */
    public Map<String, String> stateMap;
    /** What the collapse did (I7.6). */
    public String collapseReport;
    /**
     * Pronouns in this item the mapping did not bind, as they are named in {@link #semantic}.
     * Their ant(x) marker was dropped, so each is a new referent, equal to no other. Empty on a
     * fully resolved item. Clients must surface it.
     */
    public List<String> unresolvedPronouns = List.of();

    public GswbTptpBatchResult() {
    }
}
