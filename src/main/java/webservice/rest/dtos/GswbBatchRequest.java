package webservice.rest.dtos;

import java.util.HashMap;

/**
 * A batch deduction: one {@link GswbRequest} per sentence id.
 *
 * The per-sentence unit is deliberately the SAME type {@code /deduce} takes, so
 * {@code /gswb_batch_proof} is N single calls with shared setup rather than a second,
 * weaker deduction path. It used to be {@code HashMap<String,String> premises} -- flat
 * meaning constructors with no structure and no proofs -- which meant no per-origin
 * provenance, no surface labels on scope discriminants (resolveSurfaceLabel returns null
 * without a structure) and bare {@code s0} solution ids repeated across every sentence.
 *
 * An item may omit {@code gswbPreferences}; the batch-level ones are used then.
 */
public class GswbBatchRequest {

    public HashMap<String, GswbRequest> items;
    public GswbPreferences gswbPreferences;
    public String sessionKey;

    public GswbBatchRequest() {}

    public GswbBatchRequest(HashMap<String, GswbRequest> items, GswbPreferences settings) {
        this.items = items;
        this.gswbPreferences = settings;
    }

    public GswbBatchRequest(HashMap<String, GswbRequest> items, GswbPreferences settings, String sessionKey) {
        this.items = items;
        this.gswbPreferences = settings;
        this.sessionKey = sessionKey;
    }
}
