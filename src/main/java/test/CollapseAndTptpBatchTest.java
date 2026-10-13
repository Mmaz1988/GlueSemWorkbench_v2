package test;

import org.junit.jupiter.api.Test;
import webservice.rest.GswbController;
import webservice.rest.dtos.GswbCollapseAndTptpBatchRequest;
import webservice.rest.dtos.GswbTptpBatchItem;
import webservice.rest.dtos.GswbTptpBatchResult;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/** /collapse_and_tptp_batch reads a pronoun its mapping does not bind as a new referent: the
 *  ant(x) marker is dropped, the referent stays existential, and the item names the pronoun.
 *  The mapping comes inline with the sequence; a rejected reading has no TPTP and says why. */
class CollapseAndTptpBatchTest {

    private static final String SEQUENCE =
            "([x1,x2,x3],[big(x1),cat(x1),black(x2),dog(x2),eat(x3),arg2(x3,x2),arg1(x3,x1)])"
                    + " + ([x4,x5],[ant(x4),bark(x5),arg1(x5,x4)])";

    private static GswbTptpBatchResult translate(String mapping) throws Exception {
        GswbCollapseAndTptpBatchRequest request = new GswbCollapseAndTptpBatchRequest();
        request.parentSolutionId = "test";
        GswbTptpBatchItem item = new GswbTptpBatchItem();
        item.name = "sequence";
        item.semantic = SEQUENCE + mapping;
        request.items = List.of(item);
        return new GswbController().collapseAndTptpBatch(request).results.get("sequence");
    }

    @Test
    void readsAnUnmappedPronounAsANewReferent() throws Exception {
        GswbTptpBatchResult result = translate("");

        assertNull(result.rejection);
        assertFalse(result.tptp.contains("ant("), result.tptp);
        assertFalse(result.tptp.contains("="), result.tptp);
        assertTrue(result.tptp.contains("?[X4]"), result.tptp);
        assertEquals(List.of("x4"), result.unresolvedPronouns);
    }

    @Test
    void collapsesAMappedPronounAndReportsNothing() throws Exception {
        GswbTptpBatchResult result = translate(",A:[a(x4,x1)]");

        assertNull(result.rejection);
        assertFalse(result.tptp.contains("ant("), result.tptp);
        assertTrue(result.tptp.contains("X4 = X1"), result.tptp);
        assertTrue(result.unresolvedPronouns.isEmpty());
    }

    /** A DRS whose merge sits inside a condition -- the antecedent of "every", unresolved
     *  because chat sends the prior with resolveDrs off -- is translated with the merge
     *  resolved, not rejected (2026-10-06: "DrsMerge does not support TPTP translation
     *  directly", and the branch reasoned over without its anaphora binding). */
    @Test
    void resolvesAMergeNestedInsideACondition() throws Exception {
        GswbCollapseAndTptpBatchRequest request = new GswbCollapseAndTptpBatchRequest();
        request.parentSolutionId = "test";
        GswbTptpBatchItem item = new GswbTptpBatchItem();
        item.name = "context";
        item.semantic = "([],[([x1],[]) + ([],[man(x1)]) -> ([],[walk(x1)])])";
        request.items = List.of(item);

        GswbTptpBatchResult result = new GswbController().collapseAndTptpBatch(request).results.get("context");

        assertNull(result.rejection, result.rejection);
        assertTrue(result.tptp.contains("man(X1)") && result.tptp.contains("walk(X1)"), result.tptp);
    }

    @Test
    void aRejectedReadingHasNoTptpAndSaysWhy() throws Exception {
        // "No dog slept. It barked.": "it" bound to the dog inside the negation, which it
        // cannot see -- an unbound occurrence (I9).
        GswbCollapseAndTptpBatchRequest request = new GswbCollapseAndTptpBatchRequest();
        request.items = List.of(item("sequence", "([],[~([x1,x2],[dog(x1),sleep(x2),arg1(x2,x1)])])"
                + " + ([x3,x4],[ant(x3),bark(x4),arg1(x4,x3)]),A:[a(x3,x1)]"));
        GswbTptpBatchResult result = new GswbController().collapseAndTptpBatch(request).results.get("sequence");

        assertEquals("", result.tptp);
        assertNotNull(result.rejection);
    }

    /** "Peter smiled." + "Peter laughed.": each check collapses with the sequence's reading,
     *  rebased (I11), and the context with Q's own part of it. */
    @Test
    void theChecksAndTheContextInheritTheSequencesReading() throws Exception {
        String q = "([],[p(([x1],[bind(x1),x1='peter']))]) + ([x2],[smile(x2),arg1(x2,x1)])";
        GswbCollapseAndTptpBatchRequest request = new GswbCollapseAndTptpBatchRequest();
        request.parentSolutionId = "test";
        request.items = List.of(
                item("context", q),
                // The hypothesis is an update (I3): its p-box s4 stands in its own s3.
                item("sequence", q + " ; ([],[p(([x3],[bind(x3),x3='peter']))]) + ([x4],[laugh(x4),arg1(x4,x3)]),"
                        + "A:[a(x3,x1)],P:[p(s1,s1),p(s4,s0)]"),
                item("cons_neg_check", "([],[([],[p(([x1],[bind(x1),x1='peter']))]) + ([x2],[smile(x2),arg1(x2,x1)])"
                        + " -> ([],[~(([],[p(([x3],[bind(x3),x3='peter']))]) + ([x4],[laugh(x4),arg1(x4,x3)]))])])"));
        var results = new GswbController().collapseAndTptpBatch(request).results;

        GswbTptpBatchResult check = results.get("cons_neg_check");
        assertNull(check.rejection, check.rejection);
        assertEquals("P:[p(s2,s2),p(s6,s1)]", check.presuppositionMapping);
        assertEquals("A:[a(x3,x1)]", check.anaphoraMapping);
        assertEquals("s2 accommodated at s1; s6 bound at s1 (x3 ↦ x1)", check.collapseReport);
        assertEquals("s1", check.stateMap.get("s0"));

        GswbTptpBatchResult context = results.get("context");
        assertEquals("P:[p(s1,s1)]", context.presuppositionMapping);
        assertNull(context.anaphoraMapping);
        assertNull(context.rejection, context.rejection);
    }

    private static GswbTptpBatchItem item(String name, String semantic) {
        GswbTptpBatchItem item = new GswbTptpBatchItem();
        item.name = name;
        item.semantic = semantic;
        return item;
    }
}
