package test;

import org.junit.jupiter.api.Test;
import webservice.rest.GswbController;
import webservice.rest.dtos.AnaphoraRelationDto;
import webservice.rest.dtos.GswbCollapseAndTptpBatchRequest;
import webservice.rest.dtos.GswbTptpBatchItem;
import webservice.rest.dtos.GswbTptpBatchResult;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/** /collapse_and_tptp_batch reads a pronoun its mapping does not bind as a new referent: the
 *  ant(x) marker is dropped, the referent stays existential, and the item names the pronoun. */
class CollapseAndTptpBatchTest {

    private static final String SEQUENCE =
            "([x1,x2,x3],[big(x1),cat(x1),black(x2),dog(x2),eat(x3),arg2(x3,x2),arg1(x3,x1)])"
                    + " + ([x4,x5],[ant(x4),bark(x5),arg1(x5,x4)])";

    private static GswbTptpBatchResult translate(List<AnaphoraRelationDto> relations) throws Exception {
        GswbCollapseAndTptpBatchRequest request = new GswbCollapseAndTptpBatchRequest();
        request.parentSolutionId = "test";
        request.anaphoraRelations = relations;
        GswbTptpBatchItem item = new GswbTptpBatchItem();
        item.name = "sequence";
        item.semantic = SEQUENCE;
        request.items = List.of(item);
        return new GswbController().collapseAndTptpBatch(request).results.get("sequence");
    }

    @Test
    void readsAnUnmappedPronounAsANewReferent() throws Exception {
        GswbTptpBatchResult result = translate(null);

        assertNull(result.degraded);
        assertFalse(result.tptp.contains("ant("), result.tptp);
        assertFalse(result.tptp.contains("="), result.tptp);
        assertTrue(result.tptp.contains("?[X4]"), result.tptp);
        assertEquals(List.of("x4"), result.unresolvedPronouns);
    }

    @Test
    void collapsesAMappedPronounAndReportsNothing() throws Exception {
        GswbTptpBatchResult result = translate(List.of(new AnaphoraRelationDto("s1:4", "x4", "x1", "s1")));

        assertNull(result.degraded);
        assertFalse(result.tptp.contains("ant("), result.tptp);
        assertTrue(result.tptp.contains("X4 = X1"), result.tptp);
        assertTrue(result.unresolvedPronouns.isEmpty());
    }
}
