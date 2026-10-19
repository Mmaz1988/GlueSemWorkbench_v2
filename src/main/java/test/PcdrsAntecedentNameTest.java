package test;

import org.junit.jupiter.api.Test;
import webservice.rest.GswbController;
import webservice.rest.dtos.AnaphoraRelationDto;
import webservice.rest.dtos.GswbCollapseAnaphoraRequest;
import webservice.rest.dtos.GswbPcdrsOutput;
import webservice.rest.dtos.GswbPcdrsRequest;
import webservice.rest.dtos.GswbSolution;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * "every person saw the cat" + "it was cute" (2026-10-09): the rules bind the pronoun to the
 * cat, a presupposed referent inside the consequent, which they also allow globally. The graph the rules read names states and
 * referents as the PCDRS prints them (I5/I6: one enumeration), so the cat is y1 in both -- the
 * name the facts use is the name the PCDRS, its anaphoraRelations, its rendering and the
 * collapse use. (Before the one enumeration, the PCDRS renumbered the cat to x5 and x3 printed
 * the person.)
 */
class PcdrsAntecedentNameTest {

    private static final String SEQUENCE =
            "([],[([x1],[person(x1)]) -> ([x2],[p(([x3],[bind(x3),cat(x3)])),see(x2),arg2(x2,x3),arg1(x2,x1)])])"
                    + " + ([x4,x5],[ant(x4),cute(x5),be(x5),arg1(x5,x4)])";

    /** The cat accommodated globally; the collapse, one box, numbers its universe first. */
    private static final String CAT_GLOBAL = "s3 accommodated at s0; renamed s3:y1 → s0:x1, s4:y2 → s0:x2,"
            + " s4:x1 → s0:x3, s1:x1 → s1:x4, s2:x2 → s2:x5";

    private static Map<String, Object> constraint(String source, String label, String target) {
        Map<String, Object> constraint = new LinkedHashMap<>();
        constraint.put("sourceNode", source);
        constraint.put("relationLabel", label);
        constraint.put("targetNode", target);
        return constraint;
    }

    /** The rule-annotated structure, reduced to what /generate_pcdrs reads from it: NAMEs (as
     *  the graph, and so the PCDRS, numbers referents and states), the states that introduce the
     *  two referents (the cat y1 in s3, "it" y2 in s4), the POSSIBLE-ANT fact
     *  and the cat's global site (s3 -> s0). */
    private static LinkedHashMap<String, Object> rulesSayItIsTheCat() {
        List<Object> annotations = new ArrayList<>();
        annotations.add(constraint("d9", "NAME", "y1"));
        annotations.add(constraint("d16", "NAME", "y2"));
        annotations.add(constraint("d22", "NAME", "s4"));
        annotations.add(constraint("d20", "IN", "d9"));
        annotations.add(constraint("d22", "IN", "d16"));
        annotations.add(constraint("d16", "POSSIBLE-ANT", "d9"));
        annotations.add(constraint("d20", "NAME", "s3"));
        annotations.add(constraint("d21", "NAME", "s0"));
        annotations.add(constraint("d20", "POSSIBLE-ACC", "d21"));
        LinkedHashMap<String, Object> structure = new LinkedHashMap<>();
        structure.put("constraints", new ArrayList<>());
        structure.put("annotations", annotations);
        return structure;
    }

    private static GswbPcdrsOutput pcdrs() throws Exception {
        GswbPcdrsRequest request = new GswbPcdrsRequest();
        request.semantic = SEQUENCE;
        request.parentSolutionId = "test";
        request.mergedStructure = rulesSayItIsTheCat();
        return new GswbController().generatePcdrs(request);
    }

    @Test
    void pcdrsNamesTheCatAsItPrints() throws Exception {
        GswbPcdrsOutput output = pcdrs();
        // The cat in situ, in the consequent, is not accessible to "it" (I9): rejected.
        assertEquals(1, output.rejected.size());
        assertEquals("P:[p(s3,s3)]", output.rejected.get(0).presuppositionMapping);
        assertNotNull(output.rejected.get(0).rejection);
        assertEquals(1, output.solutions.size());
        GswbSolution solution = output.solutions.get(0);
        // One enumeration: person x1, seeing x2 (per path), the cat y1, "it" y2 (once across
        // the tree) -- in the graph and here.
        assertTrue(solution.semantic.contains("p(([y1],[bind(y1),cat(y1)]))"), solution.semantic);
        assertTrue(solution.semantic.contains("arg2(x2,y1)"), solution.semantic);
        assertTrue(solution.semantic.endsWith("A:[a(y2,y1)],P:[p(s3,s0)]"), solution.semantic);
        assertEquals(CAT_GLOBAL, solution.collapseReport);

        // The rendered PCDRS -- what the analysis view shows -- names the cat the same way.
        assertTrue(solution.solution.contains("ȳ2 ↦ ȳ1"), solution.solution);
        assertFalse(solution.solution.contains("↦ x1"), solution.solution);

        assertEquals(1, solution.anaphoraRelations.size());
        AnaphoraRelationDto relation = solution.anaphoraRelations.get(0);
        assertEquals("s4:y2", relation.pronounDisplay);
        assertEquals("s3:y1", relation.antecedent);
    }

    @Test
    void collapseBindsThePronounToTheCatNotThePerson() throws Exception {
        GswbSolution solution = pcdrs().solutions.get(0);
        GswbCollapseAnaphoraRequest request = new GswbCollapseAnaphoraRequest();
        request.semantic = solution.semantic;
        request.parentSolutionId = solution.id;
        GswbSolution collapsed = new GswbController().collapseAnaphora(request);

        // The cat global is x1 after renumbering, "it" x2.
        assertNull(collapsed.rejection, collapsed.rejection);
        assertTrue(collapsed.semantic.contains("x2=x1"), collapsed.semantic);
        assertFalse(collapsed.semantic.contains("ant("), collapsed.semantic);
        assertEquals(CAT_GLOBAL, collapsed.collapseReport);
    }

    @Test
    void aRejectedCollapseSaysWhy() throws Exception {
        GswbCollapseAnaphoraRequest request = new GswbCollapseAnaphoraRequest();
        // Names are the text's own: SEQUENCE writes "it" as x4.
        request.semantic = SEQUENCE + ",A:[a(s4:x4,s3:x3)],P:[p(s3,s3)]";
        GswbSolution collapsed = new GswbController().collapseAnaphora(request);
        assertNull(collapsed.semantic);
        assertNotNull(collapsed.rejection);
    }
}
