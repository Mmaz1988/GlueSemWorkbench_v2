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
 * cat, a presupposed referent inside the consequent. The graph the rules read names states and
 * referents as the PCDRS prints them (I5/I6: one enumeration), so the cat is x3 in both -- the
 * name the facts use is the name the PCDRS, its anaphoraRelations, its rendering and the
 * collapse use. (Before the one enumeration, the PCDRS renumbered the cat to x5 and x3 printed
 * the person.)
 */
class PcdrsAntecedentNameTest {

    private static final String SEQUENCE =
            "([],[([x1],[person(x1)]) -> ([x2],[p(([x3],[bind(x3),cat(x3)])),see(x2),arg2(x2,x3),arg1(x2,x1)])])"
                    + " + ([x4,x5],[ant(x4),cute(x5),be(x5),arg1(x5,x4)])";

    private static Map<String, Object> constraint(String source, String label, String target) {
        Map<String, Object> constraint = new LinkedHashMap<>();
        constraint.put("sourceNode", source);
        constraint.put("relationLabel", label);
        constraint.put("targetNode", target);
        return constraint;
    }

    /** The rule-annotated structure, reduced to what /generate_pcdrs reads from it: referent
     *  NAMEs (as the graph, and so the PCDRS, numbers them) and the POSSIBLE-ANT fact. */
    private static LinkedHashMap<String, Object> rulesSayItIsTheCat() {
        List<Object> annotations = new ArrayList<>();
        annotations.add(constraint("d9", "NAME", "x3"));
        annotations.add(constraint("d16", "NAME", "x4"));
        annotations.add(constraint("d16", "POSSIBLE-ANT", "d9"));
        LinkedHashMap<String, Object> structure = new LinkedHashMap<>();
        structure.put("constraints", new ArrayList<>());
        structure.put("annotations", annotations);
        return structure;
    }

    private static GswbSolution pcdrs() throws Exception {
        GswbPcdrsRequest request = new GswbPcdrsRequest();
        request.semantic = SEQUENCE;
        request.parentSolutionId = "test";
        request.mergedStructure = rulesSayItIsTheCat();
        GswbPcdrsOutput output = new GswbController().generatePcdrs(request);
        assertEquals(1, output.solutions.size());
        return output.solutions.get(0);
    }

    @Test
    void pcdrsNamesTheCatAsItPrints() throws Exception {
        GswbSolution solution = pcdrs();
        // One enumeration: person x1, seeing x2, the cat x3, "it" x4 -- in the graph and here.
        assertTrue(solution.semantic.contains("p(([x3],[bind(x3),cat(x3)]))"), solution.semantic);
        assertTrue(solution.semantic.contains("arg2(x2,x3)"), solution.semantic);
        assertTrue(solution.semantic.endsWith("A:[a(x4,x3)]"), solution.semantic);

        // The rendered PCDRS -- what the analysis view shows -- names the cat the same way.
        assertTrue(solution.solution.contains("x4 ↦ x3"), solution.solution);
        assertFalse(solution.solution.contains("↦ x1"), solution.solution);

        assertEquals(1, solution.anaphoraRelations.size());
        AnaphoraRelationDto relation = solution.anaphoraRelations.get(0);
        assertEquals("x4", relation.pronounDisplay);
        assertEquals("x3", relation.antecedent);
    }

    @Test
    void collapseBindsThePronounToTheCatNotThePerson() throws Exception {
        GswbSolution solution = pcdrs();
        GswbCollapseAnaphoraRequest request = new GswbCollapseAnaphoraRequest();
        request.semantic = solution.semantic;
        request.parentSolutionId = solution.id;
        request.anaphoraRelations = solution.anaphoraRelations;
        String collapsed = new GswbController().collapseAnaphora(request).semantic;

        // The collapse accommodates the cat in situ (no P: here) and equates "it" with it. The
        // cat is not accessible from the next sentence without accommodating it globally, so
        // the equation is unbound (I9): the collapse reports that and returns the branch
        // without the mapping; "it" is then its own referent, never the person.
        assertFalse(collapsed.contains("=x1"), collapsed);
        assertFalse(collapsed.contains("ant("), collapsed);
    }
}
