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
 * cat, a presupposed referent inside the consequent. /generate_pcdrs resolves and renumbers the
 * sequence, so the cat's internal name (x3) is no longer what it prints as (x5) -- and x3 now
 * prints the person. The PCDRS and the anaphoraRelations it hands out must name the cat as
 * printed, since /collapse_anaphora reapplies them to the re-parsed, printed DRS.
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
     *  NAMEs (the semantic's own, unrenumbered names) and the POSSIBLE-ANT fact. */
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
        // Renumbered: the pronoun prints x1, the person x3, the cat x5.
        assertTrue(solution.semantic.contains("p(([x5],[bind(x5),cat(x5)]))"), solution.semantic);
        assertTrue(solution.semantic.contains("arg2(x4,x5)"), solution.semantic);
        assertTrue(solution.semantic.endsWith("A:[a(x1,x5)]"), solution.semantic);

        assertEquals(1, solution.anaphoraRelations.size());
        AnaphoraRelationDto relation = solution.anaphoraRelations.get(0);
        assertEquals("x1", relation.pronounDisplay);
        assertEquals("x5", relation.antecedent);
    }

    @Test
    void collapseBindsThePronounToTheCatNotThePerson() throws Exception {
        GswbSolution solution = pcdrs();
        GswbCollapseAnaphoraRequest request = new GswbCollapseAnaphoraRequest();
        request.semantic = solution.semantic;
        request.parentSolutionId = solution.id;
        request.anaphoraRelations = solution.anaphoraRelations;
        String collapsed = new GswbController().collapseAnaphora(request).semantic;

        assertTrue(collapsed.contains("x1=x5"), collapsed);
        assertFalse(collapsed.contains("x1=x3"), collapsed);
        assertTrue(collapsed.contains("arg2(x4,x5)"), collapsed);
    }
}
