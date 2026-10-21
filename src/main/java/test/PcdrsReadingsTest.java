package test;

import org.junit.jupiter.api.Test;
import webservice.rest.GswbController;
import webservice.rest.dtos.GswbPcdrsOutput;
import webservice.rest.dtos.GswbPcdrsRequest;
import webservice.rest.dtos.GswbSolution;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

/**
 * I10 (xleplusglue docs/plans/PRESUPPOSITION_PRONOUN_PIPELINE_PLAN.md): a reading is one choice
 * for every p-box -- in situ, another site the rules allow, or a binder -- and every pronoun.
 * Every possibility is generated, a binder does not suppress accommodation, P: has an entry for
 * every p-box, and the readings the collapse rejects are returned apart, with the reason.
 */
class PcdrsReadingsTest {

    /** "Peter smiled. Peter laughed.": s0, s1 the first Peter (y1), s2 smile (x1); the second
     *  sentence is an update (I3): s3 holds the second Peter's p-box s4 (y2), s5 laugh (x2).
     *  Sites: s0+s2, s3+s5. Facts name referents with their states, as the graph does (s4:y2). */
    private static final String PETER_PETER = "([],[p(([x1],[bind(x1),x1='peter']))]) + ([x2],[smile(x2),arg1(x2,x1)])"
            + " ; ([],[p(([x3],[bind(x3),x3='peter']))]) + ([x4],[laugh(x4),arg1(x4,x3)])";

    /** Facts as LiGER returns them: node d<name> is named <name>. */
    private static LinkedHashMap<String, Object> facts(String... relations) {
        List<Object> annotations = new ArrayList<>();
        for (String relation : relations) {
            String[] parts = relation.split(" ");
            for (String name : List.of(parts[0], parts[2])) {
                annotations.add(Map.of("sourceNode", "d" + name, "relationLabel", "NAME", "targetNode", name));
            }
            annotations.add(Map.of("sourceNode", "d" + parts[0], "relationLabel", parts[1], "targetNode", "d" + parts[2]));
        }
        LinkedHashMap<String, Object> structure = new LinkedHashMap<>();
        structure.put("constraints", new ArrayList<>());
        structure.put("annotations", annotations);
        return structure;
    }

    private static GswbPcdrsOutput readings(String semantic, String... facts) throws Exception {
        GswbPcdrsRequest request = new GswbPcdrsRequest();
        request.semantic = semantic;
        request.parentSolutionId = "test";
        request.mergedStructure = facts(facts);
        return new GswbController().generatePcdrs(request);
    }

    @Test
    void aPresuppositionWhoseContentIsAMergeHasReadings() throws Exception {
        // "Every man saw the dog that liked him.": the relative clause makes the p-box a merge
        // inside a box, which /generate_pcdrs must resolve as well as one at the top. Global
        // accommodation takes "him" (x2, in the consequent) out of its scope: rejected (I9).
        GswbPcdrsOutput output = readings("([],[([x1],[man(x1)]) -> ([x2],[ant(x2),male(x2),"
                + "p(([y1],[bind(y1),dog(y1)]) + ([y2],[like(y2),arg1(y2,y1),arg2(y2,x2)]))])"
                + " + ([x3],[see(x3),arg1(x3,x1),arg2(x3,y1)])])", "s3 POSSIBLE-ACC s0");
        assertEquals(List.of("P:[p(s3,s3)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
        assertEquals(List.of("P:[p(s3,s0)]"),
                output.rejected.stream().map(solution -> solution.presuppositionMapping).toList());
        assertTrue(output.rejected.get(0).rejection.contains("(I9)"), output.rejected.get(0).rejection);
    }

    @Test
    void eachReadingCountsItsChoicesByKindAndDepth() throws Exception {
        // N4. "him" (s2:x2) in the consequent, "every man" (s1:x1) in the restrictor: the rules
        // call that pair bound. The dog's p-box (s3) stands in the consequent: in situ is local.
        // Binding the dog to the man takes "him" out of its scope (I9): rejected.
        GswbPcdrsOutput output = readings("([],[([x1],[man(x1)]) -> ([x2],[ant(x2),male(x2),"
                + "p(([y1],[bind(y1),dog(y1)]) + ([y2],[like(y2),arg1(y2,y1),arg2(y2,x2)]))])"
                + " + ([x3],[see(x3),arg1(x3,x1),arg2(x3,y1)])])",
                "s2:x2 POSSIBLE-ANT s1:x1", "s2:x2 BOUND-ANT s1:x1", "s3:y1 POSSIBLE-ANT s1:x1");
        assertEquals(List.of(
                        "A:[a(s2:x2,x1)] [1, 0, 0] [0, 0, 0] [0, 0, 1]",
                        "A:[a(s2:x2,/)] [0, 0, 1] [0, 0, 0] [0, 0, 1]"),
                output.solutions.stream().map(solution -> solution.anaphoraMapping + " "
                        + java.util.Arrays.toString(solution.pragmaticProfile.pronouns) + " "
                        + java.util.Arrays.toString(solution.pragmaticProfile.binding) + " "
                        + java.util.Arrays.toString(solution.pragmaticProfile.accommodation)).toList());
    }

    @Test
    void aBinderDoesNotSuppressAccommodation() throws Exception {
        GswbPcdrsOutput output = readings(PETER_PETER, "s4:y2 POSSIBLE-ANT s1:y1", "s4 POSSIBLE-ACC s0");
        assertEquals(List.of("P:[p(s1,s1),p(s4,s4)]", "P:[p(s1,s1),p(s4,s0)]", "P:[p(s1,s1),p(s4,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
        GswbSolution bound = output.solutions.get(2);
        assertTrue(bound.semantic.endsWith("A:[a(s4:y2,y1)],P:[p(s1,s1),p(s4,s0)]"), bound.semantic);
        // N4: the first Peter accommodated globally (in situ at the top), the second bound there.
        assertArrayEquals(new int[]{1, 0, 0}, bound.pragmaticProfile.binding);
        assertArrayEquals(new int[]{1, 0, 0}, bound.pragmaticProfile.accommodation);
        assertArrayEquals(new int[]{0, 0, 0}, bound.pragmaticProfile.pronouns);
        assertEquals("s1 accommodated at s0; s4 bound at s0 (s4:y2 ↦ s1:y1);"
                + " renamed s1:y1 → s0:x1, s2:x1 → s0:x2, s5:x2 → s0:x3", bound.collapseReport);
        assertEquals("s1 accommodated at s0; s4 accommodated at s0;"
                + " renamed s1:y1 → s0:x1, s4:y2 → s0:x2, s2:x1 → s0:x3, s5:x2 → s0:x4", output.solutions.get(1).collapseReport);
        assertTrue(output.rejected.isEmpty());
    }

    @Test
    void aSiteThatIsTheHostIsInSituOnce() throws Exception {
        // s1 stands in s0: the rules' s0 is its in-situ reading, not a second one.
        GswbPcdrsOutput output = readings(PETER_PETER, "s1 POSSIBLE-ACC s0");
        assertEquals(List.of("P:[p(s1,s1),p(s4,s4)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
    }

    @Test
    void aSiteIsOneChoiceWhicheverOfItsStatesTheRulesName() throws Exception {
        // s0 and s2 are one site (I3): one reading for both.
        GswbPcdrsOutput output = readings(PETER_PETER, "s4 POSSIBLE-ACC s0", "s4 POSSIBLE-ACC s2");
        assertEquals(List.of("P:[p(s1,s1),p(s4,s4)]", "P:[p(s1,s1),p(s4,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
    }

    @Test
    void readingsWithTheSameCollapseAndSitesAreOne() throws Exception {
        // "Bill saw Bill. Bill smiled.": y3 bound to y2 bound to y1 is y3 bound to y1.
        String bills = "([],[p(([x1],[bind(x1),x1='bill'])),p(([x2],[bind(x2),x2='bill']))]) + ([x3],[see(x3),arg1(x3,x1),arg2(x3,x2)])"
                + " ; ([],[p(([x4],[bind(x4),x4='bill']))]) + ([x5],[smile(x5),arg1(x5,x4)])";
        GswbPcdrsOutput output = readings(bills, "s2:y2 POSSIBLE-ANT s1:y1", "s5:y3 POSSIBLE-ANT s1:y1", "s5:y3 POSSIBLE-ANT s2:y2");
        List<String> anaphora = output.solutions.stream().map(solution -> solution.anaphoraMapping).toList();
        assertTrue(anaphora.contains("A:[a(s2:y2,y1),a(s5:y3,y1)]"), anaphora.toString());
        assertFalse(anaphora.contains("A:[a(s2:y2,y1),a(s5:y3,y2)]"), anaphora.toString());
        // Bound to the second Bill while that one stands alone is another reading.
        assertTrue(anaphora.contains("A:[a(s5:y3,y2)]"), anaphora.toString());
    }

    @Test
    void aBindingTheCollapseRejectsIsReturnedApart() throws Exception {
        // "No man smiled. The man laughed.": the man in the negation is off the p-box's path.
        GswbPcdrsOutput output = readings("([],[~(([x1,x2],[man(x1),smile(x2),arg1(x2,x1)]))])"
                + " ; ([],[p(([x3],[bind(x3),man(x3)]))]) + ([x4],[laugh(x4),arg1(x4,x3)])", "s3:y1 POSSIBLE-ANT s1:x1", "s3 POSSIBLE-ACC s0");
        assertEquals(List.of("P:[p(s3,s3)]", "P:[p(s3,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
        assertEquals(1, output.rejected.size());
        // Off the pronoun's path a plain x1 names nothing: the rejected reading keeps the states.
        assertEquals("A:[a(s3:y1,s1:x1)]", output.rejected.get(0).anaphoraMapping);
        assertTrue(output.rejected.get(0).rejection.contains("s3"), output.rejected.get(0).rejection);
    }

    @Test
    void severalBindersGiveSeveralReadings() throws Exception {
        // "A man saw a man. The man smiled."
        GswbPcdrsOutput output = readings("([x1,x2,x3],[man(x1),man(x2),see(x3),arg1(x3,x1),arg2(x3,x2)]) + ([x4],"
                + "[p(([x5],[bind(x5),man(x5)])),smile(x4),arg1(x4,x5)])", "s2:y1 POSSIBLE-ANT s0:x1", "s2:y1 POSSIBLE-ANT s0:x2");
        assertEquals(List.of("", "A:[a(s2:y1,x1)]", "A:[a(s2:y1,x2)]"),
                output.solutions.stream().map(solution -> solution.anaphoraMapping).toList());
    }
}
