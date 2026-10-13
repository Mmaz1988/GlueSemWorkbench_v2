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

    /** "Peter smiled. Peter laughed.": s0, s1 the first Peter (x1), s2 smile; the second sentence
     *  is an update (I3): s3 holds the second Peter's p-box s4 (x3), s5 laugh. Sites: s0+s2, s3+s5. */
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
    void aBinderDoesNotSuppressAccommodation() throws Exception {
        GswbPcdrsOutput output = readings(PETER_PETER, "x3 POSSIBLE-ANT x1", "s4 POSSIBLE-ACC s0");
        assertEquals(List.of("P:[p(s1,s1),p(s4,s4)]", "P:[p(s1,s1),p(s4,s0)]", "P:[p(s1,s1),p(s4,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
        GswbSolution bound = output.solutions.get(2);
        assertTrue(bound.semantic.endsWith("A:[a(x3,x1)],P:[p(s1,s1),p(s4,s0)]"), bound.semantic);
        assertEquals("s1 accommodated at s0; s4 bound at s0 (x3 ↦ x1)", bound.collapseReport);
        assertEquals("s1 accommodated at s0; s4 accommodated at s0", output.solutions.get(1).collapseReport);
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
        // "Bill saw Bill. Bill smiled.": x4 bound to x2 bound to x1 is x4 bound to x1.
        String bills = "([],[p(([x1],[bind(x1),x1='bill'])),p(([x2],[bind(x2),x2='bill']))]) + ([x3],[see(x3),arg1(x3,x1),arg2(x3,x2)])"
                + " ; ([],[p(([x4],[bind(x4),x4='bill']))]) + ([x5],[smile(x5),arg1(x5,x4)])";
        GswbPcdrsOutput output = readings(bills, "x2 POSSIBLE-ANT x1", "x4 POSSIBLE-ANT x1", "x4 POSSIBLE-ANT x2");
        List<String> anaphora = output.solutions.stream().map(solution -> solution.anaphoraMapping).toList();
        assertTrue(anaphora.contains("A:[a(x2,x1),a(x4,x1)]"), anaphora.toString());
        assertFalse(anaphora.contains("A:[a(x2,x1),a(x4,x2)]"), anaphora.toString());
        // Bound to the second Bill while that one stands alone is another reading.
        assertTrue(anaphora.contains("A:[a(x4,x2)]"), anaphora.toString());
    }

    @Test
    void aBindingTheCollapseRejectsIsReturnedApart() throws Exception {
        // "No man smiled. The man laughed.": the man in the negation is off the p-box's path.
        GswbPcdrsOutput output = readings("([],[~(([x1,x2],[man(x1),smile(x2),arg1(x2,x1)]))])"
                + " ; ([],[p(([x3],[bind(x3),man(x3)]))]) + ([x4],[laugh(x4),arg1(x4,x3)])", "x3 POSSIBLE-ANT x1", "s3 POSSIBLE-ACC s0");
        assertEquals(List.of("P:[p(s3,s3)]", "P:[p(s3,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
        assertEquals(1, output.rejected.size());
        assertEquals("A:[a(x3,x1)]", output.rejected.get(0).anaphoraMapping);
        assertTrue(output.rejected.get(0).rejection.contains("s3"), output.rejected.get(0).rejection);
    }

    @Test
    void severalBindersGiveSeveralReadings() throws Exception {
        // "A man saw a man. The man smiled."
        GswbPcdrsOutput output = readings("([x1,x2,x3],[man(x1),man(x2),see(x3),arg1(x3,x1),arg2(x3,x2)]) + ([x4],"
                + "[p(([x5],[bind(x5),man(x5)])),smile(x4),arg1(x4,x5)])", "x5 POSSIBLE-ANT x1", "x5 POSSIBLE-ANT x2");
        assertEquals(List.of("", "A:[a(x5,x1)]", "A:[a(x5,x2)]"),
                output.solutions.stream().map(solution -> solution.anaphoraMapping).toList());
    }
}
