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
 * for every p-box -- a site the rules allow (in situ when it is the host's box), or a binder --
 * and every pronoun. GSWB adds no site of its own (I5); without rules there is one default reading.
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

    private static GswbPcdrsRequest request(String semantic, Boolean rulesApplied, String... facts) {
        GswbPcdrsRequest request = new GswbPcdrsRequest();
        request.semantic = semantic;
        request.parentSolutionId = "test";
        request.mergedStructure = facts(facts);
        request.rulesApplied = rulesApplied;
        return request;
    }

    private static GswbPcdrsOutput readings(String semantic, String... facts) throws Exception {
        return new GswbController().generatePcdrs(request(semantic, true, facts));
    }

    /** "Every man saw the dog.": s0, s1 the restrictor, s2 the consequent with the dog's p-box s3
     *  (y1), s4 the seeing. */
    private static final String EVERY_MAN_THE_DOG = "([],[([x1],[man(x1)]) -> ([],[p(([y1],[bind(y1),dog(y1)]))])"
            + " + ([x2],[see(x2),arg1(x2,x1),arg2(x2,y1)])])";

    @Test
    void aPresuppositionWhoseContentIsAMergeHasReadings() throws Exception {
        // "Every man saw the dog that liked him.": the relative clause makes the p-box a merge
        // inside a box, which /generate_pcdrs must resolve as well as one at the top. Global
        // accommodation takes "him" (x2, in the consequent) out of its scope: rejected (I9).
        GswbPcdrsOutput output = readings("([],[([x1],[man(x1)]) -> ([x2],[ant(x2),male(x2),"
                + "p(([y1],[bind(y1),dog(y1)]) + ([y2],[like(y2),arg1(y2,y1),arg2(y2,x2)]))])"
                + " + ([x3],[see(x3),arg1(x3,x1),arg2(x3,y1)])])", "s3 POSSIBLE-ACC s2", "s3 POSSIBLE-ACC s0");
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
                "s2:x2 POSSIBLE-ANT s1:x1", "s2:x2 BOUND-ANT s1:x1", "s3:y1 POSSIBLE-ANT s1:x1", "s3 POSSIBLE-ACC s2");
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
        GswbPcdrsOutput output = readings(PETER_PETER, "s1 POSSIBLE-ACC s0", "s4:y2 POSSIBLE-ANT s1:y1", "s4 POSSIBLE-ACC s0");
        assertEquals(List.of("P:[p(s1,s1),p(s4,s0)]", "P:[p(s1,s1),p(s4,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
        GswbSolution bound = output.solutions.get(1);
        assertTrue(bound.semantic.endsWith("A:[a(s4:y2,y1)],P:[p(s1,s1),p(s4,s0)]"), bound.semantic);
        // N4: the first Peter accommodated globally (in situ at the top), the second bound there.
        assertArrayEquals(new int[]{1, 0, 0}, bound.pragmaticProfile.binding);
        assertArrayEquals(new int[]{1, 0, 0}, bound.pragmaticProfile.accommodation);
        assertArrayEquals(new int[]{0, 0, 0}, bound.pragmaticProfile.pronouns);
        assertEquals("s1 accommodated at s0; s4 bound at s0 (s4:y2 ↦ s1:y1);"
                + " renamed s1:y1 → s0:x1, s2:x1 → s0:x2, s5:x2 → s0:x3", bound.collapseReport);
        assertEquals("s1 accommodated at s0; s4 accommodated at s0;"
                + " renamed s1:y1 → s0:x1, s4:y2 → s0:x2, s2:x1 → s0:x3, s5:x2 → s0:x4", output.solutions.get(0).collapseReport);
        assertTrue(output.rejected.isEmpty());
    }

    @Test
    void aSiteThatIsTheHostIsInSitu() throws Exception {
        // s1 stands in s0: the rules' s0 is its in-situ reading. s4 stands in s3, the second
        // sentence's top, which is the global box: in situ is written s0 there, so the checks
        // rebase it into Q's top box (I11).
        GswbPcdrsOutput output = readings(PETER_PETER, "s1 POSSIBLE-ACC s0", "s4 POSSIBLE-ACC s3");
        assertEquals(List.of("P:[p(s1,s1),p(s4,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
    }

    @Test
    void aBoxIsOneChoiceWhicheverOfItsStatesTheRulesName() throws Exception {
        // s0, s2 and s4's host s3 are one box, the global one (I10): one reading for all.
        GswbPcdrsOutput output = readings(PETER_PETER, "s1 POSSIBLE-ACC s0", "s4 POSSIBLE-ACC s0", "s4 POSSIBLE-ACC s2");
        assertEquals(List.of("P:[p(s1,s1),p(s4,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
    }

    @Test
    void readingsWithTheSameCollapseAndSitesAreOne() throws Exception {
        // "Bill saw Bill. Bill smiled.": y3 bound to y2 bound to y1 is y3 bound to y1.
        String bills = "([],[p(([x1],[bind(x1),x1='bill'])),p(([x2],[bind(x2),x2='bill']))]) + ([x3],[see(x3),arg1(x3,x1),arg2(x3,x2)])"
                + " ; ([],[p(([x4],[bind(x4),x4='bill']))]) + ([x5],[smile(x5),arg1(x5,x4)])";
        GswbPcdrsOutput output = readings(bills, "s1 POSSIBLE-ACC s0", "s2 POSSIBLE-ACC s0", "s5 POSSIBLE-ACC s4",
                "s2:y2 POSSIBLE-ANT s1:y1", "s5:y3 POSSIBLE-ANT s1:y1", "s5:y3 POSSIBLE-ANT s2:y2");
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
        // In situ at the second sentence's top is the global box: one accommodation reading.
        assertEquals(List.of("P:[p(s3,s0)]"),
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
                + "[p(([x5],[bind(x5),man(x5)])),smile(x4),arg1(x4,x5)])", "s2 POSSIBLE-ACC s1", "s2:y1 POSSIBLE-ANT s0:x1", "s2:y1 POSSIBLE-ANT s0:x2");
        assertEquals(List.of("", "A:[a(s2:y1,x1)]", "A:[a(s2:y1,x2)]"),
                output.solutions.stream().map(solution -> solution.anaphoraMapping).toList());
    }

    @Test
    void noInSituReadingUnlessTheRulesProposeTheHost() throws Exception {
        // The rules allow the dog only globally: in situ, in the consequent, is not a reading.
        GswbPcdrsOutput output = readings(EVERY_MAN_THE_DOG, "s3 POSSIBLE-ACC s0");
        assertEquals(List.of("P:[p(s3,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
        assertTrue(output.rejected.isEmpty());
    }

    @Test
    void inSituComesFirstWhereverTheRulesNameTheHost() throws Exception {
        GswbPcdrsOutput output = readings(EVERY_MAN_THE_DOG, "s3 POSSIBLE-ACC s0", "s3 POSSIBLE-ACC s2");
        assertEquals(List.of("P:[p(s3,s3)]", "P:[p(s3,s0)]"),
                output.solutions.stream().map(solution -> solution.presuppositionMapping).toList());
    }

    @Test
    void aPresuppositionTheRulesGiveNoSiteAndNoBinderIsRejected() throws Exception {
        GswbPcdrsOutput output = readings(EVERY_MAN_THE_DOG);
        assertTrue(output.solutions.isEmpty());
        assertEquals(1, output.rejected.size());
        assertTrue(output.rejected.get(0).rejection.startsWith("s3: "), output.rejected.get(0).rejection);
    }

    @Test
    void withoutRulesThereIsTheDefaultReading() throws Exception {
        // Every p-box in situ, every pronoun unbound; a fact the structure holds is not a rule's.
        GswbPcdrsOutput output = new GswbController().generatePcdrs(request("([],[([x1],[man(x1)]) -> ([x2],[ant(x2),male(x2),"
                + "p(([y1],[bind(y1),dog(y1)]))]) + ([x3],[see(x3),arg1(x3,x1),arg2(x3,y1)])])", false, "s3 POSSIBLE-ACC s0",
                "s2:x2 POSSIBLE-ANT s1:x1"));
        assertEquals(List.of("A:[a(s2:x2,/)] P:[p(s3,s3)]"),
                output.solutions.stream().map(solution -> solution.anaphoraMapping + " " + solution.presuppositionMapping).toList());
    }

    @Test
    void whetherRulesRanMustBeSaid() {
        IllegalArgumentException error = assertThrows(IllegalArgumentException.class,
                () -> new GswbController().generatePcdrs(request(EVERY_MAN_THE_DOG, null, "s3 POSSIBLE-ACC s0")));
        assertTrue(error.getMessage().contains("rulesApplied"), error.getMessage());
    }

    @Test
    void withoutSvgTheReadingsAreTheSameButUnrendered() throws Exception {
        String[] facts = {"s3 POSSIBLE-ACC s0", "s3 POSSIBLE-ACC s1", "s3 POSSIBLE-ACC s2"};
        GswbPcdrsOutput rendered = readings(EVERY_MAN_THE_DOG, facts);
        GswbPcdrsRequest request = request(EVERY_MAN_THE_DOG, true, facts);
        request.includeSvg = false;
        GswbPcdrsOutput unrendered = new GswbController().generatePcdrs(request);

        assertEquals(3, rendered.solutions.size());
        assertTrue(rendered.solutions.stream().allMatch(solution -> solution.solution != null));
        assertTrue(unrendered.solutions.stream().allMatch(solution -> solution.solution == null));
        java.util.function.Function<GswbPcdrsOutput, List<String>> content = output -> output.solutions.stream()
                .map(solution -> solution.id + " " + solution.semantic + " " + solution.anaphoraMapping + " "
                        + solution.presuppositionMapping + " " + solution.collapseReport)
                .toList();
        assertEquals(content.apply(rendered), content.apply(unrendered));
    }
}
