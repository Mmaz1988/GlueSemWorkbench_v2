package test;

import org.junit.jupiter.api.Test;
import webservice.rest.PossibleAccFacts;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;

public class PossibleAccFactsTest {

    private static Map<String, Object> fact(String source, String label, String target) {
        Map<String, Object> fact = new LinkedHashMap<>();
        fact.put("sourceNode", source);
        fact.put("relationLabel", label);
        fact.put("targetNode", target);
        return fact;
    }

    /** The shape GSWB compiles "The PC-6082 is faster ..." to: one MERGE chain from the root
     *  (s0 -> s0a -> s0b -> s3), presupposition boxes hanging off it by PRSP. */
    private static LinkedHashMap<String, Object> structure(List<Map<String, Object>> annotations) {
        List<Map<String, Object>> constraints = new ArrayList<>(List.of(
                fact("d1", "NODE_TYPE", "root"), fact("d1", "NAME", "s0"),
                fact("d1", "MERGE", "d2"),
                fact("d2", "PRSP", "d3"), fact("d3", "IN", "d4"), fact("d4", "NAME", "x3"),
                fact("d2", "MERGE", "d8"),
                fact("d8", "PRSP", "d9"), fact("d9", "IN", "d10"), fact("d10", "NAME", "x4"),
                fact("d8", "MERGE", "d14"),
                fact("d14", "IN", "d15"), fact("d15", "NAME", "x1"),
                fact("d14", "NOT", "d20"), fact("d20", "IN", "d21"), fact("d21", "NAME", "x5")));
        LinkedHashMap<String, Object> structure = new LinkedHashMap<>();
        structure.put("constraints", constraints);
        structure.put("annotations", annotations);
        return structure;
    }

    @Test
    void everyStateOfTheRootChainNamesTheMainBox() {
        Map<String, LinkedHashSet<String>> sites = PossibleAccFacts.extract(structure(List.of(
                fact("d3", "POSSIBLE-ACC", "d1"), fact("d3", "POSSIBLE-ACC", "d2"),
                fact("d9", "POSSIBLE-ACC", "d1"))));
        assertEquals(Map.of("x3", Set.of("x1"), "x4", Set.of("x1")), sites);
    }

    @Test
    void aNegatedBoxIsASiteOfItsOwn() {
        Map<String, LinkedHashSet<String>> sites = PossibleAccFacts.extract(structure(List.of(
                fact("d3", "POSSIBLE-ACC", "d1"), fact("d3", "POSSIBLE-ACC", "d20"))));
        assertEquals(List.of("x1", "x5"), new ArrayList<>(sites.get("x3")));
    }

    @Test
    void aRootChainWithoutReferentsIsTheOutermostBox() {
        LinkedHashMap<String, Object> structure = new LinkedHashMap<>();
        structure.put("constraints", List.of(
                fact("d1", "NODE_TYPE", "root"), fact("d1", "MERGE", "d2"),
                fact("d2", "PRSP", "d3"), fact("d3", "IN", "d4"), fact("d4", "NAME", "x1")));
        structure.put("annotations", List.of(fact("d3", "POSSIBLE-ACC", "d1")));
        assertEquals(Map.of("x1", Set.of("s0")), PossibleAccFacts.extract(structure));
    }

    @Test
    void noPossibleAccMeansNothingToAccommodate() {
        assertEquals(Map.of(), PossibleAccFacts.extract(structure(List.of())));
    }
}
