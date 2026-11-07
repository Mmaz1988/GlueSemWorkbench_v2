package test;

import org.junit.jupiter.api.Test;
import prover.SolutionObject;
import webservice.rest.GswbController;
import webservice.rest.dtos.GswbProofInput;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;

// The prover orders readings within one MC set; the controller orders a sentence's MC sets by
// their best reading, keeping each set whole. Observed 2026-10-06: "A woman thought that a man saw
// himself" listed the relaxed-retry set (insitu 1, surface 2) first and the only fully
// surface-aligned reading (0, 0) last, because sets came out in request order.
class McSetOrderingTest {

    private static SolutionObject reading(int insitu, int surface) {
        SolutionObject so = new SolutionObject(null);
        so.insituViolations = insitu;
        so.surfaceOrderViolations = surface;
        return so;
    }

    private static GswbProofInput origin(String sentenceId) {
        GswbProofInput input = new GswbProofInput();
        input.sentenceId = sentenceId;
        return input;
    }

    @Test
    void theSetWithTheMostSurfaceFaithfulReadingComesFirstAndSetsStayWhole() {
        LinkedHashMap<Integer, List<SolutionObject>> sets = new LinkedHashMap<>();
        sets.put(0, List.of(reading(1, 2), reading(1, 3), reading(2, 3)));
        sets.put(1, List.of(reading(0, 1), reading(0, 2)));
        sets.put(2, List.of(reading(2, 2)));
        sets.put(3, List.of(reading(0, 0)));
        sets.put(4, List.of());
        Map<Integer, GswbProofInput> origins = new HashMap<>();
        for (Integer key : sets.keySet()) {
            origins.put(key, origin("S29"));
        }

        LinkedHashMap<Integer, List<SolutionObject>> ordered = GswbController.orderMcSetsByViolations(sets, origins);

        assertEquals(List.of(3, 1, 0, 2, 4), new ArrayList<>(ordered.keySet()),
                "by best reading, insitu before surface; a set without readings goes last");
        for (Integer key : sets.keySet()) {
            assertEquals(sets.get(key), ordered.get(key), "each set keeps its readings in their order");
        }
    }

    @Test
    void equallyRankedSetsKeepTheirOrder() {
        LinkedHashMap<Integer, List<SolutionObject>> sets = new LinkedHashMap<>();
        sets.put(0, List.of(reading(0, 1)));
        sets.put(1, List.of(reading(0, 1)));
        sets.put(2, List.of(reading(0, 0)));

        LinkedHashMap<Integer, List<SolutionObject>> ordered = GswbController.orderMcSetsByViolations(sets, Map.of());

        assertEquals(List.of(2, 0, 1), new ArrayList<>(ordered.keySet()));
    }

    @Test
    void sentencesAreNotReorderedAgainstEachOther() {
        LinkedHashMap<Integer, List<SolutionObject>> sets = new LinkedHashMap<>();
        sets.put(0, List.of(reading(0, 3)));
        sets.put(1, List.of(reading(0, 1)));
        sets.put(2, List.of(reading(0, 0)));
        sets.put(3, List.of(reading(0, 2)));
        Map<Integer, GswbProofInput> origins = Map.of(0, origin("S1"), 1, origin("S1"), 2, origin("S2"), 3, origin("S2"));

        LinkedHashMap<Integer, List<SolutionObject>> ordered = GswbController.orderMcSetsByViolations(sets, origins);

        assertEquals(List.of(1, 0, 2, 3), new ArrayList<>(ordered.keySet()),
                "S1's sets stay ahead of S2's, each sentence sorted on its own");
    }
}
