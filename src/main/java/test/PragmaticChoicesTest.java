package test;

import de.ukon.lfgxdrt.drs_elements.Collapse;
import de.ukon.lfgxdrt.drs_elements.PresuppositionBox;
import org.junit.jupiter.api.Test;
import webservice.rest.PragmaticChoices;
import webservice.rest.dtos.PragmaticChoiceDto;

import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

/**
 * D3/D4 (xleplusglue docs/plans/PRAGMATIC_DISCRIMINANTS_PLAN.md): a reading holds A -> X for
 * every X of A's coreference chain -- the equivalence closure of its pronoun links and bindings --
 * that starts before A or is A's own antecedent; A -> / only where A's own link is unbound; a
 * p-box is bound to a chain or accommodated at one site. Keys are the heads' spans.
 */
class PragmaticChoicesTest {

    /** "A man saw a dog. The dog barked. It smiled.": s0 man, s1 dog, s4 the dog's p-box (y1),
     *  s6 it; the boxes of all sentences are the one global box s0. */
    private static final Map<String, PragmaticChoices.Referent> CHAIN1 = new LinkedHashMap<>();

    static {
        CHAIN1.put("s0:x1", referent("man", 2, 5));
        CHAIN1.put("s1:x2", referent("dog", 12, 15));
        CHAIN1.put("s4:y1", referent("The dog", 17, 24));
        CHAIN1.put("s6:x5", referent("It", 33, 35));
    }

    private static final List<PresuppositionBox> THE_DOG = List.of(new PresuppositionBox("s4", "s3", List.of("s4:y1")));

    private static PragmaticChoices.Referent referent(String text, int start, int end) {
        return new PragmaticChoices.Referent(List.of(new int[]{start, end}), text);
    }

    private static Map<String, String> global(String... states) {
        Map<String, String> boxOf = new HashMap<>();
        for (String state : states) {
            boxOf.put(state, "s0");
        }
        return boxOf;
    }

    private static LinkedHashMap<String, String> links(String... pairs) {
        LinkedHashMap<String, String> links = new LinkedHashMap<>();
        for (int i = 0; i < pairs.length; i += 2) {
            links.put(pairs[i], pairs[i + 1]);
        }
        return links;
    }

    private static Collapse.Resolution bound(String box, String from, String to) {
        return new Collapse.Resolution(box, true, "s0", Collapse.Depth.GLOBAL, List.of(new Collapse.Renaming(from, to)));
    }

    private static Collapse.Resolution accommodated(String box, String site, Collapse.Depth depth) {
        return new Collapse.Resolution(box, false, site, depth, List.of());
    }

    private static Set<String> ids(List<PragmaticChoiceDto> choices) {
        Set<String> ids = new TreeSet<>();
        choices.forEach(choice -> ids.add(choice.id));
        return ids;
    }

    private static List<PragmaticChoiceDto> chain1(Map<String, String> anaphora, Collapse.Resolution resolution) {
        return new PragmaticChoices(CHAIN1, THE_DOG, global("s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7"))
                .of(anaphora, List.of(resolution));
    }

    @Test
    void aPronounLinkedToABoundPresuppositionHoldsTheBindersChainToo() {
        // It -> the dog, the dog bound to a dog: "It -> a dog" by the chain, "It -> the dog" its own.
        List<PragmaticChoiceDto> choices = chain1(links("s6:x5", "s4:y1", "s4:y1", "s1:x2"),
                bound("s4", "s4:y1", "s1:x2"));
        assertEquals(Set.of("ANAPHORA@33-35=12-15", "ANAPHORA@33-35=17-24", "PRESUPPOSITION@17-24=12-15"), ids(choices));
        PragmaticChoiceDto own = choices.stream().filter(c -> c.id.equals("ANAPHORA@33-35=17-24")).findFirst().orElseThrow();
        PragmaticChoiceDto chained = choices.stream().filter(c -> c.id.equals("ANAPHORA@33-35=12-15")).findFirst().orElseThrow();
        assertTrue(own.direct);
        assertFalse(chained.direct);
        assertEquals("It", chained.pointLabel);
        assertEquals("dog", chained.targetLabel);
    }

    @Test
    void linksToTheSameReferentMakeTheirAnaphorsOneChain() {
        // It -> a man, the dog bound to a man: A -> C and B -> C, so "It -> the dog" holds as well.
        List<PragmaticChoiceDto> choices = chain1(links("s6:x5", "s0:x1", "s4:y1", "s0:x1"),
                bound("s4", "s4:y1", "s0:x1"));
        assertEquals(Set.of("ANAPHORA@33-35=2-5", "ANAPHORA@33-35=17-24", "PRESUPPOSITION@17-24=2-5"), ids(choices));
    }

    @Test
    void accommodationAddsNoLink() {
        List<PragmaticChoiceDto> choices = chain1(links("s6:x5", "s0:x1"), accommodated("s4", "s0", Collapse.Depth.GLOBAL));
        assertEquals(Set.of("ANAPHORA@33-35=2-5", "PRESUPPOSITION@17-24=GLOBAL"), ids(choices));
        assertEquals("The dog", choices.get(1).pointLabel);
        assertEquals("ACCOMMODATED", choices.get(1).resolution);
    }

    @Test
    void anUnboundPronounHoldsOnlySlash() {
        List<PragmaticChoiceDto> choices = chain1(links("s6:x5", "/"), accommodated("s4", "s0", Collapse.Depth.GLOBAL));
        assertEquals(Set.of("ANAPHORA@33-35=/", "PRESUPPOSITION@17-24=GLOBAL"), ids(choices));
    }

    @Test
    void anUnboundPronounALaterOneLinksToHoldsNothingLater() {
        // "A man smiled. He saw a dog. He barked.": the first he unbound, the second -> the first.
        Map<String, PragmaticChoices.Referent> referents = new LinkedHashMap<>();
        referents.put("s0:x1", referent("man", 2, 5));
        referents.put("s2:x3", referent("He", 14, 16));
        referents.put("s5:x6", referent("He", 28, 30));
        List<PragmaticChoiceDto> choices = new PragmaticChoices(referents, List.of(), global("s0", "s2", "s5"))
                .of(links("s2:x3", "/", "s5:x6", "s2:x3"), List.of());
        assertEquals(Set.of("ANAPHORA@14-16=/", "ANAPHORA@28-30=14-16"), ids(choices));
    }

    @Test
    void aPronounHoldsItsOwnAntecedentEvenAfterIt() {
        // Cataphora: the direct link is always held (D4), later chain members are not.
        Map<String, PragmaticChoices.Referent> referents = new LinkedHashMap<>();
        referents.put("s0:x1", referent("he", 6, 8));
        referents.put("s1:x2", referent("John", 10, 14));
        referents.put("s2:x3", referent("him", 20, 23));
        List<PragmaticChoiceDto> choices = new PragmaticChoices(referents, List.of(), global("s0", "s1", "s2"))
                .of(links("s0:x1", "s1:x2", "s2:x3", "s1:x2"), List.of());
        assertEquals(Set.of("ANAPHORA@6-8=10-14", "ANAPHORA@20-23=6-8", "ANAPHORA@20-23=10-14"), ids(choices));
    }

    @Test
    void anIntermediateSiteIsNamedByTheReferentsOfItsBox() {
        // "Every man saw the cat that smiled.": s1 the restrictor (man), s3 the cat's p-box.
        Map<String, PragmaticChoices.Referent> referents = new LinkedHashMap<>();
        referents.put("s1:x1", referent("man", 6, 9));
        referents.put("s3:y1", referent("the cat", 14, 21));
        Map<String, String> boxOf = new HashMap<>(Map.of("s0", "s0", "s1", "s1", "s2", "s2", "s3", "s3"));
        List<PragmaticChoiceDto> choices = new PragmaticChoices(referents,
                List.of(new PresuppositionBox("s3", "s2", List.of("s3:y1"))), boxOf)
                .of(links(), List.of(accommodated("s3", "s1", Collapse.Depth.INTERMEDIATE)));
        assertEquals(Set.of("PRESUPPOSITION@14-21=INTERMEDIATE:6-9"), ids(choices));
        assertEquals("box of man", choices.get(0).targetLabel);
    }

    @Test
    void aReferentWithoutAHeadSpanHasNoKey() {
        Map<String, PragmaticChoices.Referent> referents = new LinkedHashMap<>(CHAIN1);
        referents.put("s0:x1", new PragmaticChoices.Referent(List.of(), "man"));
        PragmaticChoices choices = new PragmaticChoices(referents, THE_DOG, global("s0", "s4", "s6"));
        assertThrows(IllegalStateException.class, () -> choices.of(links("s6:x5", "s0:x1"),
                List.of(accommodated("s4", "s0", Collapse.Depth.GLOBAL))));
    }
}
