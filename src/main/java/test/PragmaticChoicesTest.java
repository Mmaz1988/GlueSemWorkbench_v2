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

    /** States as the provenance describes them: name, entering edge, parent, SRC span, text. */
    private static Map<String, PragmaticChoices.State> states(Object... rows) {
        Map<String, PragmaticChoices.State> states = new HashMap<>();
        for (int i = 0; i < rows.length; i += 6) {
            states.put((String) rows[i], new PragmaticChoices.State((String) rows[i + 1], (String) rows[i + 2],
                    List.of(new int[]{(Integer) rows[i + 3], (Integer) rows[i + 4]}), (String) rows[i + 5]));
        }
        return states;
    }

    private static Map<String, PragmaticChoices.State> global(String... names) {
        Map<String, PragmaticChoices.State> states = new HashMap<>();
        for (String name : names) {
            states.put(name, new PragmaticChoices.State(name.equals("s0") ? null : "MERGE", "s0",
                    List.of(new int[]{0, 1}), "A"));
        }
        return states;
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

    /** "Every cat with a hat liked the hat.", a hat inside the restrictor: s0 global, s1+s2 the
     *  restrictor (SUB, "Every"), s3+s5 the consequent (IMP from the restrictor's last state
     *  s2; its first state's SRC is "the"), s4 the hat's p-box. */
    private static final Map<String, PragmaticChoices.State> EVERY_CAT = states(
            "s0", null, null, 15, 16, "a",
            "s1", "SUB", "s0", 0, 5, "Every",
            "s2", "MERGE", "s1", 15, 16, "a",
            "s3", "IMP", "s2", 27, 30, "the",
            "s4", "PRSP", "s3", 27, 30, "the",
            "s5", "MERGE", "s3", 21, 26, "liked");

    private static List<PragmaticChoiceDto> everyCat(Collapse.Resolution resolution) {
        Map<String, PragmaticChoices.Referent> referents = new LinkedHashMap<>();
        referents.put("s1:x1", referent("cat", 6, 9));
        referents.put("s2:x2", referent("hat", 17, 20));
        referents.put("s4:y1", referent("the hat", 27, 34));
        return new PragmaticChoices(referents, List.of(new PresuppositionBox("s4", "s3", List.of("s4:y1"))), EVERY_CAT)
                .of(links(), List.of(resolution));
    }

    @Test
    void anIntermediateSiteIsNamedByTheOperatorThatOpensItsBox() {
        List<PragmaticChoiceDto> choices = everyCat(accommodated("s4", "s1", Collapse.Depth.INTERMEDIATE));
        assertEquals(Set.of("PRESUPPOSITION@27-34=INTERMEDIATE:restrictor@0-5"), ids(choices));
        assertEquals("restrictor of every", choices.get(0).targetLabel);
    }

    @Test
    void aConsequentIsNamedByItsRestrictorsOperator() {
        // The consequent's own SRC is "the"; it hangs off the restrictor's last state s2.
        List<PragmaticChoiceDto> choices = everyCat(accommodated("s4", "s3", Collapse.Depth.INTERMEDIATE));
        assertEquals(Set.of("PRESUPPOSITION@27-34=INTERMEDIATE:scope@0-5"), ids(choices));
        assertEquals("scope of every", choices.get(0).targetLabel);
    }

    @Test
    void aBoxWithoutReferentsIsASiteAndAPBoxWithoutBindReferentsIsNamedByItsTrigger() {
        // "No cat saw that it rained."-like: s1 the negated box (NOT, "No") introduces nothing;
        // the p-box s2 has no bind referent, so its trigger names it.
        Map<String, PragmaticChoices.State> states = states(
                "s0", null, null, 0, 2, "No",
                "s1", "NOT", "s0", 0, 2, "No",
                "s2", "PRSP", "s1", 11, 15, "that");
        List<PragmaticChoiceDto> choices = new PragmaticChoices(new LinkedHashMap<>(),
                List.of(new PresuppositionBox("s2", "s1", List.of())), states)
                .of(links(), List.of(accommodated("s2", "s1", Collapse.Depth.INTERMEDIATE)));
        assertEquals(Set.of("PRESUPPOSITION@11-15=INTERMEDIATE:negation@0-2"), ids(choices));
        assertEquals("that", choices.get(0).pointLabel);
        assertEquals("under no", choices.get(0).targetLabel);
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
