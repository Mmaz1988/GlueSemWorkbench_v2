package webservice.rest;

import de.ukon.lfgxdrt.drs_elements.Collapse;
import de.ukon.lfgxdrt.drs_elements.PresuppositionBox;
import webservice.rest.dtos.PragmaticChoiceDto;

import java.util.*;

/**
 * The alternatives a PCDRS reading holds (D1-D4 in xleplusglue's
 * docs/plans/PRAGMATIC_DISCRIMINANTS_PLAN.md), keyed by the character spans of referents' heads
 * as LiGER's /drs_provenance gives them.
 *
 * A reading's coreference chains are the equivalence closure of its identity links: every
 * a(x,y) with y not "/", pronouns and bind referents alike; accommodation adds none. A
 * pronoun -- or a bound p-box's bind referent -- holds A -> X for every X of its chain whose
 * head starts before its own, and for its own antecedent. An accommodation site is its depth
 * across readings; an intermediate one also names its box by its first state, an identity among
 * the readings of one semantic reading only (D2a).
 */
public final class PragmaticChoices {

    /** A referent as the provenance describes it: its head's spans, lexical text, and the
     *  constants it equals (a proper name's). */
    public record Referent(List<int[]> spans, String text, List<String> names) {

        public Referent(List<int[]> spans, String text) {
            this(spans, text, List.of());
        }

        String key() {
            StringJoiner key = new StringJoiner(",");
            spans.forEach(span -> key.add(span[0] + "-" + span[1]));
            return key.toString();
        }

        int start() {
            return spans.get(0)[0];
        }
    }

    /** A state as the provenance describes it: the edge entering it, the node that edge comes
     *  from, and the spans and text of the meaning constructor it comes from (its SRC). */
    public record State(String enteredBy, String parent, List<int[]> spans, String text) {
    }

    private final Map<String, Referent> referents;
    private final List<PresuppositionBox> boxes;
    private final Map<String, State> states;

    /**
     * @param referents every referent by its qualified name (s4:y1), as the PCDRS prints it
     * @param boxes     the p-boxes of the DRS
     * @param states    every state by its name (s3), as P: writes it
     */
    public PragmaticChoices(Map<String, Referent> referents, List<PresuppositionBox> boxes,
                            Map<String, State> states) {
        this.referents = referents;
        this.boxes = boxes;
        this.states = states;
    }

    /**
     * @param anaphora    the reading's links: pronoun or bind referent -> antecedent or "/"
     * @param resolutions the collapse's resolution of every p-box
     * @throws IllegalStateException a referent without a head span: it has no key (D2)
     */
    public List<PragmaticChoiceDto> of(Map<String, String> anaphora, List<Collapse.Resolution> resolutions) {
        Map<String, String> parent = new HashMap<>();
        anaphora.forEach((anaphor, antecedent) -> {
            if (!"/".equals(antecedent)) {
                parent.put(find(parent, anaphor), find(parent, antecedent));
            }
        });
        Map<String, List<String>> chains = new HashMap<>();
        for (String name : new ArrayList<>(parent.keySet())) {
            chains.computeIfAbsent(find(parent, name), root -> new ArrayList<>()).add(name);
        }

        Set<String> bindReferents = new HashSet<>();
        boxes.forEach(box -> bindReferents.addAll(box.bindReferents()));
        List<PragmaticChoiceDto> choices = new ArrayList<>();
        anaphora.forEach((pronoun, antecedent) -> {
            if (bindReferents.contains(pronoun)) {
                return;
            }
            Referent anaphor = referent(pronoun);
            if ("/".equals(antecedent)) {
                choices.add(new PragmaticChoiceDto("ANAPHORA", anaphor.key(), anaphor.text(), "UNBOUND",
                        "/", "/", true));
                return;
            }
            for (String member : earlier(pronoun, antecedent, chains.get(find(parent, pronoun)))) {
                Referent target = referent(member);
                choices.add(new PragmaticChoiceDto("ANAPHORA", anaphor.key(), anaphor.text(), "ANTECEDENT",
                        target.key(), target.text(), member.equals(antecedent)));
            }
        });

        Map<String, Collapse.Resolution> resolutionOf = new HashMap<>();
        resolutions.forEach(resolution -> resolutionOf.put(resolution.presupposition(), resolution));
        for (PresuppositionBox box : boxes) {
            Collapse.Resolution resolution = resolutionOf.get(box.label());
            if (resolution == null) {
                throw new IllegalStateException(box.label() + ": the collapse reports no resolution for it");
            }
            // A p-box by its bind referents' heads; without any, by its trigger (its state's SRC).
            String point;
            String pointLabel;
            String name = null;
            if (box.bindReferents().isEmpty()) {
                State trigger = state(box.label());
                point = spansKey(trigger.spans());
                pointLabel = trigger.text();
            } else {
                List<Referent> own = box.bindReferents().stream().map(this::referent).toList();
                point = keys(own);
                pointLabel = String.join(" ", own.stream().map(Referent::text).toList());
                // A proper name's p-box: one bind referent, equal to one constant
                if (own.size() == 1 && own.get(0).names().size() == 1) {
                    name = own.get(0).names().get(0);
                }
            }
            int first = choices.size();
            if (!resolution.bound()) {
                choices.add(switch (resolution.depth()) {
                    case GLOBAL -> new PragmaticChoiceDto("PRESUPPOSITION", point, pointLabel, "ACCOMMODATED",
                            "GLOBAL", "global", true);
                    case LOCAL -> new PragmaticChoiceDto("PRESUPPOSITION", point, pointLabel, "ACCOMMODATED",
                            "LOCAL", "in situ", true);
                    case INTERMEDIATE -> {
                        String start = boxStart(resolution.site());
                        yield PragmaticChoiceDto.intermediate(point, pointLabel, start, role(start));
                    }
                });
            } else {
                boolean several = box.bindReferents().size() > 1;
                for (String bound : box.bindReferents()) {
                    String antecedent = anaphora.get(bound);
                    Referent from = referent(bound);
                    for (String member : earlier(bound, antecedent, chains.get(find(parent, bound)))) {
                        Referent target = referent(member);
                        choices.add(new PragmaticChoiceDto("PRESUPPOSITION", point, pointLabel, "BOUND",
                                several ? from.key() + ">" + target.key() : target.key(),
                                several ? from.text() + " ↦ " + target.text() : target.text(),
                                member.equals(antecedent)));
                    }
                }
            }
            for (PragmaticChoiceDto choice : choices.subList(first, choices.size())) {
                choice.name = name;
            }
        }
        return choices;
    }

    /** The members of an anaphor's chain it can name (D4): before it, or its own antecedent. */
    private List<String> earlier(String anaphor, String antecedent, List<String> chain) {
        int start = referent(anaphor).start();
        return chain.stream()
                .filter(member -> !member.equals(anaphor)
                        && (member.equals(antecedent) || referent(member).start() < start))
                .sorted(Comparator.comparingInt((String member) -> referent(member).start()))
                .toList();
    }

    /** A box's role by the edge that enters its first state, for display only (D2a). */
    private String role(String first) {
        String entered = state(first).enteredBy();
        return entered == null ? "box" : switch (entered) {
            case "SUB" -> "antecedent";
            case "IMP" -> "consequent";
            case "NOT" -> "negation";
            case "OR" -> "disjunct";
            case "SCOPE" -> "embedding";
            default -> entered.toLowerCase();
        };
    }

    /** The first state of the box a state belongs to: MERGE and UPDATE edges followed back. */
    private String boxStart(String name) {
        String current = name;
        while (true) {
            State state = state(current);
            if (!"MERGE".equals(state.enteredBy()) && !"UPDATE".equals(state.enteredBy())) {
                return current;
            }
            current = state.parent();
        }
    }

    private State state(String name) {
        State state = states.get(name);
        if (state == null) {
            throw new IllegalStateException(name + ": the provenance describes no such state");
        }
        return state;
    }

    private static String spansKey(List<int[]> spans) {
        StringJoiner key = new StringJoiner(",");
        spans.forEach(span -> key.add(span[0] + "-" + span[1]));
        return key.toString();
    }

    private static String keys(List<Referent> referents) {
        StringJoiner key = new StringJoiner(",");
        referents.forEach(referent -> key.add(referent.key()));
        return key.toString();
    }

    private Referent referent(String name) {
        Referent referent = referents.get(name);
        if (referent == null || referent.spans().isEmpty()) {
            throw new IllegalStateException(name + ": the provenance gives it no head span");
        }
        return referent;
    }

    private static String find(Map<String, String> parent, String name) {
        parent.putIfAbsent(name, name);
        String root = name;
        while (!parent.get(root).equals(root)) {
            root = parent.get(root);
        }
        parent.put(name, root);
        return root;
    }
}
