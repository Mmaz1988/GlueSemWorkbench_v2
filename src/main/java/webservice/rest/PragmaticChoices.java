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
 * head starts before its own, and for its own antecedent.
 */
public final class PragmaticChoices {

    /** A referent as the provenance describes it: its head's spans and lexical text. */
    public record Referent(List<int[]> spans, String text) {

        String key() {
            StringJoiner key = new StringJoiner(",");
            spans.forEach(span -> key.add(span[0] + "-" + span[1]));
            return key.toString();
        }

        int start() {
            return spans.get(0)[0];
        }
    }

    private final Map<String, Referent> referents;
    private final List<PresuppositionBox> boxes;
    private final Map<String, String> boxOf;

    /**
     * @param referents every referent by its qualified name (s4:y1), as the PCDRS prints it
     * @param boxes     the p-boxes of the DRS
     * @param boxOf     every state label -> the first state of its box ({@code DRS.boxes()})
     */
    public PragmaticChoices(Map<String, Referent> referents, List<PresuppositionBox> boxes,
                            Map<String, String> boxOf) {
        this.referents = referents;
        this.boxes = boxes;
        this.boxOf = boxOf;
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
            List<Referent> own = box.bindReferents().isEmpty()
                    ? referentsOfBox(box.label()) : box.bindReferents().stream().map(this::referent).toList();
            String point = keys(own);
            String pointLabel = String.join(" ", own.stream().map(Referent::text).toList());
            if (!resolution.bound()) {
                String[] site = site(resolution);
                choices.add(new PragmaticChoiceDto("PRESUPPOSITION", point, pointLabel, "ACCOMMODATED",
                        site[0], site[1], true));
                continue;
            }
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

    /** A site as a key and a label: global, in situ, or the box it opens, by its referents. */
    private String[] site(Collapse.Resolution resolution) {
        return switch (resolution.depth()) {
            case GLOBAL -> new String[]{"GLOBAL", "global"};
            case LOCAL -> new String[]{"LOCAL", "in situ"};
            case INTERMEDIATE -> {
                List<Referent> own = referentsOfBox(resolution.site());
                yield new String[]{"INTERMEDIATE:" + keys(own),
                        "box of " + String.join(" ", own.stream().map(Referent::text).toList())};
            }
        };
    }

    /** The referents introduced in the box a state belongs to, in text order. */
    private List<Referent> referentsOfBox(String state) {
        String box = boxOf.getOrDefault(state, state);
        List<Referent> own = new ArrayList<>();
        referents.forEach((name, referent) -> {
            int colon = name.indexOf(':');
            if (colon > 0 && box.equals(boxOf.getOrDefault(name.substring(0, colon), name.substring(0, colon)))) {
                own.add(referent);
            }
        });
        if (own.isEmpty()) {
            throw new IllegalStateException("box " + box + " introduces no referent to name it by");
        }
        own.sort(Comparator.comparingInt(Referent::start));
        return own;
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
