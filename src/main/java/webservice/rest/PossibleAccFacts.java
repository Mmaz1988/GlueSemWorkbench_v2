package webservice.rest;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Deque;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.logging.Logger;

/**
 * The accommodation sites the post-processing rules left on a structure: {@code #p POSSIBLE-ACC #s}
 * from a presupposition state to a state of its previous context, the presupposition counterpart
 * of POSSIBLE-ANT.
 *
 * <p>Each end is named by a referent, as anaphora mappings are: the presupposition by the referent
 * its state introduces, the site by the first referent (in fact order) of the site's MERGE chain.
 * The graph is compiled from the DRS before its merges are resolved, so one box is a chain of
 * states there, and every state of a chain names the same box -- and the same site. A chain that
 * introduces no referent but holds the root is the outermost box, {@code s0}.
 */
public final class PossibleAccFacts {

    static final String RELATION = "POSSIBLE-ACC";
    private static final Logger LOGGER = Logger.getLogger(PossibleAccFacts.class.getName());

    private record Fact(String source, String label, String target) {
    }

    private final List<Fact> facts = new ArrayList<>();

    private PossibleAccFacts(Object structure) {
        collect(structure);
    }

    /** Presupposed referent to the referents (or s0) naming its sites, in the order found. */
    public static LinkedHashMap<String, LinkedHashSet<String>> extract(Map<String, Object> structure) {
        return new PossibleAccFacts(structure).sites();
    }

    private LinkedHashMap<String, LinkedHashSet<String>> sites() {
        Map<String, String> names = new LinkedHashMap<>();
        Set<String> roots = new LinkedHashSet<>();
        for (Fact fact : facts) {
            if ("NAME".equals(fact.label())) {
                names.put(fact.source(), fact.target());
            } else if ("NODE_TYPE".equals(fact.label()) && "root".equals(fact.target())) {
                roots.add(fact.source());
            }
        }

        LinkedHashMap<String, LinkedHashSet<String>> sites = new LinkedHashMap<>();
        for (Fact fact : facts) {
            if (!RELATION.equals(fact.label())) {
                continue;
            }
            String presupposed = firstReferent(Set.of(fact.source()), names);
            if (presupposed == null) {
                LOGGER.warning("Ignoring " + RELATION + " from a state that introduces no named referent: " + fact);
                continue;
            }
            Set<String> chain = mergeChain(fact.target());
            String site = firstReferent(chain, names);
            if (site == null && chain.stream().anyMatch(roots::contains)) {
                site = "s0";
            }
            if (site == null) {
                LOGGER.warning("Ignoring " + RELATION + " to a box that introduces no named referent: " + fact);
                continue;
            }
            sites.computeIfAbsent(presupposed, ignored -> new LinkedHashSet<>()).add(site);
        }
        return sites;
    }

    private Set<String> mergeChain(String state) {
        Set<String> chain = new LinkedHashSet<>();
        Deque<String> open = new ArrayDeque<>();
        open.add(state);
        while (!open.isEmpty()) {
            String current = open.poll();
            if (!chain.add(current)) {
                continue;
            }
            for (Fact fact : facts) {
                if (!"MERGE".equals(fact.label())) {
                    continue;
                }
                if (fact.source().equals(current)) {
                    open.add(fact.target());
                } else if (fact.target().equals(current)) {
                    open.add(fact.source());
                }
            }
        }
        return chain;
    }

    private String firstReferent(Set<String> states, Map<String, String> names) {
        for (Fact fact : facts) {
            if ("IN".equals(fact.label()) && states.contains(fact.source())) {
                String name = names.get(fact.target());
                if (name != null && !name.isBlank()) {
                    return name;
                }
            }
        }
        return null;
    }

    private void collect(Object value) {
        if (value instanceof Map<?, ?> map) {
            Object label = map.get("relationLabel");
            Object source = map.containsKey("fsNode") ? map.get("fsNode") : map.get("sourceNode");
            Object target = map.containsKey("fsValue") ? map.get("fsValue") : map.get("targetNode");
            if (label != null && source != null && target != null) {
                facts.add(new Fact(String.valueOf(source).trim(), String.valueOf(label).trim(),
                        String.valueOf(target).trim()));
            }
            for (Object child : map.values()) {
                collect(child);
            }
        } else if (value instanceof Iterable<?> iterable) {
            for (Object child : iterable) {
                collect(child);
            }
        }
    }
}
