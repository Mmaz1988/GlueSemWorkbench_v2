package glueSemantics.parser;

import Discriminants.McDiscriminant;
import glueSemantics.semantics.MeaningConstructor;
import org.jgrapht.Graph;
import org.jgrapht.graph.DefaultEdge;

import java.util.*;

public class LexicalEntries {
    public LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries;
    public Graph<String, DefaultEdge> multiStageGraph;

    public LexicalEntries(LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries) {
        this.lexicalEntries = lexicalEntries;
    }

    public LexicalEntries(LinkedHashMap<Integer, List<MeaningConstructor>> lexicalEntries, Graph<String, DefaultEdge> multiStageGraph) {
        this.lexicalEntries = lexicalEntries;
        this.multiStageGraph = multiStageGraph;
    }


    public List<McDiscriminant> calculateDiscriminants() {
        HashMap<String, McDiscriminant> mcDiscriminantValues = new HashMap<>();

        int discriminantCounter = 0;

        for (Integer key1 : lexicalEntries.keySet()) {
            for (MeaningConstructor mc1 : lexicalEntries.get(key1)) {
                if (mcDiscriminantValues.containsKey(mc1.toString())) {
                    mcDiscriminantValues.get(mc1.toString()).mcSetIds.add(key1);
                    continue;
                }
                HashSet<Integer> mcSetIds = new HashSet<>();
                mcSetIds.add(key1);
                mcDiscriminantValues.put(mc1.toString(), new McDiscriminant("mc" + discriminantCounter, mc1.toString(), mcSetIds));
                discriminantCounter++;
            }
        }

        //Iterate through mcDiscrimantValues and remove all entries whose counter equals the number of keys in lexicalEntries
        mcDiscriminantValues.entrySet().removeIf((entry) -> (entry.getValue().mcSetIds.size() == lexicalEntries.size()));


        List<McDiscriminant> initialDiscriminantValues = mcDiscriminantValues.values().stream()
                .sorted(Comparator.comparingDouble((McDiscriminant v) ->
                        entropy(v.mcSetIds.size(), lexicalEntries.size())))
                .toList();

// LinkedHashMap preserves insertion order.
// If you reversed the list first, insertion order will follow that reversed order.
        Map<Set<Integer>, McDiscriminant> unique = new LinkedHashMap<>();

        for (McDiscriminant d : initialDiscriminantValues) {

            // IMPORTANT:
            // mcSetIds is mutable (HashSet). If it changes later, it would break map keying.
            // So we make an immutable copy to use as the key.
            Set<Integer> key = Set.copyOf(d.mcSetIds);

            // Keep the first discriminant we see for this key:
            unique.putIfAbsent(key, d);
        }

// Now the unique representatives (in preserved order):
        initialDiscriminantValues = new ArrayList<>(unique.values());
        Collections.reverse(initialDiscriminantValues);

        return initialDiscriminantValues;
    }

    private static double entropy(int k, int n) {
        if (n <= 0) return 0.0;
        if (k <= 0 || k >= n) return 0.0;
        double p = (double) k / (double) n;
        return -(p * log2(p) + (1.0 - p) * log2(1.0 - p));
    }

    private static double log2(double x) {
        return Math.log(x) / Math.log(2.0);
    }
}

