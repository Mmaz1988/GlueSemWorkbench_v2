package test;

import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import glueSemantics.linearLogic.Category;
import prover.GraphAnalysis;
import prover.categoryGraph.CGNode;

import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;

import static org.junit.jupiter.api.Assertions.assertEquals;

/** The explanation graph labels a strongly connected component by what it produces. */
public class GraphAnalysisTest {

    private static Map<String, CGNode> nodes(Graph<CGNode, DefaultEdge> graph, String... names) {
        Map<String, CGNode> byName = List.of(names).stream()
                .collect(Collectors.toMap(Function.identity(), n -> new CGNode(n, CGNode.type.CATEGORY, null)));
        byName.values().forEach(graph::addVertex);
        return byName;
    }

    private static Graph<CGNode, DefaultEdge> componentOf(Graph<CGNode, DefaultEdge> graph, CGNode member) {
        return new KosarajuStrongConnectivityInspector<>(graph).getStronglyConnectedComponents().stream()
                .filter(c -> c.containsVertex(member)).findFirst().orElseThrow();
    }

    @Test
    void aComponentsOutputsAreTheMembersWithAnEdgeLeavingIt() {
        Graph<CGNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        Map<String, CGNode> n = nodes(graph, "a", "b", "c", "goal");
        graph.addEdge(n.get("a"), n.get("b"));
        graph.addEdge(n.get("b"), n.get("a"));
        graph.addEdge(n.get("b"), n.get("goal"));
        graph.addEdge(n.get("c"), n.get("a"));

        GraphAnalysis analysis = new GraphAnalysis("goal", null, graph);

        assertEquals(List.of("b"), analysis.sccOutputs(componentOf(graph, n.get("a"))));
    }

    @Test
    void aComponentThatEndsTheDerivationIsLabelledByItsGoal() {
        Graph<CGNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        Map<String, CGNode> n = nodes(graph, "x", "goal", "p");
        graph.addEdge(n.get("x"), n.get("goal"));
        graph.addEdge(n.get("goal"), n.get("x"));
        graph.addEdge(n.get("p"), n.get("x"));

        GraphAnalysis analysis = new GraphAnalysis("goal", null, graph);

        assertEquals(List.of("goal"), analysis.sccOutputs(componentOf(graph, n.get("x"))));
    }

    @Test
    void withoutTheFullGraphThereAreNoOutputs() {
        Graph<CGNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        Map<String, CGNode> n = nodes(graph, "a", "b");
        graph.addEdge(n.get("a"), n.get("b"));
        graph.addEdge(n.get("b"), n.get("a"));

        assertEquals(List.of(), new GraphAnalysis("goal", null).sccOutputs(componentOf(graph, n.get("a"))));
    }

    @Test
    void aCategoryStringIsPrettyPrintedLikeAParsedCategory() {
        assertEquals("gₑ", Category.toUTF8("g_e"));
        assertEquals("(gₑ ⊸ fₜ) ⊸ hₜ", Category.toUTF8("(g_e -o f_t) -o h_t"));
        assertEquals("gₑ ⊸ fₜₑₘₚ", Category.toUTF8("g_e -o f_temp"));
    }

    @Test
    void anUnknownTypeStaysAsWrittenInsteadOfPrintingNull() {
        assertEquals("g_q ⊸ fₜ", Category.toUTF8("g_q -o f_t"));
        assertEquals("g_q", new Category("g_q", Set.of()).toUTF8());
        // As before: only an atom with exactly one "_" gets a subscript.
        assertEquals("x_1_e", Category.toUTF8("x_1_e"));
    }

    /**
     * Why category nodes are printed from the parsed category, not from their string: toString()
     * drops brackets, so both groupings print alike. Latent only: categories come from compiled
     * premises, whose antecedents are atomic, so the string is unambiguous in practice; it would
     * matter if an X -o X modifier stayed uncompiled (Lev's optimization-1, commented out).
     * The string form keeps what it is given.
     */
    @Test
    void theParsedFormKeepsBracketsThatToStringDrops() {
        Category leftNested = new Category(new Category(new Category("g_e", Set.of()), new Category("f_t", Set.of()), Set.of()),
                new Category("h_t", Set.of()), Set.of());

        assertEquals("(gₑ ⊸ fₜ) ⊸ hₜ", leftNested.toUTF8());
        assertEquals("g_e -o f_t -o h_t", leftNested.toString());
        assertEquals("gₑ ⊸ fₜ ⊸ hₜ", Category.toUTF8(leftNested.toString()));
    }

    @Test
    void aComponentsOutputsArePrettyPrinted() {
        Graph<CGNode, DefaultEdge> graph = new DefaultDirectedGraph<>(DefaultEdge.class);
        Map<String, CGNode> n = nodes(graph, "g_e -o f_t", "f_t", "h_t");
        graph.addEdge(n.get("g_e -o f_t"), n.get("f_t"));
        graph.addEdge(n.get("f_t"), n.get("g_e -o f_t"));
        graph.addEdge(n.get("g_e -o f_t"), n.get("h_t"));

        GraphAnalysis analysis = new GraphAnalysis("h_t", null, graph);

        assertEquals(List.of("gₑ ⊸ fₜ"), analysis.sccOutputs(componentOf(graph, n.get("f_t"))));
    }
}
