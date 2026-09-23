package test;

import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.KosarajuStrongConnectivityInspector;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.junit.jupiter.api.Test;
import prover.GraphAnalysis;
import prover.categoryGraph.CGNode;

import java.util.List;
import java.util.Map;
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
}
