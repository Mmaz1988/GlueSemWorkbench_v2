package prover;


import com.mxgraph.layout.hierarchical.mxHierarchicalLayout;
import com.mxgraph.layout.mxGraphLayout;
import com.mxgraph.model.mxCell;
import com.mxgraph.swing.mxGraphComponent;
import com.mxgraph.util.mxConstants;
import glueSemantics.linearLogic.Premise;
import org.jgrapht.Graph;
import org.jgrapht.ext.JGraphXAdapter;
import org.jgrapht.graph.DefaultEdge;
import prover.categoryGraph.CGNode;
import prover.categoryGraph.History;
import webservice.rest.dtos.GswbEdge;
import webservice.rest.dtos.GswbGraph;
import webservice.rest.dtos.GswbGraphComponent;
import webservice.rest.dtos.GswbNode;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.List;
import java.util.*;
import java.util.stream.Collectors;

public class GraphAnalysis {

    private Graph<Graph<CGNode,DefaultEdge>,DefaultEdge> stronglyConnectedGraph;

    private Graph<CGNode,DefaultEdge> currentGraph;

    private HashMap<Premise,Set<String>> compiledToPremiseMapping;
    private String goalCategory;
    private mxGraphComponent graphComponent;
    private HashMap<mxCell,mxGraphComponent> sccMap = new HashMap<>();

    public GraphAnalysis(String goalCategory, Graph<Graph<CGNode,DefaultEdge>,DefaultEdge> stronglyConnectedGraph)
    {
        this.goalCategory = goalCategory;
        this.stronglyConnectedGraph = stronglyConnectedGraph;
    }

    public GraphAnalysis(String goalCategory, Graph<Graph<CGNode,DefaultEdge>,DefaultEdge> stronglyConnectedGraph, Graph<CGNode,DefaultEdge> currentGraph)
    {
        this.goalCategory = goalCategory;
        this.stronglyConnectedGraph = stronglyConnectedGraph;
        this.currentGraph = currentGraph;
    }

    public GraphAnalysis(String goalCategory, Graph<Graph<CGNode,DefaultEdge>,DefaultEdge> stronglyConnectedGraph,
                         Graph<CGNode,DefaultEdge> currentGraph, HashMap<Premise, Set<String>> compiledToPremiseMapping)
    {
        this.goalCategory = goalCategory;
        this.stronglyConnectedGraph = stronglyConnectedGraph;
        this.currentGraph = currentGraph;
        this.compiledToPremiseMapping = compiledToPremiseMapping;
    }

    public GswbGraph returnJSONGraph()
    {
        List<GswbGraphComponent> graphComponents = new ArrayList<>();


        HashMap<String,Set<GswbNode>> categoryToPremiseMapping = new HashMap<>();

        for (Premise p : compiledToPremiseMapping.keySet())
        {
            GswbNode premiseNode = new GswbNode();
            premiseNode.data = new HashMap<>();
            premiseNode.data.put("id",p.getGlueTerm().category().toString());
            premiseNode.data.put("color", "orange");
            premiseNode.data.put("solutions",Collections.singleton(p.toString()));

          // graphComponents.add(premiseNode);

            for (String c : compiledToPremiseMapping.get(p))
            {
                if (!categoryToPremiseMapping.containsKey(c))
                {
                    categoryToPremiseMapping.put(c,new HashSet<>());
                }
                categoryToPremiseMapping.get(c).add(premiseNode);
            }

        }




        //Create Gswbgraph nodes
        for (Graph<CGNode,DefaultEdge> g : this.stronglyConnectedGraph.vertexSet())
        {
            GswbNode gswbNode = new GswbNode();
            gswbNode.data = new HashMap<>();
            gswbNode.data.put("id",g.toString());
            if (g.edgeSet().isEmpty())
            {
                CGNode currentNode = g.vertexSet().stream().findAny().get();

                if (currentNode.toString().equals(goalCategory))
                {
                    gswbNode.data.put("color", "yellow");

                } else  if (currentNode.histories.isEmpty())
                {
                    gswbNode.data.put("color", "red");
                } else
                {
                    gswbNode.data.put("color", "blue");
                }

                Set<String> solutions = new HashSet<>();

                if (currentNode.histories != null && !currentNode.histories.isEmpty())
                {
                    for (History h : currentNode.histories)
                    {
                        try {
                            solutions.addAll(h.calculateSolutions().stream().map(Premise::toString).collect(Collectors.toList()));
                        } catch(Exception e)
                        {
                            System.out.println("No solutions to calculate.");
                        }}
                }
                if (!solutions.isEmpty()) {
                    gswbNode.data.put("solutions", solutions);
                }else
                {
                    if (currentNode.histories != null && !currentNode.histories.isEmpty()) {
                        History h = currentNode.histories.stream().findAny().get();
                        gswbNode.data.put("solutions", Collections.singleton(h.p.toString()));
                    }
                }

                if (categoryToPremiseMapping.containsKey(currentNode.category.toString()))
                {

                    List<GswbEdge> incomingEdges = new ArrayList<>();

                    for (GswbNode gn : categoryToPremiseMapping.get(currentNode.category.toString()))
                    {
                        GswbEdge parent = new GswbEdge((String) gn.data.get("id"),g.toString());
                        parent.data.put("edge_type","parent");
                        graphComponents.add(gn);
                        graphComponents.add(parent);
                    }



                }




                /*
                //create a set of all values in compiledToPremiseMapping and map them to Premise.getGlueTerm().category.toString()
                Set<Premise> values = new HashSet<>(compiledToPremiseMapping.values());
              //  Set<String> valuesAsString = values.stream().map(x -> x.getGlueTerm().category().toString()).collect(Collectors.toSet());

                for (Premise value : values)
                {
                    GswbNode premiseNode = new GswbNode();
                    premiseNode.data = new HashMap<>();
                    premiseNode.data.put("id",value.getGlueTerm().category().toString());
                    premiseNode.data.put("color", "blue");

                    Set<String> originalPremises = Collections.singleton(value.toString());
                    premiseNode.data.put("solutions", originalPremises);

                    graphComponents.add(premiseNode);

                    if (compiledToPremiseMapping.containsKey(currentNode.category.toString()))
                    {
                        GswbEdge ge = new GswbEdge(value.getGlueTerm().category().toString(),
                                currentNode.category.toString());
                        ge.data.put("edge_type","default");
                        graphComponents.add(ge);
                    }

                }

                 */


            } else
            {
                gswbNode.data.put("color", "green");
                //g is a strongly connected component: Create subraph
                Set<CGNode> nodes = new HashSet<>(g.vertexSet());
                List<GswbGraphComponent> subGraphList = new ArrayList<>();

                //subgraph nodes
                for (CGNode node : nodes) {
                    GswbNode subGraphNode = new GswbNode();
                    subGraphNode.data = new HashMap<>();

                    subGraphNode.data.put("id", node.toString());

                    if (node.toString().equals(goalCategory)) {
                        subGraphNode.data.put("color", "yellow");
                    } else {
                        subGraphNode.data.put("color", "green");
                    }

                    List<String> solutions = new ArrayList<>();
                    if (node.histories != null && !node.histories.isEmpty()) {
                        for (History h : node.histories) {
                            try {
                                solutions.addAll(h.calculateSolutions().stream().map(Premise::toString).collect(Collectors.toList()));
                            } catch (Exception e) {
                                System.out.println("No solutions to calculate.");
                            }
                        }
                    }
                    if (!solutions.isEmpty()) {
                        subGraphNode.data.put("solutions", solutions);
                    } else
                    {if (node.histories != null && !node.histories.isEmpty()) {
                        History h = node.histories.stream().findAny().get();
                        subGraphNode.data.put("solutions", Collections.singleton(h.p.toString()));
                    }
                    }


                    if (categoryToPremiseMapping.containsKey(node.toString())) {


                        for (GswbNode gn : categoryToPremiseMapping.get(node.toString())) {
                            GswbEdge parent = new GswbEdge((String) gn.data.get("id"), node.toString());
                            parent.data.put("edge_type", "parent");
                            subGraphList.add(gn);
                            subGraphList.add(parent);
                        }
                    }
                        subGraphList.add(subGraphNode);
                    }
                //Create subgraph Edges
                for (DefaultEdge e : g.edgeSet())
                {
                    GswbEdge ge = new GswbEdge(g.getEdgeSource(e).toString(),
                            g.getEdgeTarget(e).toString());
                    ge.data.put("edge_type","default");
                    subGraphList.add(ge);
                }

                Set<DefaultEdge> incomingEdges = this.stronglyConnectedGraph.incomingEdgesOf(g);

                for (DefaultEdge e : incomingEdges)
                {
                 Set<CGNode> sourceSet = this.stronglyConnectedGraph.getEdgeSource(e).vertexSet();
                 Set<CGNode> targetSet = this.stronglyConnectedGraph.getEdgeTarget(e).vertexSet();

                 for (CGNode currentSource : sourceSet)
                 {

                    Set<DefaultEdge> outSet = this.currentGraph.outgoingEdgesOf(currentSource);
                    outSet = outSet.stream().filter(x -> targetSet.contains(this.currentGraph.getEdgeTarget(x))).collect(Collectors.toSet());

                    if (!outSet.isEmpty()) {
                        for (DefaultEdge e1 : outSet) {
                            GswbEdge ge = new GswbEdge(this.currentGraph.getEdgeSource(e1).toString(),
                                    this.currentGraph.getEdgeTarget(e1).toString());
                            ge.data.put("edge_type","external");
                            subGraphList.add(ge);
                        }
                        GswbNode subgraphNode = new GswbNode();
                        subgraphNode.data = new HashMap<>();
                        subgraphNode.data.put("id",currentSource.toString());
                        subgraphNode.data.put("color","blue");

                        List<String> solutions = new ArrayList<>();
                        if (currentSource.histories != null && !currentSource.histories.isEmpty())
                        {
                            for (History h : currentSource.histories)
                            {
                                try {
                                    solutions.addAll(h.calculateSolutions().stream().map(Premise::toString).collect(Collectors.toList()));
                                } catch(Exception exc)
                                {
                                    System.out.println("No solutions to calculate.");
                                }}
                        }

                        if (!solutions.isEmpty()) {
                            subgraphNode.data.put("solutions", solutions);
                        }

                        subGraphList.add(subgraphNode);
                    }
                    }
                }

                gswbNode.data.put("subgraph",new GswbGraph(subGraphList));
            }

            graphComponents.add(gswbNode);
        }

        //create gswbGraph edges
        for (DefaultEdge e : this.stronglyConnectedGraph.edgeSet())
        {
            GswbEdge ge = new GswbEdge(stronglyConnectedGraph.getEdgeSource(e).toString(),
                                        stronglyConnectedGraph.getEdgeTarget(e).toString());
            ge.data.put("edge_type","default");
            graphComponents.add(ge);
        }



        return new GswbGraph(graphComponents);
    }

    public void displayGraph()
    {
        JGraphXAdapter<Graph<CGNode,DefaultEdge>, DefaultEdge> jGraphXAdapter = new JGraphXAdapter<>(stronglyConnectedGraph);
        jGraphXAdapter.getStylesheet().getDefaultEdgeStyle().put(mxConstants.STYLE_NOLABEL, "1");
        jGraphXAdapter.getModel().beginUpdate();

        for (Object g : jGraphXAdapter.getVertexToCellMap().keySet()) {
            if (g instanceof Graph) {
                if (((Graph) g).edgeSet().isEmpty()) {

                    Set<CGNode> nodes = (Set<CGNode>) ((Graph) g).vertexSet().stream().collect(Collectors.toSet());
                    Set<String> vs = nodes.stream().map(Object::toString).collect(Collectors.toSet());

                    if (vs.contains(goalCategory)) {
                        jGraphXAdapter.getModel().setStyle(jGraphXAdapter.getVertexToCellMap().get(g), "defaultVertex;fillColor=yellow;opacity=50");
                    } else {
                        boolean contains = false;
                        for (CGNode node : nodes) {
                            if (!node.histories.isEmpty()) {
                                contains = true;
                                break;
                            }
                        }
                        if (!contains) {
                            jGraphXAdapter.getModel().setStyle(jGraphXAdapter.getVertexToCellMap().get(g), "defaultVertex;fillColor=red;opacity=50");
                        } else {
                            jGraphXAdapter.getModel().setStyle(jGraphXAdapter.getVertexToCellMap().get(g), "defaultVertex;fillColor=lightblue;opacity=50");
                        }
                    }
                } else {
                    jGraphXAdapter.getModel().setStyle(jGraphXAdapter.getVertexToCellMap().get(g), "defaultVertex;fillColor=lightgreen;opacity=50");
                    mxCell mx = ((mxCell) jGraphXAdapter.getVertexToCellMap().get(g));

                    JGraphXAdapter<CGNode,DefaultEdge> jGraphXAdapter1 = new JGraphXAdapter<>((Graph<CGNode,DefaultEdge>) g);
                    jGraphXAdapter1.getStylesheet().getDefaultEdgeStyle().put(mxConstants.STYLE_NOLABEL, "1");
                    jGraphXAdapter1.getModel().beginUpdate();

                    for (Object g1 : jGraphXAdapter1.getVertexToCellMap().keySet()) {
                        if (g1 instanceof CGNode) {
                            if (g1.toString().equals(goalCategory)) {
                                jGraphXAdapter1.getModel().setStyle(jGraphXAdapter1.getVertexToCellMap().get(g1), "defaultVertex;fillColor=yellow;opacity=50");
                            } else {
                                /*
                                if (((CGNode) g1).histories.isEmpty()) {
                                    jGraphXAdapter1.getModel().setStyle(jGraphXAdapter1.getVertexToCellMap().get(g1), "defaultVertex;fillColor=red;opacity=50");
                                } else {
                                 */
                                    jGraphXAdapter1.getModel().setStyle(jGraphXAdapter1.getVertexToCellMap().get(g1), "defaultVertex;fillColor=lightblue;opacity=50");
                                }
                        }
                    }
                    mxGraphLayout layout = new mxHierarchicalLayout(jGraphXAdapter1);
                    layout.execute(jGraphXAdapter1.getDefaultParent());
                    jGraphXAdapter1.getModel().endUpdate();
                    mxGraphComponent graphComponent1 = new mxGraphComponent(jGraphXAdapter1);

                    sccMap.put(mx, graphComponent1);
                }
            }
        }


                    mxGraphLayout layout = new mxHierarchicalLayout(jGraphXAdapter);
                    layout.execute(jGraphXAdapter.getDefaultParent());
                    jGraphXAdapter.getModel().endUpdate();
                    graphComponent = new mxGraphComponent(jGraphXAdapter);

                    graphComponent.getGraphControl().addMouseListener(new inspectProofAdapter(graphComponent,sccMap));
                 /*


                        @Override
                        public void mouseClicked(MouseEvent e) {
                            Object cell = graphComponent.getCellAt(e.getX(), e.getY());
                            if (cell instanceof mxCell) {
                                if (sccMap.containsKey(cell)) {

                                    JDialog testFrame = new JDialog();
                                    testFrame.setSize(1200, 800);
                                    testFrame.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);
                                    testFrame.getContentPane().add(sccMap.get(cell));
                                    sccMap.get(cell).getGraphControl().addMouseListener(new inspectProofAdapter(sccMap.get(cell)));
                                    testFrame.pack();
                                    testFrame.setVisible(true);
                                    testFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

// your function
                                } else {
                                    JDialog testFrame = new JDialog();
                                    testFrame.setSize(1200, 800);
                                    testFrame.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);

                                    StringBuilder intermediateResultBuilder = new StringBuilder();
                                    List<Premise> solutions = new ArrayList();

                                    for (History h : ((CGNode) ((Graph) ((mxCell) cell).getValue()).vertexSet().stream().findAny().get()).histories)
                                    {

                                        if (h.parents.size() == 1 && h.parents.stream().findAny().get().isEmpty())
                                        {
                                            solutions.add(h.p);
                                        }
                                        else {
                                            try {
                                                solutions.addAll(h.calculateSolutions());
                                            } catch (VariableBindingException ex) {
                                                throw new RuntimeException(ex);
                                            } catch (ProverException ex) {
                                                throw new RuntimeException(ex);
                                            }
                                        }
                                    }

                                    for (Premise h : solutions) {
                                        intermediateResultBuilder.append(h.toString() + " " + h.getPremiseIDs().toString());
                                        intermediateResultBuilder.append("<br>");

                                    }

                                    JLabel solutionLabel = new JLabel();
                                    solutionLabel.setText("<html><body>" + intermediateResultBuilder.toString() + "</body></html>" );

                                    testFrame.getContentPane().add(solutionLabel);
                                    testFrame.pack();
                                    testFrame.setVisible(true);
                                    testFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
                                }
                            }
                        }
                    }

                    */







                //    graphComponent.getGraphControl().addMouseListener();


                    JDialog testFrame = new JDialog();
                    testFrame.setSize(1200, 800);
                    testFrame.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);
                    testFrame.getContentPane().add(graphComponent);
                    testFrame.pack();
                    testFrame.setVisible(true);
                    testFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
                }


            }

class inspectProofAdapter extends MouseAdapter {
    public mxGraphComponent gc;
    private HashMap<mxCell,mxGraphComponent> sccMap;

    public inspectProofAdapter(mxGraphComponent gc, HashMap<mxCell,mxGraphComponent> sccMap)
    {this.gc = gc;
    this.sccMap = sccMap;}

    @Override
    public void mouseClicked(MouseEvent e) {
        Object cell = gc.getCellAt(e.getX(), e.getY());
        if (cell instanceof mxCell) {
            if (this.sccMap != null && sccMap.containsKey(cell)) {

                JDialog testFrame = new JDialog();
                testFrame.setSize(1200, 800);
                testFrame.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);
                sccMap.get(cell).getGraphControl().addMouseListener(new inspectSCCAdapter(sccMap.get(cell)));
                testFrame.getContentPane().add(sccMap.get(cell));
                testFrame.pack();
                testFrame.setVisible(true);
                testFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

// your function
            } else {
                JDialog testFrame = new JDialog();
                testFrame.setSize(1200, 800);
                testFrame.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);

                StringBuilder intermediateResultBuilder = new StringBuilder();
                List<Premise> solutions = new ArrayList();

                for (History h : ((CGNode) ((Graph) ((mxCell) cell).getValue()).vertexSet().stream().findAny().get()).histories)
                {

                    if (h.parents.size() == 1 && h.parents.stream().findAny().get().isEmpty())
                    {
                        solutions.add(h.p);
                    }
                    else {
                        try {
                            solutions.addAll(h.calculateSolutions());
                        } catch (VariableBindingException ex) {
                            throw new RuntimeException(ex);
                        } catch (ProverException ex) {
                            throw new RuntimeException(ex);
                        }
                    }
                }

                for (Premise h : solutions) {
                    intermediateResultBuilder.append(h.toString() + " " + h.getPremiseIDs().toString());
                    intermediateResultBuilder.append("<br>");

                }

                JLabel solutionLabel = new JLabel();
                solutionLabel.setText("<html><body>" + intermediateResultBuilder.toString() + "</body></html>" );

                testFrame.getContentPane().add(solutionLabel);
                testFrame.pack();
                testFrame.setVisible(true);
                testFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
            }
        }
    }
}

class inspectSCCAdapter extends MouseAdapter {
    public mxGraphComponent gc;
    private HashMap<mxCell,mxGraphComponent> sccMap;

    public inspectSCCAdapter(mxGraphComponent gc)
    {this.gc = gc;}


    @Override
    public void mouseClicked(MouseEvent e) {
        Object cell = gc.getCellAt(e.getX(), e.getY());
        if (cell instanceof mxCell) {

                JDialog testFrame = new JDialog();
                testFrame.setSize(1200, 800);
                testFrame.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);

                StringBuilder intermediateResultBuilder = new StringBuilder();
                List<Premise> solutions = new ArrayList();

                for (History h : ((CGNode) ((mxCell) cell).getValue()).histories)
                {

                    if (h.parents.size() == 1 && h.parents.stream().findAny().get().isEmpty())
                    {
                        solutions.add(h.p);
                    }
                    else {
                        try {
                            solutions.addAll(h.calculateSolutions());
                        } catch (VariableBindingException ex) {
                            throw new RuntimeException(ex);
                        } catch (ProverException ex) {
                            throw new RuntimeException(ex);
                        }
                    }
                }

                for (Premise h : solutions) {
                    intermediateResultBuilder.append(h.toString() + " " + h.getPremiseIDs().toString());
                    intermediateResultBuilder.append("<br>");

                }

                JLabel solutionLabel = new JLabel();
                solutionLabel.setText("<html><body>" + intermediateResultBuilder.toString() + "</body></html>" );

                testFrame.getContentPane().add(solutionLabel);
                testFrame.pack();
                testFrame.setVisible(true);
                testFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
            }
        }
    }
