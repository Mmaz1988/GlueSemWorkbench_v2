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

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

public class GraphAnalysis {

    private Graph<Graph<CGNode,DefaultEdge>,DefaultEdge> stronglyConnectedGraph;
    private String goalCategory;
    private mxGraphComponent graphComponent;
    private HashMap<mxCell,mxGraphComponent> sccMap = new HashMap<>();

    public GraphAnalysis(String goalCategory, Graph<Graph<CGNode,DefaultEdge>,DefaultEdge> stronglyConnectedGraph)
    {
        this.goalCategory = goalCategory;
        this.stronglyConnectedGraph = stronglyConnectedGraph;
    }


    public void returnJSONGraph()
    {

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
