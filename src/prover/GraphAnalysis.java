package prover;

import com.mxgraph.layout.hierarchical.mxHierarchicalLayout;
import com.mxgraph.layout.mxGraphLayout;
import com.mxgraph.model.mxCell;
import com.mxgraph.swing.mxGraphComponent;
import com.mxgraph.util.mxConstants;
import org.jgrapht.Graph;
import org.jgrapht.ext.JGraphXAdapter;
import prover.categoryGraph.CGNode;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.HashMap;
import java.util.Set;
import java.util.stream.Collectors;

public class GraphAnalysis {

    private Graph stronglyConnectedGraph;
    private String goalCategory;
    private mxGraphComponent graphComponent;
    private HashMap<mxCell,mxGraphComponent> sccMap = new HashMap<>();

    public GraphAnalysis(String goalCategory, Graph stronglyConnectedGraph)
    {
        this.goalCategory = goalCategory;
        this.stronglyConnectedGraph = stronglyConnectedGraph;
    }

    public void displayGraph()
    {
        JGraphXAdapter jGraphXAdapter = new JGraphXAdapter(stronglyConnectedGraph);
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
                    //TODO open strongly connected graph in new frame on click
                    JGraphXAdapter jGraphXAdapter1 = new JGraphXAdapter((Graph) g);
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




                            /* for test purposes
                            JDialog testFrame = new JDialog();
                            testFrame.setSize(1200, 800);
                            testFrame.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);
                            testFrame.getContentPane().add(graphComponent1);
                            testFrame.pack();
                            testFrame.setVisible(true);
                            testFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
    */

                    mxGraphLayout layout = new mxHierarchicalLayout(jGraphXAdapter);
                    layout.execute(jGraphXAdapter.getDefaultParent());
                    jGraphXAdapter.getModel().endUpdate();
                    mxGraphComponent graphComponent = new mxGraphComponent(jGraphXAdapter);

                    graphComponent.getGraphControl().addMouseListener(new MouseAdapter() {
                        @Override
                        public void mouseClicked(MouseEvent e) {
                            Object cell = graphComponent.getCellAt(e.getX(), e.getY());
                            if (cell != null && cell instanceof mxCell) {
                                if (sccMap.containsKey(cell)) {

                                    JDialog testFrame = new JDialog();
                                    testFrame.setSize(1200, 800);
                                    testFrame.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);
                                    testFrame.getContentPane().add(sccMap.get(cell));
                                    testFrame.pack();
                                    testFrame.setVisible(true);
                                    testFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

// your function
                                }
                            }
                        }
                    });

                    JDialog testFrame = new JDialog();
                    testFrame.setSize(1200, 800);
                    testFrame.setModalityType(Dialog.DEFAULT_MODALITY_TYPE);
                    testFrame.getContentPane().add(graphComponent);
                    testFrame.pack();
                    testFrame.setVisible(true);
                    testFrame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
                }


            }
