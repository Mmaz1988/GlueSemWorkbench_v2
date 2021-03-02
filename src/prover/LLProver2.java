package prover;

import glueSemantics.linearLogic.*;
import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.*;
import main.Settings;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.GabowStrongConnectivityInspector;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.traverse.TopologicalOrderIterator;
import prover.categoryGraph.CGNode;
import prover.categoryGraph.History;
import test.Debugging;
import utilities.LexVariableHandler;

import java.io.IOException;
import java.util.*;

public class LLProver2 {

    private static Settings settings;


    private Sequent currentSequent;

    //TODO Add a third chart for modifiers and both atomic elements as well as non atomic elements are first
    //run through the modifier chart.

    //   private HashMap<String,List<Premise>> modifierChart = new HashMap<>();

    // A chart that associates variables that are compiled out with their original formula.
    // This is necessary to instantiate variables that are atmoic elements rather than variables that occur in formulas.


    private LinkedList<Premise> agenda;
    private LinkedList<History> solutions = new LinkedList<>();

    private StringBuilder proofBuilder;

    private HashSet<Integer> goalIDs = new HashSet<>();

    public Debugging db;


    /**
     * LLProver version 2.0
     * Implements Lev's rather than Hepple's algorithm. Avoids need for accidental binding.
     *
     * @param settings
     */
    public LLProver2(Settings settings) {
        setSettings(settings);
        this.proofBuilder = new StringBuilder();
    }

    public LLProver2(Settings settings, StringBuilder proofBuilder) {
        this.proofBuilder = proofBuilder;
        setSettings(settings);
    }



    public void deduce(Sequent seq) throws ProverException, VariableBindingException {

        this.db = new Debugging();
        this.currentSequent = seq;
        LinkedList<Premise> agenda = new LinkedList<>();

        long startTime = System.nanoTime();

        StringBuilder sb = new StringBuilder();
        sb.append("Sequent:");
        sb.append(System.lineSeparator());
        for (Premise le : currentSequent.getLhs()) {
            sb.append(le);
            sb.append(System.lineSeparator());
        }

        System.out.println(sb.toString());

        //TODO insert boolean for distinguishing between sdout and file
        if (true) {
            proofBuilder.append(sb.toString());
            proofBuilder.append(System.lineSeparator());
            proofBuilder.append(System.lineSeparator());
        }

        for (Premise p : currentSequent.getLhs()) {
            List<Premise> compiled = convert(p);
            agenda.addAll(compiled);
        }

        String goalCategory = findAtomicGoal(agenda);
        System.out.println("Automatically detected goal category: " + goalCategory);

        StringBuilder ab = new StringBuilder();
        ab.append("Agenda:");
        ab.append(System.lineSeparator());
        for (Premise p : agenda) {
            ab.append(p);
            ab.append(System.lineSeparator());
        }
        System.out.println(ab.toString());

        //TODO insert boolean for distinguishing between sdout and file
        if (true) {
            proofBuilder.append(ab.toString());
            proofBuilder.append(System.lineSeparator());
            proofBuilder.append(System.lineSeparator());
        }

        this.agenda = agenda;

        List<LLTerm> initialCategories = new ArrayList<>();
        HashMap<String,List<Premise>> category2premiseMapping = new HashMap<>();

        for (Premise p : agenda) {
            if (p.getPremiseIDs().size() == 1) {
                goalIDs.addAll(p.getPremiseIDs());
            }

            if (!category2premiseMapping.containsKey(p.getGlueTerm().category()))
            {
              category2premiseMapping.put(p.getGlueTerm().category(), new ArrayList<>());
            }
            category2premiseMapping.get(p.getGlueTerm().category()).add(p);

            Boolean contains = false;
                for (LLTerm glue : initialCategories) {
                    if (p.getGlueTerm().category().equals(glue.category())) {
                        contains = true;
                        break;
                    }
                }
                    if(!contains)
                    {
                        initialCategories.add(p.getGlueTerm());
                    }
            }
        List<LLTerm> copy = new ArrayList<LLTerm>(initialCategories);

        Graph<CGNode, DefaultEdge> categoryGraph2 = calculateCategoryGraph(copy,category2premiseMapping);
        GabowStrongConnectivityInspector ins2 = new GabowStrongConnectivityInspector(categoryGraph2);
        Graph scc2 = ins2.getCondensation();

        TopologicalOrderIterator graphIter2 = new TopologicalOrderIterator(scc2);

        while (graphIter2.hasNext())
        {
            Graph sccKey = (Graph) graphIter2.next();

            //Element is outside of an scc
            if (sccKey.vertexSet().size() == 1)
            {
                CGNode node =  (CGNode) sccKey.vertexSet().stream().findAny().get();

                if (node.nodeType.equals(CGNode.type.CATEGORY)) {

                    for (Object edge : scc2.incomingEdgesOf(sccKey))
                        {
                           CGNode parentConnectorNode = (CGNode) ((Graph) scc2.getEdgeSource(edge)).vertexSet().stream().findAny().get();
                           node.histories.addAll(parentConnectorNode.histories);
                        }
                    //Compress histories
                }
                //Element is a connector node
                if (node.nodeType.equals(CGNode.type.CONNECTOR))
                {
                    Object childNodeGraph = categoryGraph2.outgoingEdgesOf(node).stream().findAny().get();
                    CGNode childNode = (CGNode) scc2.getEdgeTarget(childNodeGraph);

                    List<CGNode> parents = new ArrayList<>();
                    for (Object edge : categoryGraph2.incomingEdgesOf(node)) {
                        CGNode parentNode = (CGNode) categoryGraph2.getEdgeSource((DefaultEdge) edge);
                        parents.add(parentNode);
                    }
                    CGNode arg;
                    CGNode func;
                    if (parents.get(0).toString().startsWith(parents.get(1).toString()))
                    {
                        func = parents.get(0);
                        arg = parents.get(1);
                    }
                    else
                    {
                        func = parents.get(1);
                        arg = parents.get(0);
                    }
                    for (History h1 : func.histories)
                    {
                        for (History h2 : arg.histories)
                        {
                            History result = combineHistories(h1,h2);
                            if (result != null) {
                                node.histories.add(result);
                            }
                        }
                    }
                }
            }
            else
            {
                List<CGNode> outputNodes = new ArrayList<>();
                List<CGNode> inputNodes = new ArrayList<>();
                Set<History> sccHistories = new HashSet<>();

                for (Object node : sccKey.vertexSet())
                {

                    if (categoryGraph2.incomingEdgesOf((CGNode) node).size() ==0)
                    {
                        inputNodes.add((CGNode) node);
                            sccHistories.addAll(((CGNode) node).histories);
                    } else
                    {
                        boolean input = true;
                        for (Object edge : categoryGraph2.incomingEdgesOf((CGNode) node))
                        {
                            if (sccKey.vertexSet().contains(categoryGraph2.getEdgeSource((DefaultEdge) edge)))
                            {
                                input = false;
                            } else
                            {
                                sccHistories.addAll(categoryGraph2.getEdgeSource((DefaultEdge) edge).histories);
                            }
                        }
                        if (input)
                        {
                            inputNodes.add((CGNode) node);
                            sccHistories.addAll(((CGNode) node).histories);
                        }
                    }
                    if (categoryGraph2.outgoingEdgesOf((CGNode) node).size() > 0)
                    {
                        for (Object edge : categoryGraph2.outgoingEdgesOf((CGNode) node))
                        {
                            if (!(sccKey.vertexSet().contains(categoryGraph2.getEdgeTarget((DefaultEdge) edge)))
                            && ((CGNode) categoryGraph2.getEdgeTarget((DefaultEdge) edge)).nodeType.equals(CGNode.type.CONNECTOR) )
                            {
                                outputNodes.add((CGNode) node);
                                sccHistories.addAll(((CGNode) node).histories);
                                break;
                            }
                        }
                        }
                    else {
                        if (((CGNode) node).category.equals(goalCategory))
                        {
                            outputNodes.add((CGNode) node);
                            sccHistories.addAll(((CGNode) node).histories);
                    }
                    }
                }

                List<History> sccAgenda = new ArrayList<>();
                sccAgenda.addAll(sccHistories);
                Chart c = chartDeduce(sccAgenda);

                for (CGNode outputNode : outputNodes)
                {
                    if (c.atomicChart.containsKey(outputNode.category))
                    {
                        outputNode.histories.addAll(c.atomicChart.get(outputNode.category));

                    } else if (c.nonAtomicChart.containsKey(outputNode.category)) {
                        outputNode.histories.addAll(c.nonAtomicChart.get(outputNode.category));

                }
                }
            }
        }


        long endTime = System.nanoTime();
        db.computationTime = endTime - startTime;
        proofBuilder.append(System.lineSeparator());

    }

    public Chart chartDeduce(List<History> histories) throws VariableBindingException, ProverException {
        Chart chart = new Chart();

        List<History> agenda = new ArrayList<>();
        agenda.addAll(histories);

        while (!agenda.isEmpty()) {
            ListIterator<History> agendaIterator = agenda.listIterator();

            while (agendaIterator.hasNext()) {
                History current = agendaIterator.next();
                agendaIterator.remove();

                if (current.premise.getGlueTerm() instanceof LLFormula) {

                    if (chart.atomicChart.containsKey(current.category))
                    {
                        for (History h : chart.atomicChart.get(current.category)) {
                            History combined = combineHistories(current,h);
                            if (combined != null) {
                                agendaIterator.add(combined);
                            }
                        }
                    }
                    if (chart.nonAtomicChart.containsKey(((LLFormula) current.premise.getGlueTerm()).getLhs()))
                    {
                        chart.nonAtomicChart.get(((LLFormula) current.premise.getGlueTerm()).getLhs()).add(current);
                    } else
                    {
                        Set<History> histories1 = new HashSet<>();
                        histories1.add(current);
                        chart.nonAtomicChart.put(((LLFormula) current.premise.getGlueTerm()).getLhs().category(),histories1);
                    }

                } else if (current.premise.getGlueTerm() instanceof LLAtom) {
                    if (chart.nonAtomicChart.containsKey(current.category))
                    {
                        for (History h : chart.nonAtomicChart.get(current.category))
                        {
                            History combined = combineHistories(h,current);
                            if (combined != null) {
                                agendaIterator.add(combined);
                            }
                        }
                    }

                    if (chart.atomicChart.containsKey(current.category))
                    {
                        chart.atomicChart.get(current.category).add(current);
                    } else
                    {
                        Set<History> histories1 = new HashSet<>();
                        histories1.add(current);
                        chart.atomicChart.put(current.category,histories1);
                    }
                }
            }
        }
        return chart;
    }


    public Graph<CGNode,DefaultEdge> calculateCategoryGraph(List<LLTerm> initialCategories, HashMap<String,List<Premise>> category2premiseMapping)
    {
        HashMap<String,Set<LLTerm>> atomicChart = new HashMap<>();
        HashMap<String,Set<LLTerm>> nonAtomicChart = new HashMap<>();
        Graph<CGNode,DefaultEdge> categoryGraph = new DefaultDirectedGraph<CGNode,DefaultEdge>(DefaultEdge.class);

        HashMap<String,CGNode> category2graphmapping = new HashMap<>();

        while (!initialCategories.isEmpty()) {

            ListIterator<LLTerm> categoryIterator = initialCategories.listIterator();

            while (categoryIterator.hasNext()) {
                LLTerm category = categoryIterator.next();
                categoryIterator.remove();
                CGNode currentNode;
                if (!category2graphmapping.containsKey(category.category())) {
                    currentNode = new CGNode(category.category(), CGNode.type.CATEGORY);

                    if (category2premiseMapping.containsKey(currentNode.category)) {
                        for (Premise p : category2premiseMapping.get((currentNode).category)) {

                            History h = new History(currentNode.category, p.getPremiseIDs(), null, p);

                            if (p.getGlueTerm() instanceof LLFormula) {
                                h.discharges.addAll(((LLFormula) p.getGlueTerm()).getLhs().orderedDischarges.keySet());
                                h.requirements.addAll(((LLFormula) p.getGlueTerm()).getRhs().dischargeRequirements());
                            }
                            currentNode.histories.add(h);
                        }
                    }
                    category2graphmapping.put(category.category(), currentNode);
                    categoryGraph.addVertex(currentNode);
                } else
                {
                    currentNode = category2graphmapping.get(category.category());
                }
                if (category instanceof LLAtom) {
                    if (!atomicChart.containsKey(category.category()))
                    {
                        if (nonAtomicChart.containsKey(category.category())) {
                            for (LLTerm nonAtomic : nonAtomicChart.get(category.category())) {
                                CGNode connectorNode = new CGNode(LexVariableHandler.returnNewVar(LexVariableHandler.variableType.connectorNode), CGNode.type.CONNECTOR);

                                CGNode resultNode;
                                if (!category2graphmapping.containsKey(((LLFormula) nonAtomic).getRhs().category())) {
                                    resultNode = new CGNode(((LLFormula) nonAtomic).getRhs().category(), CGNode.type.CATEGORY);
                                    category2graphmapping.put(((LLFormula) nonAtomic).getRhs().category(), resultNode);
                                    categoryGraph.addVertex(resultNode);
                                } else
                                {
                                    resultNode = category2graphmapping.get(((LLFormula) nonAtomic).getRhs().category());
                                }

                                categoryGraph.addVertex(connectorNode);
                                categoryGraph.addEdge(currentNode, connectorNode);
                                categoryGraph.addEdge(category2graphmapping.get(nonAtomic.category()), connectorNode);
                                categoryGraph.addEdge(connectorNode, resultNode);
                                if (!initialCategories.contains(((LLFormula) nonAtomic).getRhs())) {
                                    categoryIterator.add(((LLFormula) nonAtomic).getRhs());
                                }
                            }
                        }
                    }

                    if (atomicChart.containsKey(category.category())) {
                        atomicChart.get(category.category()).add(category);
                    } else {
                        Set<LLTerm> premises = new HashSet<>();
                        premises.add(category);
                        atomicChart.put(category.category(), premises);

                    }

                }

                if (category instanceof LLFormula) {
                    if (atomicChart.containsKey(((LLFormula) category).getLhs().category())) {
                        for (LLTerm atomic : atomicChart.get(((LLFormula) category).getLhs().category())) {
                            CGNode resultNode;
                            CGNode connectorNode = new CGNode(LexVariableHandler.returnNewVar(LexVariableHandler.variableType.connectorNode), CGNode.type.CONNECTOR);
                            if (!category2graphmapping.containsKey(((LLFormula) category).getRhs().category())) {
                                resultNode = new CGNode(((LLFormula) category).getRhs().category(), CGNode.type.CATEGORY);
                                category2graphmapping.put(((LLFormula) category).getRhs().category(), resultNode);
                                categoryGraph.addVertex(resultNode);
                            } else
                            {
                                resultNode = category2graphmapping.get(((LLFormula) category).getRhs().category());
                            }
                            categoryGraph.addVertex(connectorNode);
                            categoryGraph.addEdge(currentNode, connectorNode);
                            categoryGraph.addEdge(category2graphmapping.get(atomic.category()), connectorNode);
                            categoryGraph.addEdge(connectorNode, resultNode);
                            if (!initialCategories.contains(((LLFormula) category).getRhs())) {
                                categoryIterator.add(((LLFormula) category).getRhs());
                            }
                        }
                    }
                    if (nonAtomicChart.containsKey(((LLFormula) category).getLhs().category())) {
                        nonAtomicChart.get(((LLFormula) category).getLhs().category()).add(category);
                    } else {
                        Set<LLTerm> premises = new HashSet<>();
                        premises.add(category);
                        nonAtomicChart.put(((LLFormula) category).getLhs().category(), premises);
                    }
                }
            }
        }
        return categoryGraph;
    }


    public History combineHistories(History h1, History h2) throws VariableBindingException, ProverException {
        if (Collections.disjoint(h1.indexSet,h2.indexSet)) {
            if (h2.indexSet.containsAll(h1.discharges)) {
                if (Collections.disjoint(h1.requirements, h2.indexSet)) {
                    Premise p = combinePremises(h1.premise, h2.premise);

                    Set<Integer> union = new HashSet<>();
                    union.addAll(h1.indexSet);
                    union.addAll(h2.indexSet);
                    HashMap<Integer, History> parentNodes = new HashMap<>();
                    parentNodes.put(0, h1);
                    parentNodes.put(1, h2);

                    History result = new History(((LLFormula)h1.premise.getGlueTerm()).getRhs().category(), union, parentNodes, p);

                    if (result.indexSet.equals(goalIDs))
                    {
                        solutions.add(result);
                    }

                    return result;

                }
            }
        }
        return null;
    }


    public Premise combinePremises(Premise functor, Premise argument) throws VariableBindingException, ProverException {

        Premise func = new Premise(functor.getPremiseIDs(), functor.getSemTerm().clone(), functor.getGlueTerm().clone());
        Premise argumentClone = null;


        Boolean variableArgument = false;
        if (((LLAtom)argument.getGlueTerm()).lltype.equals(LLAtom.LLType.VAR)) {
            variableArgument = true;

            if (((LLAtom)((LLFormula)  functor.getGlueTerm()).getLhs()).lltype.equals(LLAtom.LLType.CONST)) {
                argumentClone = new Premise(argument.getPremiseIDs(), argument.getSemTerm().clone(),
                        ((LLFormula) func.getGlueTerm()).getLhs().clone());
                argumentClone.getGlueTerm().getAssumptions2().addAll(argument.getGlueTerm().getAssumptions2());
            }else {
                return null;
            }

        }
        else
        {
            argumentClone = new Premise(argument.getPremiseIDs(), argument.getSemTerm().clone(),
                    argument.getGlueTerm().clone());
        }

        LinkedHashSet<Equality> eqs = ((LLFormula) func.getGlueTerm()).getLhs().checkCompatibility(argument.getGlueTerm());

        if (eqs == null) {
            return null;
        }

        if (eqs.size() > 0) {

            //If there are duplicate bindings no valid proof can be reached.
            if (LLProver2.checkDuplicateBinding(eqs)) {
                throw new VariableBindingException();
            } else {
                //instantiates variables with constants (i.e. skolemizes the formula so it can take a constant)

                for (Equality eq : eqs) {
                    ((LLFormula) func.getGlueTerm()).instantiateVariables(eq);
                }
            }
        }

        Premise combined = null;

        HashSet<Integer> combined_IDs = new HashSet<>();
        if (((LLFormula) func.getGlueTerm()).getLhs().checkEquivalence(argumentClone.getGlueTerm())
                && Collections.disjoint(func.getPremiseIDs(), argument.getPremiseIDs())) {
            combined_IDs.addAll(func.getPremiseIDs());
            combined_IDs.addAll(argument.getPremiseIDs());


            if (((LLFormula) func.getGlueTerm()).getLhs().getOrderedDischarges().isEmpty()) {

                SemanticRepresentation reducedSem = combine(func,argumentClone).betaReduce();

                LLTerm newTerm = ((LLFormula) func.getGlueTerm()).getRhs();
                if (func.getGlueTerm().getVariable() != null) {
                    newTerm.setVariable(func.getGlueTerm().getVariable());
                    if (newTerm instanceof LLFormula) {
                        for (LLAtom var : newTerm.getVariable()) {
                            newTerm.updateBoundVariables(var);
                        }
                    }
                }

                combined = new Premise(combined_IDs, reducedSem, newTerm);

                if (variableArgument && combined != null)
                {

                        newTerm.getVariableAssignment().put((LLAtom) argument.getGlueTerm(), (LLAtom) argumentClone.getGlueTerm());

                    newTerm.getVariableAssignment().putAll(functor.getGlueTerm().getVariableAssignment());
                }

        }
         else {
                if (checkDischarges(func, argument)) {

                    if (!func.getGlueTerm().getVariableAssignment().keySet().isEmpty() && !argumentClone.getGlueTerm().getVariableAssignment().keySet().isEmpty())
                    {
                        boolean subset = false;

                        for (LLAtom funcCategory : functor.getGlueTerm().getVariableAssignment().keySet())
                        {
                            if (argumentClone.getGlueTerm().getVariableAssignment().containsKey(funcCategory))
                            {
                                if (functor.getGlueTerm().getVariableAssignment().get(funcCategory).
                                        equals(argumentClone.getGlueTerm().getVariableAssignment().get(funcCategory)));
                                subset = true;
                                break;
                            }
                        }

                        if (!subset)
                        {return  null;}
                    }

                    if (!argument.getGlueTerm().getVariableAssignment().keySet().isEmpty())
                    {
                      LinkedHashSet<Equality> eqs2 =   new LinkedHashSet<>();

                      for (LLAtom key : argument.getGlueTerm().getVariableAssignment().keySet())
                      {
                          for (LLAtom key2 : ((LLFormula) func.getGlueTerm()).getBoundVariables().keySet())
                          {
                              if (key2.category().equals(key.category()))
                              {
                                  for (LLAtom key3 : ((LLFormula) func.getGlueTerm()).getBoundVariables().get(key2))
                                  {
                                      eqs2.add(new Equality(key3,argument.getGlueTerm().getVariableAssignment().get(key)));
                                  }
                              }
                          }


                      }

                        for (Equality eq : eqs2) {
                            ((LLFormula) func.getGlueTerm()).instantiateVariables(eq);
                        }

                    }

                    SemanticRepresentation temp = argument.getSemTerm().clone();

                    //LinkedHashMap<Integer,Premise> discharges =  ((LLFormula) func.getGlueTerm()).getLhs().getOrderedDischarges();
                    LinkedList<Map.Entry<Integer, Premise>> discharges = new LinkedList<>(((LLFormula) func.getGlueTerm()).getLhs().getOrderedDischarges().entrySet());


                    LLTerm argumentGlueClone = argument.getGlueTerm().clone();

                    while (!discharges.isEmpty())
                    {
                        Premise p = discharges.removeLast().getValue();
                        temp = new SemFunction((SemAtom) p.getSemTerm(),temp);
                        argumentGlueClone.getAssumptions2().remove(p);

                       }

                    argumentClone = new Premise(argument.getPremiseIDs(),temp,argumentGlueClone);

                    SemanticRepresentation reducedSem = combine(func,argumentClone).betaReduce();

                    LLTerm newTerm = ((LLFormula) func.getGlueTerm()).getRhs();
                    if (func.getGlueTerm().getVariable() != null) {
                        newTerm.setVariable(func.getGlueTerm().getVariable());
                        if (newTerm instanceof LLFormula) {
                            for (LLAtom var : newTerm.getVariable()) {
                                newTerm.updateBoundVariables(var);
                            }
                        }
                    }

                    combined = new Premise(combined_IDs, reducedSem,  newTerm);

                }

            }

            if (combined != null) {
                combined.getGlueTerm().assumptions2.addAll(func.getGlueTerm().assumptions2);
                combined.getGlueTerm().assumptions2.addAll(argumentClone.getGlueTerm().assumptions2);
            }


        }

        if (combined != null)
        {
            String f = "";
            String a = "";
            if (getSettings().isGlueOnly())
            {
                f = functor.getGlueTerm().toPlainString();
                a = argument.getGlueTerm().toPlainString();
            }
            else
            {
                f = functor.toString();
                a = argument.toString();
            }

           System.out.println("Combining " + f + " and " + a);
            System.out.println("to: " + combined.toString());

            //TODO sdout vs file
            if (true)
            {
                proofBuilder.append("Combining " + f + " and " + a);
                proofBuilder.append(System.lineSeparator());
                proofBuilder.append("to: " + combined.toString());
                proofBuilder.append(System.lineSeparator());
            }


        }

        return combined;

    }

    public static SemanticRepresentation combine(Premise func, Premise argument) throws ProverException
    {
        SemanticRepresentation reducedSem;
        if (getSettings().isBetaReduce()) {
        //    System.out.println("Beta reduced: " + func.getSemTerm().toString() + ", " + argument.getSemTerm().toString());
            reducedSem = new FuncApp(func.getSemTerm(), argument.getSemTerm()).betaReduce();
        //    System.out.println("To:" + reducedSem.toString());
        } else
            reducedSem = new FuncApp(func.getSemTerm(), argument.getSemTerm());

        return reducedSem;

    }

    public Boolean checkDischarges(Premise functor, Premise argument) {

        for (Integer key : ((LLFormula) functor.getGlueTerm()).getLhs().getOrderedDischarges().keySet()) {
            Premise t = ((LLFormula) functor.getGlueTerm()).getLhs().getOrderedDischarges().get(key);
            if (!argument.getGlueTerm().assumptions2.contains(t)){
                return false;
        }
    }
        return true;

    }


    public LinkedList<Premise> convert(Premise p)
    {
        LinkedList<Premise> compiled = new LinkedList<>();

        if (p.getGlueTerm() instanceof LLFormula || p.getGlueTerm() instanceof LLQuantEx) {

            // if (!p.getGlueTerm().isModifier()) {

            LLTerm t = p.getGlueTerm();

                while (t instanceof LLQuantEx) {
                    t = ((LLQuantEx) t).getScope();
                }


            LLFormula f = (LLFormula) t;
            LLTerm l = f.getLhs();


            if (l instanceof LLFormula) {
                db.compilations++;

                //Update history


                //Compile out stuff
                LLFormula compiledGlue = new LLFormula(((LLFormula) l).getRhs(), f.getRhs(), f.isPolarity(), f.getVariable());
                compiledGlue.getLhs().orderedDischarges.putAll(l.getOrderedDischarges());
                LLTerm outGlue = ((LLFormula) l).getLhs();


                //outGlue.assumptions.add(outGlue);
                //  compiledGlue.getLhs().getOrderedDischarges().add(outGlue);

                SemType newtype = null;


                //Routine to define type of compiled out variables in typed lambda calculus.
                try{
                    SemType compileType = l.getType();
                    SemType currentType = null;

                    if (p.getSemTerm() instanceof SemFunction) {
                        currentType =((SemFunction) p.getSemTerm()).getBinder().getType();
                    }
                    else if (p.getSemTerm() instanceof SemSet)
                    {
                        currentType = p.getSemTerm().getType().getLeft();
                    }

                    if (compileType.typeStructureEquals(currentType)) {
                        newtype = new SemType(currentType.getLeft().clone());
                    }
                    else
                    {
                        boolean typeMismatch = true;

                        SemType tempType;

                        if (p.getSemTerm() instanceof SemSet)
                        {
                           tempType =  p.getSemTerm().getType().getLeft().getRight().clone();
                        }
                        else {
                            tempType = ((SemFunction) p.getSemTerm()).getBinder().getType().getRight().clone();
                        }
                        while (typeMismatch)
                        {

                            if ( l.getType().typeStructureEquals(tempType))
                            {
                                typeMismatch = false;
                                newtype = tempType.getLeft();
                                break;
                            }
                            else
                            {
                                tempType = tempType.getRight();
                            }

                            if (((SemFunction) p.getSemTerm()).getBinder().getType().getRight() == null)
                            {

                                throw new IOException("Typemismatch between glue type and lambda type.");
                            }
                        }

                    }
               //     ((SemFunction) p.getSemTerm()).getBinder().setType(((SemFunction) p.getSemTerm()).getBinder().getType().getRight());
                }catch(Exception e)
                {newtype = new SemType(((LLFormula) l).getLhs().getType());
                    System.out.println("Semantic side inherits type from linear logic side.");
                }



                SemAtom asumptionVar = new SemAtom(SemAtom.SemSort.VAR,
                        LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE), newtype);


                Premise assumption = new Premise(currentSequent.getNewID(), asumptionVar, outGlue);


                compiledGlue.getLhs().getOrderedDischarges().put(assumption.getPremiseIDs().stream().findAny().get(),assumption);

                Premise compiledPremise = new Premise(p.getPremiseIDs(), p.getSemTerm(), compiledGlue);


                assumption.getGlueTerm().assumptions2.add(assumption);

                //  compiled.add(compiledPremise);
                // compiled.add(assumption);



                List<Premise> recurseCompiled = convert(compiledPremise);
                List<Premise> recurseAssumption = convert(assumption);

                compiled.addAll(recurseCompiled);
                compiled.addAll(recurseAssumption);

                return compiled;


            } else if (f.isNested() && !f.isModifier()) {
                SemanticRepresentation tempSem;
                if (p.getSemTerm() instanceof SemFunction) {
                    tempSem = ((SemFunction) p.getSemTerm()).getFuncBody();
                }
                else{
                    tempSem = p.getSemTerm();
                }

                Premise temp = new Premise(p.getPremiseIDs(),tempSem, f.getRhs());
                LinkedList<Premise> tempList = convert(temp);

                for (int i = 1; i < tempList.size(); i++) {
                    compiled.add(tempList.get(i));
                }

                LLFormula newLogic = new LLFormula(f.getLhs(), tempList.getFirst().getGlueTerm(),
                        tempList.getFirst().getGlueTerm().isPolarity(), f.getVariable());

                p.setGlueTerm(newLogic);


            }

        }
        compiled.add(p);


        return compiled;
    }


    /**
     * Checks for duplicate bindings and returns false if a variable is assigned more than one value
     * @param in A set of variable bindings
     * @return true if no duplicate bindings were detected, false if otherwise
     */
    private static boolean checkDuplicateBinding(LinkedHashSet<Equality> in) {
        List<Equality> eqs = new ArrayList<>(in);

        // no multiple assignments possible
        if (eqs.size() <= 1)
            return false;

        for (int i = 0; i < eqs.size(); i++)
        {
            for (int j = 0; j <eqs.size(); j++)
            {
                if (eqs.get(i).getVariable().getName().equals(eqs.get(j).getVariable().getName())
                        && eqs.get(i).getVariable().getType().equals(eqs.get(j).getVariable().getType())
                        && !(eqs.get(i).getConstant().getName().equals(eqs.get(j).getConstant().getName())))
                {
                    return true;
                }
            }
        }
        return false;
    }

    public static Settings getSettings() {
        return settings;
    }

    public static void setSettings(Settings settings) {
        LLProver2.settings = settings;
    }

    //Getter and Setter
    public LinkedList<History> getSolutions() {
        return solutions;
    }

    public void setSolutions(LinkedList<History> solutions) {
        this.solutions = solutions;
    }

    public String findAtomicGoal(List<Premise> premises)
    {
        List<LLAtom> allAtoms = new ArrayList<>();
        List<String> positive = new ArrayList<>();
        List<String> negative = new ArrayList<>();


        List<Premise> agendaCopy = new ArrayList<>(premises);

        ListIterator<Premise> iter = agendaCopy.listIterator();

        while (iter.hasNext())
        {
            Premise p = iter.next();

            if (p.getGlueTerm().isModifier())
            {
                iter.remove();
            }
            allAtoms.addAll(p.getGlueTerm().returnAllAtoms());
        }

        for (LLAtom atom : allAtoms)
        {
            if (atom.isPolarity())
            {
                positive.add(atom.category());
            } else
            {
                negative.add(atom.category());
            }
        }


        ListIterator<String> negIter = negative.listIterator();

        while (negIter.hasNext())
        {
            String negAtom = negIter.next();
            ListIterator<String> posIter = positive.listIterator();
            while (posIter.hasNext())
            {
                String posAtom = posIter.next();
                if (negAtom.equals(posAtom))
                {
                    posIter.remove();
                    negIter.remove();
                    break;
                }
            }

        }

        return positive.stream().findAny().get();
    }


    public StringBuilder getProofBuilder() {
        return proofBuilder;
    }

    public void setProofBuilder(StringBuilder proofBuilder) {
        this.proofBuilder = proofBuilder;
    }

}
