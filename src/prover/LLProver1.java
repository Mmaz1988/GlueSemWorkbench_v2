package prover;


import glueSemantics.linearLogic.*;
import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.*;
import main.InputOutputProcessor;
import main.Settings;
import org.jgrapht.Graph;
import org.jgrapht.alg.connectivity.GabowStrongConnectivityInspector;
import org.jgrapht.alg.interfaces.StrongConnectivityAlgorithm;
import org.jgrapht.graph.DefaultDirectedGraph;
import org.jgrapht.graph.DefaultEdge;
import org.jgrapht.traverse.TopologicalOrderIterator;
import prover.categoryGraph.CGNode;
import prover.categoryGraph.History;
import utilities.Debugging;
import utilities.LexVariableHandler;

import java.io.IOException;
import java.util.*;
import java.util.stream.Collectors;


public class LLProver1 extends LLProver {



    private Sequent currentSequent;
    private LinkedList<History> finalHistories = new LinkedList<>();
    private StringBuilder proofBuilder;
    private HashSet<Integer> goalIDs = new HashSet<>();
    public long startTime;
    public GraphAnalysis analysis;

    /**
     * LLProver1 implements a procedure for Glue semantics derivations based on Lev (2007), chapter 6
     * It uses a cartography graph to order combination steps handling skeleton premises and a chart-based
     * algorithm to deal with modifier premises, i.e. cyclic elements in a glue proof.
     *
     * @param settings
     */
    public LLProver1(Settings settings) {
        setSettings(settings);
        this.proofBuilder = new StringBuilder();
    }

    public LLProver1(Settings settings, StringBuilder proofBuilder) {
        this.proofBuilder = proofBuilder;
        setSettings(settings);
    }

    public void deduce(Sequent seq) throws ProverException, VariableBindingException {

        this.db = new Debugging();
        this.currentSequent = seq;
        LinkedList<Premise> agenda = new LinkedList<>();

        startTime = System.nanoTime();


        StringBuilder sb = new StringBuilder();
        sb.append("Input premises:");
        sb.append(System.lineSeparator());
        for (Premise le : currentSequent.getLhs()) {
            sb.append(le);
            sb.append(System.lineSeparator());
        }

        String inputPremises = currentSequent.getLhs().stream().map(Objects::toString).collect(Collectors.joining(", "));

        getLOGGER().fine("List of current premises: " + inputPremises);

        //TODO insert boolean for distinguishing between sdout and file
        if (true) {
            proofBuilder.append(sb.toString());
            proofBuilder.append(System.lineSeparator());
            proofBuilder.append(System.lineSeparator());
        }

        getLOGGER().fine("Starting compilation process...");
        for (Premise p : currentSequent.getLhs()) {
            List<Premise> compiled = convert(p);
            agenda.addAll(compiled);
        }

        String goalCategory = findAtomicGoal(agenda);
        getLOGGER().fine("Automatically detected goal category: " + goalCategory);


        getLOGGER().fine("Starting deduction procedure...");
        StringBuilder ab = new StringBuilder();
        ab.append("Agenda:");
        ab.append(System.lineSeparator());
        for (Premise p : agenda) {
            ab.append(p);
            ab.append(System.lineSeparator());
        }


        //TODO insert boolean for distinguishing between sdout and file
        if (true) {
            proofBuilder.append(ab.toString());
            proofBuilder.append(System.lineSeparator());
            proofBuilder.append(System.lineSeparator());
        }

        List<LLTerm> initialCategories = new ArrayList<>();
        HashMap<String,List<Premise>> category2premiseMapping = new HashMap<>();

        for (Premise p : agenda) {
            if (p.getPremiseIDs().size() == 1) {
                goalIDs.addAll(p.getPremiseIDs());
            }

            if (!category2premiseMapping.containsKey(p.getGlueTerm().category().toString()))
            {
              category2premiseMapping.put(p.getGlueTerm().category().toString(), new ArrayList<>());
            }
            category2premiseMapping.get(p.getGlueTerm().category().toString()).add(p);

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

        Graph<CGNode, DefaultEdge> categoryGraph2 = calculateCategoryGraph2(copy,category2premiseMapping);

        StrongConnectivityAlgorithm ins2 = new GabowStrongConnectivityInspector(categoryGraph2);
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
                           node.compressHistories();

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
                        CGNode parentNode = categoryGraph2.getEdgeSource((DefaultEdge) edge);
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
                    node.compressHistories();
                }
            }
            else
            {
            	//Inside an scc

                List<CGNode> outputNodes = new ArrayList<>();
                List<CGNode> inputNodes = new ArrayList<>();
                Set<History> sccHistories = new HashSet<>();

                for (Object node : sccKey.vertexSet())
                {
                    inputNodes.add((CGNode) node);
                    sccHistories.addAll(((CGNode) node).histories);

                    if (!(categoryGraph2.incomingEdgesOf((CGNode) node).size() == 0))
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
                        }
                    }
                    if (categoryGraph2.outgoingEdgesOf((CGNode) node).size() > 0)
                    {
                        for (Object edge : categoryGraph2.outgoingEdgesOf((CGNode) node))
                        {
                            if (!(sccKey.vertexSet().contains(categoryGraph2.getEdgeTarget((DefaultEdge) edge)))
                            && categoryGraph2.getEdgeTarget((DefaultEdge) edge).nodeType.equals(CGNode.type.CONNECTOR) )
                            {
                                outputNodes.add((CGNode) node);
                                break;
                            }
                        }
                        }
                    else {
                        if (((CGNode) node).category.equals(goalCategory))
                        {
                            outputNodes.add((CGNode) node);
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
                    outputNode.compressHistories();
                }
            }
        }
        
        analysis = new GraphAnalysis(goalCategory,scc2);
        //analysis.displayGraph();     
        
        getLOGGER().fine("Starting semantic calculations...");

        StringBuilder resultBuilder = new StringBuilder();

        if (!finalHistories.isEmpty()) {
            for (History solution : finalHistories) {
                getSolutions().addAll(solution.calculateSolutions(resultBuilder));
            }
            proofBuilder.append(resultBuilder.toString());

            getLOGGER().info("Found the following glue derivation(s):\n" + resultBuilder.toString());
        

        
        }



       // System.out.println(resultBuilder);
        // System.out.println(System.lineSeparator());

        long endTime = System.nanoTime();
        db.computationTime = endTime - startTime;
        proofBuilder.append(System.lineSeparator());
    }

    public Chart chartDeduce(List<History> histories) throws VariableBindingException, ProverException {
        getLOGGER().finer("Beginning a partial chart derivation...");
        Chart chart = new Chart();

        List<History> agenda = new ArrayList<>(histories);

        while (!agenda.isEmpty()) {
            ListIterator<History> agendaIterator = agenda.listIterator();

            while (agendaIterator.hasNext()) {
                History current = agendaIterator.next();
                agendaIterator.remove();

                if (!current.category.atomic) {

                    if (chart.atomicChart.containsKey(current.category.left.toString()))
                    {
                        for (History h : chart.atomicChart.get(current.category.left.toString())) {
                            History combined = combineHistories(current,h);
                            if (combined != null) {
                                agendaIterator.add(combined);
                            }
                        }
                    }
                    if (chart.nonAtomicChart.containsKey(current.category.left.toString()))
                    {
                        chart.nonAtomicChart.get(current.category.left.toString()).add(current);
                    } else
                    {
                        Set<History> histories1 = new HashSet<>();
                        histories1.add(current);
                        chart.nonAtomicChart.put(current.category.left.toString(),histories1);
                    }

                } else {
                    if (chart.nonAtomicChart.containsKey(current.category.toString()))
                    {
                        for (History h : chart.nonAtomicChart.get(current.category.toString()))
                        {
                            History combined = combineHistories(h,current);
                            if (combined != null) {
                                agendaIterator.add(combined);
                            }
                        }
                    }

                    if (chart.atomicChart.containsKey(current.category.toString()))
                    {
                        chart.atomicChart.get(current.category.toString()).add(current);
                    } else
                    {
                        Set<History> histories1 = new HashSet<>();
                        histories1.add(current);
                        chart.atomicChart.put(current.category.toString(),histories1);
                    }
                }
            }
        }
        return chart;
    }


    public Graph<CGNode,DefaultEdge> calculateCategoryGraph2(List<LLTerm> initialPremises,HashMap<String,List<Premise>> category2premiseMapping)
    {
        Graph<CGNode,DefaultEdge> categoryGraph = new DefaultDirectedGraph<CGNode,DefaultEdge>(DefaultEdge.class);
        HashMap<String,CGNode> category2node = new HashMap<>();
        Set<Category> categories = new HashSet<>();
        for (LLTerm t :initialPremises)
        {
            categories.addAll(t.returnAllCategories());
        }
        List<Category> cgnList = new ArrayList<>(categories);

        cgnList.sort((s1,s2) -> s1.toString().length() - s2.toString().length());

        for (Category category : cgnList)
        {
            CGNode currentNode = new CGNode(category.toString(), CGNode.type.CATEGORY,this);

            if (category2premiseMapping.containsKey(category.toString())) {

                for (Premise p : category2premiseMapping.get(category.toString())) {

                    History h = new History(p.getGlueTerm().category(), p.getPremiseIDs(), Collections.singleton(new HashMap<>()),p,this);

                    if (p.getGlueTerm() instanceof LLFormula) {
                        h.discharges.addAll(((LLFormula) p.getGlueTerm()).getLhs().orderedDischarges.keySet());
                        h.requirements.addAll(((LLFormula) p.getGlueTerm()).getRhs().category().dischargeRequirements());
                    }
                    currentNode.histories.add(h);
                }
            }
            categoryGraph.addVertex(currentNode);
            category2node.put(category.toString(),currentNode);
        }

             ListIterator<Category> iter= cgnList.listIterator();

             while (iter.hasNext())
             {
             Category category = iter.next();
             iter.remove();

                ListIterator<Category> iter2 = cgnList.listIterator();
                while (iter2.hasNext()) {
                    Category compareCategory = iter2.next();
                    if (!compareCategory.atomic) {
                        if (compareCategory.left.equals(category)) {
                            CGNode func = category2node.get(category.toString());
                            String prefix = func + "\u22B8";
                            CGNode arg = category2node.get(compareCategory.toString());
                            CGNode result = category2node.get(compareCategory.right.toString());
                            CGNode connector =
                                    new CGNode(LexVariableHandler.
                                            returnNewVar(LexVariableHandler.variableType.connectorNode),
                                            CGNode.type.CONNECTOR,this);
                            categoryGraph.addVertex(connector);
                            categoryGraph.addEdge(func, connector);
                            categoryGraph.addEdge(arg, connector);
                            categoryGraph.addEdge(connector, result);
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
                    /*
                    Premise p = combinePremises(h1.premise, h2.premise);
                    if (p == null)
                    {
                        return null;
                    }
                     */
                    Set<Integer> union = new HashSet<>();
                    union.addAll(h1.indexSet);
                    union.addAll(h2.indexSet);
                    HashMap<Integer, History> parentNodes = new HashMap<>();
                    parentNodes.put(0, h1);
                    parentNodes.put(1, h2);

                    Set<HashMap<Integer,History>> parents = Collections.singleton(parentNodes);

                    History result = new History( h1.category.right, union, parents,this);

                    if (!h1.category.right.atomic) {
                        result.discharges = h1.category.right.left.discharges;
                        result.requirements = h1.category.right.right.dischargeRequirements();
                    }

                    getLOGGER().finer("Now combining " + h1.category.toString() +
                            " and " + h2.category.toString() +
                            " with result: " + result.category);


                    if (result.indexSet.equals(goalIDs))
                    {
                        finalHistories.add(result);
                    }
                    return result;
                }
            }
        }
        return null;
    }

    @Override
    public Premise combinePremises(Premise functor, Premise argument, StringBuilder proofBuilder) throws VariableBindingException, ProverException {

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
            if (LLProver1.checkDuplicateBinding(eqs)) {
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

                SemanticRepresentation reducedSem = null;

                try {
                reducedSem = combine(func, argumentClone).betaReduce();
                } catch(Exception e)
                {
                    getLOGGER().warning("Failed to combine functor: " + func.toString() + " and argument: " +
                            argumentClone.toString());
                    return null;
                }

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

                    SemanticRepresentation reducedSem = null;
                    try {
                        reducedSem = combine(func, argumentClone).betaReduce();
                    } catch(Exception e)
                    {
                        getLOGGER().warning("Failed to combine functor: " + func.toString() + " and argument: " +
                                argumentClone.toString());
                        return null;
                    }

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

            /*
           System.out.println("Combining " + f + " and " + a);
            System.out.println("to: " + combined.toString());
*/
            //TODO sdout vs file
            if (true)
            {
                proofBuilder.append("Combining " + InputOutputProcessor.restoreBackLinearLogicSide(f) + " and " + InputOutputProcessor.restoreBackLinearLogicSide(a));
                proofBuilder.append(System.lineSeparator());
                proofBuilder.append("to: " + InputOutputProcessor.restoreBackLinearLogicSide(combined.toString()));
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

    public static Boolean checkDischarges(Premise functor, Premise argument) {

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
                    getLOGGER().finer("Semantic side inherits type from linear logic side.");
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
        compiled.addFirst(p);
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
                positive.add(atom.category().toString());
            } else
            {
                negative.add(atom.category().toString());
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



    //Getter and Setter
    public LinkedList<History> getFinalHistories() {
        return finalHistories;
    }

    public void setFinalHistories(LinkedList<History> finalHistories) {
        this.finalHistories = finalHistories;
    }



    public StringBuilder getProofBuilder() {
        return proofBuilder;
    }

    public void setProofBuilder(StringBuilder proofBuilder) {
        this.proofBuilder = proofBuilder;
    }



}
