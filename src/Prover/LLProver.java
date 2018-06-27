package Prover;

import gluePaP.glue.LexVariableHandler;
import gluePaP.linearLogic.*;
import gluePaP.semantics.*;

import java.util.*;

import static gluePaP.glue.LexVariableHandler.variableType.SemVar;
import static gluePaP.glue.LexVariableHandler.variableType.SemVarE;
import static gluePaP.semantics.SemAtom.SemSort.VAR;
import static gluePaP.semantics.SemType.AtomicType.T;

public class LLProver {
    // For Lev algorithm
    private Set<CategoryNode> catGraph = new HashSet<>();

    private LinkedList<Premise> skeletons = new LinkedList<>();
    private LinkedList<Premise> modifiers = new LinkedList<>();
    private LinkedList<Premise> agenda = new LinkedList<>();
    private LinkedList<Premise> database = new LinkedList<>();
    private List<Premise> skeletonTree = new ArrayList<>();
    private List<Premise> solutions = new ArrayList<>();
    private Sequent seq;
    //private Stack<SemAtom> assumptionVars = new Stack<>();
    private List<SemAtom> assumptionVars = new ArrayList<>();
    private int steps;


    public LLProver(Sequent seq) {
        this.seq = seq;
    }

    /**TODO Check if compilation works properly
     * Does a deduction of a given sequent by evaluating the list of premises on its LHS
     * and trying to find a valid proof for its RHS.
     * @return A list of all valid deductions
     * @throws ProverException If the proof is invalid
     * @throws VariableBindingException If an invalid variable binding is detected
     */
    public List<Premise> deduce() throws ProverException,VariableBindingException {
        /*
        Initialize an skeletons stack initially containing all premises from the sequent.
        Premises are popped from the stack into the database and additionally created
        premises get pushed onto the stack.
        Then initialize a database of all premises which is used to look for possible
        implication elimination steps.
        */
        skeletons.clear();
        modifiers.clear();
        database.clear();
        skeletonTree.clear();
        steps = 0;

        for (Premise p: seq.getLhs()) {

            /*
            * Check all premises for nested formulas. Alle nested formulas
            * (with two or more nested operators) are compiled following the algorithm
            * outlined by Hepple(1996). All extracted assumptions are added to the skeletons
            * as new premises with new IDs. Assumptions are premises that contain themselves
            * in their set of assumptions, but in the course of the derivation they may carry
            * additional assumptions (when they combine with other assumptions).
            * NOTE: due to the design of the conversion algorithm, a given term's discharge is always
            * contained in that term's set of assumptions. This shouldn't be a problem, however, as
            * terms with discharges are by design always formulas and can therefore not be arguments.
            * Their assumptions are thus not relevant in the derivation process.
            */

            try {
                Premise compiled = convert(p);
                if (!compiled.isModifier())
                    skeletons.add(compiled);
                else
                    modifiers.add(compiled);
                agenda.add(compiled);
                //createCategoryGraph();
            } catch (ProverException e) {
                e.printStackTrace();
            }
        }
        seq.getLhs().clear();
        seq.getLhs().addAll(skeletons);
        seq.getLhs().addAll(modifiers);
        System.out.println("Agenda: "+ seq.getLhs().toString());
        /*
        Initialize the set containing the IDs of all premises of the sequent.
        This set is used to determine possible goal terms.
        */
        HashSet<Integer> goalIDs = seq.getMaxIDSet();

        /*
        The algorithm loops over the skeletons until it is empty or until a premise is created
        that contains all indexes of the sequent's premises and is therefore the goal.
        */
        while (!(skeletons.size() == 0)) {
            Premise curr_premise = skeletons.pop();
            // add premise to database

            // Check if one or more modifier are applicable to the premise and apply them right away
            Iterator<Premise> it = modifiers.iterator();
            while (it.hasNext()) {
                Premise mod = it.next();
                if (((LLFormula) mod.getGlueTerm()).getLhs().checkEquivalence(curr_premise.getGlueTerm())) {
                    curr_premise = combinePremises(mod, curr_premise);
                    //curr_premise.setSemTerm(curr_premise.getSemTerm().betaReduce());
                    it.remove();
                }

            }

            for (Premise db_premise : database) {
                //if (db_premise == curr_premise)
                //   continue;
                steps++;
                /*
                Check if the database term is a (complex) formula, if so try to do an
                implication elimination step with the current term on the skeletons (curr_premise).
                If successful add the newly created Premise to the database.
                */
                if (db_premise.getGlueTerm() instanceof LLFormula) {

                    Premise new_premise = this.combinePremises(db_premise, curr_premise);
                    if (new_premise != null) {
                        new_premise.setHistory(db_premise, curr_premise);
                        System.out.println("Combining premises " + curr_premise + " and " + db_premise);
                        System.out.println("--> " + new_premise);
                        if (new_premise.getPremiseIDs().equals(goalIDs)) {
                            solutions.add(new_premise);
                        } else {
                            skeletons.push(new_premise);
                        }
                        continue;
                    }
                }
                /*
                Check if the current term on the skeletons is a (complex) formula. If so, do the same procedure
                as above, but reverse (apply db_premise to curr_premise).
                 */
                if (curr_premise.getGlueTerm() instanceof LLFormula) {
                    Premise new_premise = this.combinePremises(curr_premise, db_premise);
                    if (new_premise != null) {
                        new_premise.setHistory(curr_premise, db_premise);
                        System.out.println("Combining premises " + curr_premise + " and " + db_premise);
                        System.out.println("-->" + new_premise);

                        if (new_premise.getPremiseIDs().equals(goalIDs)) {
                            solutions.add(new_premise);
                        } else {
                            skeletons.push(new_premise);
                        }
                    }
                }
            }
            // After all combination checks are made, add the current premise to the database
            database.addFirst(curr_premise);

        }

        /*
        All premises of the skeletons were added to the database. If there are
        no possible solutions now, return a ProverException, otherwise return
        the set of solutions.
        */
        if (solutions.isEmpty())
            throw new ProverException("No valid proof found for premises");

        return solutions;
    }

    /**
     * Helper method for doing the modifier interpolations.
     * @param fullDerivation
     * @param m
     * @return
     */
    private Premise interpolateModifiers(Premise fullDerivation, LinkedList<Premise> m) {
        LinkedList<Premise> modifiers = new LinkedList<>(m);
        LinkedList<Premise> skelPremises = new LinkedList<>(m);
            Premise mod = modifiers.removeFirst();
        Premise p = fullDerivation;
        while (p.getFunc() != null) {
            Premise parentFunc = p.getFunc();
            Premise parentArg = p.getArg();
            if (((LLFormula) mod.getGlueTerm()).getLhs().checkEquivalence(parentFunc.getGlueTerm())) {
                //do interpolation
                // TODO what to do with all later derivations? Recursive helper function?
                // Combine modifier with respective skeletion premise and go back the derivation tree
                // changing all the premises accordingly.
                Premise newParent = combineDisjointID(mod, parentFunc);
                newParent.setHistory(parentFunc.getFunc(),parentFunc.getArg());
            }
            else
                skelPremises.push(p);
                p = parentFunc;
        }
        if (modifiers.isEmpty())
            return fullDerivation;

        return interpolateModifiers(fullDerivation, modifiers);

    }

    /*
    Trying to implement Lev's stuff
     */

    /**
     * Creates a category graph from the premises in the agenda. A category graph
     * contains each glue resource exactly once as a node, as well as combinations of two
     * resources as a connector node. Resources that can be combined link to the respective connector
     * node and the connector node links to the resource that is created by combining the two connected
     * resources.
     * Ex:
     *             connector
     *     left:A --> o  <-- formula:A -o B
     *                |
     *                v
     *          right:B
     * @throws ProverException
     */
    private void createCategoryGraph() throws ProverException{
        for (Premise p : agenda) {
            LLTerm category = p.getGlueTerm();
            if (category instanceof LLAtom)
                catGraph.add(new CategoryNode(category.toPlainString(),new HashSet<>()));
            else {
                while (category instanceof LLFormula) {
                    LLTerm leftNode = ((LLFormula) category).getLhs();
                    if (!(leftNode instanceof LLAtom)) {
                        throw new ProverException("Found uncompiled category while creating category graph");
                    }
                    CategoryNode formula = getNode(category.toPlainString());
                    CategoryNode left = getNode(leftNode.toPlainString());
                    CategoryNode right = getNode(((LLFormula) category).getRhs().toPlainString());
                    CategoryNode connector = getNode(leftNode.toPlainString() + "+"
                            +((LLFormula) category).getRhs().toPlainString());
                    if (formula == null)
                        formula = new CategoryNode(category.toPlainString(),new HashSet<>());
                    if (left == null)
                        left = new CategoryNode(leftNode.toPlainString(),new HashSet<>());
                    if (right == null)
                        right = new CategoryNode(((LLFormula) category).getRhs().toPlainString(),new HashSet<>());
                    if (connector == null) {
                        connector = new CategoryNode(leftNode.toPlainString() + "+"
                                + ((LLFormula) category).getRhs().toPlainString(), new HashSet<>());
                    }
                    formula.addLink(connector);
                    left.addLink(connector);
                    connector.addLink(right);

                    catGraph.add(formula);
                    catGraph.add(left);
                    catGraph.add(connector);
                    catGraph.add(right);

                    category = ((LLFormula) category).getRhs();
                }
            }
        }
    }

    private CategoryNode getNode(String nl) {
        for (CategoryNode node : catGraph) {
            if (nl.equals(node.toString()))
                return node;
        }
        return null;
    }


    private class CategoryNode {
        String nodeLabel;
        Set<CategoryNode> linksTo;

        CategoryNode(String l, Set<CategoryNode> links) {
            this.nodeLabel = l;
            this.linksTo = links;
        }

        private void addLink(CategoryNode c) {
            linksTo.add(c);
        }

        @Override
        public String toString() {
            return nodeLabel;
        }
    }


    /**
    Implementation of the linear implication elimination rule for indexed premises
    check if arg is equivalent to LHS of func and then return RHS of func
    then check if the sets of indexes are disjoint
    if both checks succeed a new Premise is created containing the unified set of indexes
    and the RHS LL term of func (see below)
     @param f the functor to be applied
     @param arg the argument that should match the antecedent of the functor
     @return the combined premise if successful, null if otherwise
    */
    private Premise combinePremises(Premise f, Premise arg) throws VariableBindingException, ProverException {


        // possible substitutions for variables and constants
        Premise func = new Premise(f.getPremiseIDs(),f.getSemTerm().clone(),f.getGlueTerm().clone());

        LinkedHashSet<Equality> eqs = ((LLFormula) func.getGlueTerm()).getLhs().checkCompatibility(arg.getGlueTerm());

        if (eqs == null) {return null;}

        if (eqs.size() > 0) {

            //If there are duplicate bindings no valid proof can be reached.
            if (LLProver.checkDuplicateBinding(eqs)) {
                throw new VariableBindingException();
            } else {
                //instantiates variables with constants (i.e. skolemizes the formula so it can take a constant)

                for (Equality eq : eqs) {
                    ((LLFormula) func.getGlueTerm()).instantiateVariables(eq);
                }
            }
        }

        Premise combined = null;

        // No assumptions or discharges involved, proceed with a "normal" implication elimination
        if (arg.getGlueTerm().assumptions.isEmpty()
                && arg.getGlueTerm().discharges.isEmpty()
                && func.getGlueTerm().assumptions.isEmpty()
                && func.getGlueTerm().discharges.isEmpty()) {
            return combineDisjointID(func, arg);
        }
        /*
        * Func or arg contain assumptions, but no discharges.
        * Combine the terms and their sets of assumptions
        * */
        else if ((!arg.getGlueTerm().assumptions.isEmpty()
                || !func.getGlueTerm().assumptions.isEmpty())
                && arg.getGlueTerm().discharges.isEmpty()
                && func.getGlueTerm().discharges.isEmpty()) {
            combined = combineDisjointID(func, arg);

            /* create new set of assumptions which can be modified independently from
            the set of assumptions of arg and func and add all assumptions to it */
            if (combined == null)
                throw new ProverException("Meaning side does not match structure of glue side");
            combined.getGlueTerm().assumptions = new HashSet<>();
            combined.getGlueTerm().assumptions.addAll(arg.getGlueTerm().assumptions);
            combined.getGlueTerm().assumptions.addAll(func.getGlueTerm().assumptions);

            return combined;
        }
        /*
        Functor has discharges, check if they are a subset of the argument's assumptions.
        If so call combineDisjointID which checks the ID sets of func and arg and then
        does the actual implication elimination step. For the new premise, all assumptions
        from arg are copied, except the one that was discharged in func.
        func: (b[a] -o c); arg: {a,(x -o y)} ==> c with assumption {(x -o y)}
        */
        else if (!func.getGlueTerm().discharges.isEmpty()) {
            if (arg.getGlueTerm().assumptions.containsAll(func.getGlueTerm().discharges))
            {

                combined = combineDisjointID(func, arg);
                /* create new set of assumptions which can be modified independently from
                the sets of assumptions of arg and func and add all assumptions to it*/
                if (combined == null)
                    throw new ProverException("Meaning side does not match structure of glue side");
                combined.getGlueTerm().assumptions = new HashSet<>();
                combined.getGlueTerm().assumptions.addAll(arg.getGlueTerm().assumptions);
                combined.getGlueTerm().assumptions.addAll(func.getGlueTerm().assumptions);
                combined.getGlueTerm().assumptions.removeAll(func.getGlueTerm().discharges);

                return combined;
            }
        }

        // The discharges are somehow incompatible, return null.
        return null;
    }


    /**
    * Check if the LHS of func is equivalent to arg
    * and if the two sets of indexes associated with them are disjoint.
    * If so return the simplified term (the RHS of func) with combined ID sets and
    * apply the meaning side of the argument to that of the functor and beta-reduce.
     * @param func the functor to be applied
     * @param arg the argument that the functor is applied to
     * @return the combined premise with the unified ID set
     *
    * */
    private Premise combineDisjointID(Premise func, Premise arg) {
        HashSet<Integer> combined_IDs = new HashSet<>();
        if (((LLFormula) func.getGlueTerm()).getLhs().checkEquivalence(arg.getGlueTerm())
                && Collections.disjoint(func.getPremiseIDs(),arg.getPremiseIDs())){
            combined_IDs.addAll(func.getPremiseIDs());
            combined_IDs.addAll(arg.getPremiseIDs());

            // Apply and beta-reduce meaning side
            //FuncApp applied = new FuncApp(new SemFunction((SemFunction) func.getSemTerm()),arg.getSemTerm());
            FuncApp applied = new FuncApp(func.getSemTerm(),arg.getSemTerm());
            SemRepresentation reducedSem = applied.betaReduce();
            //SemRepresentation reducedSem = applied;

            if (((LLFormula) func.getGlueTerm()).getRhs() instanceof  LLAtom) {
                return new Premise(combined_IDs, reducedSem,
                        new LLAtom((LLAtom) ((LLFormula) func.getGlueTerm()).getRhs()));
            }
            return new Premise(combined_IDs, reducedSem, ((LLFormula) func.getGlueTerm()).getRhs());
        }
        return null;
    }



    /**
     * converts premises by calling convertSemantics() and convertNested()
     * @param p The premise to be converted
     * @return The converted premise, where all complex antecedents are compiled out as separate
     *          premises. See Hepple(1996) for a descritption of the compilation process.
     *          Following Gupta&Lamping(1998) modifier formulas (of the form A -o A, where A
     *          may be atomic or complex) are not compiled.
     * @see #convertNested(Premise)
     * @throws ProverException
     */
    public Premise convert(Premise p) throws ProverException {
        if (p.getGlueTerm() instanceof LLFormula && !p.isModifier()) {
            LLFormula f = (LLFormula) p.getGlueTerm();


            if (f.getLhs() instanceof LLFormula /*
                    ((LLFormula) f.getLhs()).getOperator() instanceof LLImplication*/) {
                return convertNested(p);
            }
            else {
                // the term is of the form (A -o B), where A is an atomic formula
                // no conversion step needed on the glue side, but lambda abstraction on
                // the meaning side is necessary. But only if there has been no conversion?
                // TODO suspended semantic conversion for now...
                // ...because it has no use and adds unnecessary complexity to the variable assignments
                //
                //p.setSemTerm(this.convertSemantics(p.getSemTerm()));
                return p;
            }
        }
        return p;
    }

    /*
    * Recursively converts a semantic term by replacing the variables with newly created ones
    * */
    private SemRepresentation convertSemantics(SemRepresentation sem) {
        if (sem instanceof SemFunction) {
            // create new variable with the type of the binder of the inner function
            SemAtom var = new SemAtom(VAR,LexVariableHandler.returnNewVar(SemVar)
                    ,((SemFunction) sem).getBinder().getType());
            // apply var
            SemRepresentation compiled = new FuncApp(sem,var).betaReduce();
            SemRepresentation inner = convertSemantics(compiled);
            // return new function with the applied variable as binder
            return new SemFunction(var,inner);
        }
        return sem;
    }

    /**
    * This method does the actual compilation process. It adds created "assumptions" as additional
     * premises to the skeletons and returns the compilated premise. The algorithm is based on Hepple(1996)
     * and works as follows:
    * The LHS of the LHS of f will become an assumption which in turn gets converted as well.
    * The assumption gets converted as well and is marked as an assumption
    * by adding itself to its set of assumptions. That is, an LLTerm "a" is an assumption
    * iff its set of assumptions contains "a". This way of marking assumptions allows easy
    * combination with other assumptions and LLTerms with discharges.
    * All extracted assumptions are stored in a HashSet in dependency.
    * Ex. if f = ((a -o b) -o c) then dependency = (b -o c) and assumption = {a}
    * Dependency is a new formula consisting of the rest of f, that is, the RHS of the LHS of f
    * and the RHS of f.
    *
    * On the semantic side this amounts to creating a new variable for the assumption and a lambda
    * term that binds the new variable of the assumption.
    * Ex. LP.Ex[person(x) & P(x)] : ((g -o X) -o X) is converted to
    *    Lu.LP.Ex[person(x) & P(x)](Lv.u) : (X[g] -o X) and v:{g}
    * NOTE:
    * In cases where a formula like ((((a -o b) -o c) -o d) -o e) is compiled
    * we want to derive the assumptions {a} and {(b -o c)} and the dependency
    * (d -o e)[(b -o c)[a]], that is, a discharge with a nested discharge.
    * However, for a formula like (((a -o (b -o c)) -o d)
    * we want to get {a} and {b} and the dependency (c -o d)[a,b], that is,
    * with a list of (atomic) discharges. Otherwise we would run into a dead end
    * while combining the extracted premises.
    * This is achieved by only adding the single assumption that is currently
    * being extracted to the dependency's discharges.
     * @param p The nested premise to be converted
     * @return The converted premise
    * */
    private Premise convertNested(Premise p) throws ProverException {
        if (p.getGlueTerm() instanceof LLFormula) {
            LLFormula f = (LLFormula) p.getGlueTerm();

            // nested formula; extract assumption and build new term
            if (f.getLhs() instanceof LLFormula) {

                if (!(p.getSemTerm() instanceof SemFunction))
                    throw new ProverException("Meaning side does not match structure of glue side");


                SemAtom assumpVar = new SemAtom(VAR, LexVariableHandler.returnNewVar(SemVarE),
                        ((SemFunction) p.getSemTerm()).getBinder().getType().getLeft());
                assumptionVars.add(assumpVar);

                Premise assumption = convertNested(new Premise(seq.getNewID(), ((LLFormula) f.getLhs()).getLhs()));
                assumption.getGlueTerm().assumptions.add(assumption.getGlueTerm());
                skeletons.add(assumption);
                Premise dependency = new Premise(p.getPremiseIDs(), p.getSemTerm(), new LLFormula(((LLFormula) f.getLhs()).getRhs(),
                        f.getOperator(), f.getRhs(), f.isPolarity(), f.getVariable()));
                dependency.getGlueTerm().discharges.add(assumption.getGlueTerm());
                dependency = convertNested(dependency);
                /*
                If reordering is required (see function below), reorder the dependency, by
                temporarily removing the newly created lambda term on the meaning side
                and reapplying it after the reordering process.

                if (((LLFormula) dependency.getGlueTerm()).getRhs() instanceof LLFormula) {
                    SemRepresentation inner = ((SemFunction) dependency.getSemTerm()).getFuncBody();
                    Premise reordered = reorder(new Premise(p.getPremiseIDs(),inner,dependency.getGlueTerm()));
                    dependency = new Premise(p.getPremiseIDs(),
                            new SemFunction(((SemFunction) dependency.getSemTerm()).getBinder(),reordered.getSemTerm()),reordered.getGlueTerm());
                }
                */
                return dependency;
            }
            // There might be cases like a -o ((b -o c) -o d) where reordering is necessary before
            // the term can be compiled
            else if (f.getRhs() instanceof  LLFormula &&
                    ((LLFormula) f.getRhs()).getLhs() instanceof LLFormula) {
                p = convertNested(reorder(p));
            }
            /*
            simple implication; create new lambda function which binds var (the variable
            associated with the assumption created in this method). Then add this new lambda
            term as argument to the current meaning term and wrap everything in a new lambda
            term binding the newly created variable.
            */
            SemAtom binderVar = new SemAtom(VAR,LexVariableHandler.returnNewVar(SemVar),T);
            SemFunction newArg = new SemFunction(assumptionVars.remove(0),binderVar);
            //((SemFunction) p.getSemTerm()).setArgument(newArg);
            p.setSemTerm(new SemFunction(binderVar,new FuncApp(p.getSemTerm(),newArg)));
            return p;
        }
        // only an atomic glue term which will become an assumption;
        // return it and add the new variable as meaning side
        else {
            p.setSemTerm(assumptionVars.get(assumptionVars.size()-1));
            return p;
        }
    }

    /**
    * A helper method for restructuring the glue side of a premise so that it has the form A -o (b -o C)
    * (where lower case letters denote atoms and upper case letters complex
    * formulas). The full term consists only of subformulas of this form where A and C are at most binary formulas
    * This is the form that is compilable by our algorithm. Glue terms are swapped to obtain
    * the desired form. All swapping operations on the glue side amount to swapping operations of lambda binders
    * on the meaning side to keep the two sides aligned.
    * @param p The premise to be reordered
     * @return The reordered premise
     * @throws ProverException
    * */
    private Premise reorder(Premise p) throws ProverException{
        // Glue term has the form A -o (B -o C)
        if (((LLFormula) p.getGlueTerm()).getRhs() instanceof LLFormula) {

            // Glue term has the form A -o ((B -o D) -o C) check if A and B can be swapped, then
            // recursively call the function on the RHS of the (potentially swapped) formula
            if (((LLFormula)((LLFormula) p.getGlueTerm()).getRhs()).getLhs() instanceof LLFormula) {
                SemFunction sem = (SemFunction) p.getSemTerm();
                LLFormula glue = (LLFormula) p.getGlueTerm();

                // Glue term has the form a -o ((B -o D) -o C); swap a and (B -o D)
                if (((LLFormula) p.getGlueTerm()).getLhs() instanceof LLAtom) {
                    LLTerm oldLeft = ((LLFormula) p.getGlueTerm()).getLhs();
                    LLTerm oldInnerLeft = ((LLFormula) ((LLFormula) p.getGlueTerm()).getRhs()).getLhs();
                    LLTerm oldInnerRight = ((LLFormula) ((LLFormula) p.getGlueTerm()).getRhs()).getRhs();
                    if (!(p.getSemTerm() instanceof SemFunction))
                        throw new ProverException("Semantic term does not match structure of glue side.");
                    else
                        sem = swapLambdas(sem);
                    LLFormula newinner = new LLFormula(oldLeft, oldInnerRight, oldInnerRight.isPolarity(),((LLFormula) ((LLFormula) p.getGlueTerm()).getRhs()).getVariable());
                    newinner.discharges.addAll(glue.discharges);
                    glue = new LLFormula(oldInnerLeft,newinner, p.getGlueTerm().isPolarity(), newinner.getVariable());
                }
                Premise inner = reorder(new Premise(p.getPremiseIDs(),sem.getFuncBody(),glue.getRhs()));
                glue = new LLFormula(glue.getLhs(),inner.getGlueTerm(),glue.isPolarity(),glue.getVariable());
                sem = new SemFunction(sem.getBinder(),inner.getSemTerm());

                return new Premise(p.getPremiseIDs(),sem,glue);
            }
        }
        // other cases? A -o b; no reordering required
        // TODO are these all cases?
        return p;
    }

    /**
     * A helper method for the reorder() method
     * @see #reorder(Premise)
     * @param outer the semantic function to be swapped
     * @return the swapped semantic function
     * @throws ProverException
     */
    private SemFunction swapLambdas(SemFunction outer) throws ProverException{
        if (outer.getFuncBody() instanceof SemFunction) {
            SemFunction inner = (SemFunction) outer.getFuncBody();
            return new SemFunction(inner.getBinder(),new SemFunction(outer.getBinder(),inner.getFuncBody()));
        }
        else
            throw new ProverException("Semantic term does not match structure of glue side.");
    }

    /**
     * Checks for duplicate bindings and returns false if a variable is assigned more than one value
     * @param in
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


}
