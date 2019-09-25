/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package prover;

import glueSemantics.semantics.MeaningRepresentation;
import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.*;
import glueSemantics.synInterface.dependency.LexVariableHandler;
import glueSemantics.linearLogic.*;
import main.Settings;
import test.Debugging;

import java.util.*;

import static glueSemantics.semantics.lambda.SemType.AtomicType.TEMP;
import static glueSemantics.synInterface.dependency.LexVariableHandler.variableType.SemVar;
import static glueSemantics.synInterface.dependency.LexVariableHandler.variableType.SemVarE;
import static glueSemantics.semantics.lambda.SemAtom.SemSort.VAR;
import static glueSemantics.semantics.lambda.SemType.AtomicType.T;

public class LLProver {
    public static Settings getSettings() {
        return settings;
    }

    public static void setSettings(Settings settings) {
        LLProver.settings = settings;
    }

    private static Settings settings;

    private SemanticRepresentation currentSemantics;
    private SemanticRepresentation dummySemantics;

    private LinkedList<Premise> skeletons;
    private LinkedList<Premise> modifiers;
    private LinkedList<Premise> agenda;
    private LinkedList<Premise> database;
    private LinkedList<Premise> solutions;
    /*
    Initialize the set containing the IDs of all premises of the sequent.
    This set is used to determine possible goal terms.
    */
    private HashSet<Integer> goalIDs;
    private Sequent currSeq;
   // private LinkedList<SemAtom> assumptionVars = new LinkedList<>();

    private LinkedHashMap<Integer,LinkedList<SemAtom>> assumptionVars = new LinkedHashMap<>();

   // private LinkedList<SemAtom> binderVars = new LinkedList<>();

    private LinkedHashMap<Integer,LinkedList<SemAtom>> binderVars = new LinkedHashMap<>();


    private int counter = 0;
    private int argcounter = 0;

    public Debugging db = new Debugging();

    public LLProver(Settings settings) {
        setSettings(settings);
        this.skeletons = new LinkedList<>();
        this.modifiers = new LinkedList<>();
        this.agenda = new LinkedList<>();
        this.database = new LinkedList<>();
        this.solutions = new LinkedList<>();
    }

    /**
     * Does a deduction of a given sequent by evaluating the list of premises on its LHS
     * and trying to find a valid proof for its RHS.
     * @return A list of all valid deductions
     * @throws ProverException If the proof is invalid
     * @throws VariableBindingException If an invalid variable binding is detected
     */
    public List<Premise> deduce(Sequent seq) throws ProverException,VariableBindingException {
        this.currSeq = seq;
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
        solutions.clear();

        long startTime = System.nanoTime();

        for (Premise p: currSeq.getLhs()) {

            /*
            * Check all premises for nested formulas. Alle nested formulas
            * (with two or more nested operators) are compiled following the algorithm
            * outlined by Hepple(1996). All extracted assumptions are added to the skeletons
            * as new premises with new IDs. Assumptions are premises that contain themselves
            * in their set of assumptions, but in the course of the derivation they may carry
            * additional assumptions (when they combine with other assumptions).
            */

            try {
                Premise compiled = convert(p);
                if (!compiled.isModifier())
                    skeletons.add(compiled);
                else
                    modifiers.add(compiled);
                agenda.add(compiled);
            } catch (ProverException e) {
                e.printStackTrace();
            }
        }
        currSeq.getLhs().clear();
        currSeq.getLhs().addAll(skeletons);
        currSeq.getLhs().addAll(modifiers);
        goalIDs = currSeq.getMaxIDSet();
        //System.out.println("Agenda: "+ currSeq.getLhs().toString());

        StringBuilder sb = new StringBuilder();
        sb.append("Agenda:");
        sb.append(System.lineSeparator());
        for (Premise p : currSeq.getLhs())
        {
            sb.append(p);
            sb.append(System.lineSeparator());
        }
        System.out.println(sb.toString());

        /*
        The algorithm loops over the skeletons until it is empty or until a premise is created
        that contains all indexes of the sequent's premises and is therefore the goal.
        */
        while (!skeletons.isEmpty() && solutions.isEmpty()) {
            while (!skeletons.isEmpty()) {
                Premise currentPremise = skeletons.pop();

                // Check all database entries for possible combinations
                checkDatabase(currentPremise);

                // Check if one or more modifier are applicable to the premise and apply them right away
/*            Iterator<Premise> it = modifiers.iterator();
            while (it.hasNext()) {
                Premise mod = it.next();
                if (((LLFormula) mod.getGlueTerm()).getLhs().checkEquivalence(currentPremise.getGlueTerm())) {
                    currentPremise = combinePremises(mod, currentPremise);
                    // Remove modifier from modifier list.
                    it.remove();
                }
            }*/

                // After all combination checks are made, add the current premise to the database
                database.addFirst(currentPremise);
            }
            ListIterator<Premise> modIterator = modifiers.listIterator();
            // TODO other stop condition for the loop: ID set is full for one premise?
            /*while(modIterator.hasNext()) {
                if (checkDatabase(modIterator.next()))
                    modIterator.remove();
            }*/
            for (Premise modifier : modifiers) {
                checkDatabase(modifier);
            }
        }



        /*
        All premises of the skeletons were added to the database. If there are
        no possible solutions now, return a ProverException, otherwise return
        the set of solutions.
        */
        if (solutions.isEmpty()) {
            //throw new ProverException("No valid proof found for premises");
            System.out.println("Found no valid full derivation, only partial derivations were found.");
        }

        long endTime = System.nanoTime();

        db.computationTime = endTime - startTime;

        return solutions;
    }


    /**
     * Checks the database for possible combinations with currentPremise, both as functor and as argument.
     * If modified is set to true (e.g. after all skeletons have been added to the database), this method
     * always adds new premises to the modifiers list, so because that list contains all modified premises.
     * @param currentPremise
     * @throws VariableBindingException
     * @throws ProverException
     */
    private boolean checkDatabase(Premise currentPremise) throws VariableBindingException, ProverException {
        boolean hasCombined = false;



        for (Premise dbPremise : database) {
            db.allIterations++;
            if (dbPremise == currentPremise)
                continue;
                /*
                Check if the database term is a (complex) formula, if so try to do an
                implication elimination step with the current term on the skeletons (currentPremise).
                If successful add the newly created Premise to the database.
                */
            if (dbPremise.getGlueTerm() instanceof LLFormula) {

                Premise newPremise = this.combinePremises(dbPremise, currentPremise);
                if (newPremise != null) {
                    hasCombined = true;
                    newPremise.setHistory(dbPremise, currentPremise);
                    System.out.println("Combining premises " + currentPremise + " and " + dbPremise);
                    System.out.println("--> " + newPremise);
                    if (newPremise.getPremiseIDs().equals(goalIDs)) {
                        solutions.add(newPremise);
                    }
                    if (newPremise.isModifier()) {
                        /*if (modifiers.contains(newPremise)|| solutions.contains(newPremise))
                            modifiers.remove(currentPremise);
                        else*/
                            modifiers.add(newPremise);
                    }
                    else
                        skeletons.push(newPremise);
                    continue;
                }
            }
                /*
                Check if the current term on the skeletons list is a (complex) formula. If so, do the same procedure
                as above, but reverse (apply dbPremise to currentPremise).
                 */
            if (currentPremise.getGlueTerm() instanceof LLFormula) {
                Premise newPremise = this.combinePremises(currentPremise, dbPremise);
                if (newPremise != null) {
                    hasCombined = true;
                    newPremise.setHistory(currentPremise, dbPremise);
                    System.out.println("Combining premises " + currentPremise + " and " + dbPremise);
                    System.out.println("-->" + newPremise);

                    if (newPremise.getPremiseIDs().equals(goalIDs)) {
                        solutions.add(newPremise);
                    }
                    if (newPremise.isModifier()) {
                        /*if (modifiers.contains(newPremise)|| solutions.contains(newPremise))
                            modifiers.remove(currentPremise);
                        else*/
                            modifiers.add(newPremise);
                    }
                    else
                        skeletons.push(newPremise);
                }
            }
        }





        return hasCombined;
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

            db.combinations++;
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

                db.combinations++;
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
    private Premise combineDisjointID(Premise func, Premise arg) throws ProverException {
        HashSet<Integer> combined_IDs = new HashSet<>();
        if (((LLFormula) func.getGlueTerm()).getLhs().checkEquivalence(arg.getGlueTerm())
                && Collections.disjoint(func.getPremiseIDs(),arg.getPremiseIDs())){
            combined_IDs.addAll(func.getPremiseIDs());
            combined_IDs.addAll(arg.getPremiseIDs());

            // Apply and beta-reduce meaning side
            //FuncApp applied = new FuncApp(func.getSemTerm(),arg.getSemTerm());
            SemanticRepresentation reducedSem;
            if (getSettings().isBetaReduce())
            {
                System.out.println("Beta reduced: " + func.getSemTerm().toString() +", " +arg.getSemTerm().toString());
                reducedSem = new FuncApp(func.getSemTerm(),arg.getSemTerm()).betaReduce();
                System.out.println("To:" + reducedSem.toString());
            }
            else
                reducedSem = new FuncApp(func.getSemTerm(),arg.getSemTerm());

            if (((LLFormula) func.getGlueTerm()).getRhs() instanceof  LLAtom) {
                return new Premise(combined_IDs, reducedSem,
                        new LLAtom((LLAtom) ((LLFormula) func.getGlueTerm()).getRhs()));
            }
            return new Premise(combined_IDs, reducedSem, ((LLFormula) func.getGlueTerm()).getRhs());
        }
        return null;
    }


    private void interpolateModifier() throws ProverException {

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


            if (f.getLhs() instanceof LLFormula) {
                currentSemantics = p.getSemTerm().clone();
                dummySemantics = new MeaningRepresentation("dum_var");

                Premise compiled = convertNested(p);
              compiled.setSemTerm((SemanticExpression) replaceDummy(compiled.getSemTerm()));

                dummySemantics = currentSemantics;
                binderVars = new LinkedHashMap<>();
                assumptionVars = new LinkedHashMap<>();





                return convertNested(compiled);
            }
            else {
                return p;
            }
        }
        return p;
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
        db.compilations++;



        if (p.getGlueTerm() instanceof LLFormula) {
            LLFormula f = (LLFormula) p.getGlueTerm();

            // nested formula; extract assumption and build new term
            if (f.getLhs() instanceof LLFormula) {

                if (!(p.getSemTerm() instanceof SemFunction || p.getSemTerm() instanceof MeaningRepresentation))
                    throw new ProverException("Meaning side does not match structure of glue side");


                SemType newtype = new SemType(((LLFormula) f.getLhs()).getRhs().getType());

                Boolean complex = false;

                if (((LLFormula) f.getLhs()).getRhs().getType().getSimple() == null)
                {
                    complex = true;
                }
                //Old version

                SemAtom binderVar = new SemAtom(VAR,LexVariableHandler.returnNewVar(SemVar),newtype);

                if (binderVars.keySet().contains(argcounter))
                {
                    binderVars.get(argcounter).addLast(binderVar);
                } else
                {
                    binderVars.put(argcounter,new LinkedList<>(Arrays.asList(binderVar)));
                }

                //This is the variable that gets cut off the main formula
                SemAtom assumpVar = new SemAtom(VAR, LexVariableHandler.returnNewVar(SemVarE),new SemType(((LLFormula) f.getLhs()).getLhs().getType()));

                if (assumptionVars.keySet().contains(argcounter))
                {
                    assumptionVars.get(argcounter).addLast(assumpVar);
                } else
                {
                    assumptionVars.put(argcounter,new LinkedList<>(Arrays.asList(assumpVar)));
                }

                counter = counter + 1;


                //New version
                //SemAtom assumpVar = new SemAtom(VAR, LexVariableHandler.returnNewVar(SemVarE),new SemType(TEMP));
                //assumptionVars.addLast(assumpVar);


               Premise assumption = convertNested(new Premise(currSeq.getNewID(), ((LLFormula) f.getLhs()).getLhs()));
              //  Premise assumption = convertNested(new Premise(currSeq.getNewID(), returnLeftMostFormula(f)));
                assumption.getGlueTerm().assumptions.add(assumption.getGlueTerm());
                // TODO add distinction between skel and mod here?
                skeletons.add(assumption);

                //((SemFunction) p.getSemTerm()).setArgument(newArg);


                //Original version:

                Premise dependency = new Premise(p.getPremiseIDs(), p.getSemTerm(), new LLFormula(((LLFormula) f.getLhs()).getRhs(),
                        f.getRhs(), f.isPolarity(), f.getVariable()));


                dependency.getGlueTerm().setDischarges(p.getGlueTerm().getDischarges());
                dependency.getGlueTerm().discharges.add(assumption.getGlueTerm());


                //Reiterate to collect all necessary variables
                dependency = convertNested(dependency);


                //This guarantees that the first argument of the compiled formula turns out to be the first argument of the
                //original semantics

             //   SemAtom argbv = binderVars.get(argcounter).get(counter);

                Integer j = binderVars.keySet().size()-argcounter-1;
               SemAtom bv = binderVars.get(j).get(binderVars.get(argcounter).size() - counter);
                SemAtom binder = binderVars.get(argcounter).get(binderVars.get(argcounter).size() - counter);


               Integer index = assumptionVars.keySet().size()-argcounter-1;


                //SemFunction newArg = new SemFunction(assumptionVars.get(argcounter).remove(assumptionVars.get(argcounter).size()-counter),binder,true);

            //    SemFunction newArg = new SemFunction(assumptionVars.get(index).remove(assumptionVars.get(index).size()-counter),bv,true);

                if (complex) {

                    SemFunction newArg = new SemFunction(assumptionVars.get(index).remove(assumptionVars.get(index).size()-counter),bv,true);

                    SemFunction newArg2 = new SemFunction(((SemFunction) dummySemantics).getBinder(),
                            new SemFunction(((SemFunction)((FuncApp) currentSemantics).getArgument()).getBinder(),((SemFunction) dummySemantics).getBinder()));
                       //     ((SemFunction)((FuncApp) currentSemantics).getArgument().get);
                    FuncApp newFA = new FuncApp(newArg2,newArg);
                    SemanticRepresentation newBody = ((FuncApp) currentSemantics).getFunctor();
                    currentSemantics = new FuncApp(newBody,newFA);


                    dummySemantics = new SemFunction(binder, ((SemFunction) dummySemantics).getFuncBody());
                 //   currentSemantics = new SemFunction(binder,currentSemantics);

               //     currentSemantics = new SemFunction(binder, currentSemantics);
                 //   currentSemantics = new FuncApp(currentSemantics, newArg);

                }
                else
                    {
                        SemFunction newArg = new SemFunction(assumptionVars.get(index).remove(assumptionVars.get(index).size()-counter),bv,true);
                        currentSemantics = new FuncApp(currentSemantics, newArg);
                        dummySemantics = new SemFunction(binder, dummySemantics);
                //        currentSemantics = new SemFunction(binder, currentSemantics);
                    }




                /*

                if ( !complex) {
                    currentSemantics = new SemFunction(binder, currentSemantics);
                    currentSemantics = new FuncApp(currentSemantics, newArg);

                } else
                {


                }
                */

                /*
                if (!complex) {
                    currentSemantics = new FuncApp(currentSemantics, newArg);
                    dummySemantics = new SemFunction(binder, dummySemantics);
                } else
                {
                    dummySemantics = new SemFunction(binder,new FuncApp(dummySemantics,newArg));
                    //dummySemantics = new FuncApp(dummySemantics,newArg);
                    // dummySemantics = new SemFunction(binder,dummySemantics);
                }
*/
                counter = counter - 1;

                dependency.setSemTerm((SemanticExpression) dummySemantics);
                //dependency.setSemTerm((SemanticExpression) currentSemantics);

                // dependency.setSemTerm(new SemFunction(bv, new FuncApp(dependency.getSemTerm(), newArg)));




                // Premise dependency = new Premise(p.getPremiseIDs(), p.getSemTerm(), returnNewLeftMostFormula(f));




                return dependency;
            }
            /*
            There might be cases like a -o ((b -o c) -o d) where reordering is necessary before
            the term can be compiled. In these cases it is only necessary if the LHS of the whole
            term is NOT a modifier type
            */
            else if (f.isNested() && !f.getRhs().isModifier()) {
 /*
                while (f.getLhs() instanceof LLAtom)
                {

                }

   */           counter = 0;
                argcounter = argcounter + 1;
                Premise temp = new Premise(p.getPremiseIDs(),p.getSemTerm(),new LLFormula((LLFormula) f.getRhs()));
                temp = convertNested(temp);
                LLTerm newGlue = new LLFormula(f.getLhs(),temp.getGlueTerm(),temp.getGlueTerm().isPolarity());
                newGlue.discharges.addAll(p.getGlueTerm().discharges);
                p = new Premise(p.getPremiseIDs(),temp.getSemTerm(),newGlue);

                argcounter = argcounter - 1;
                counter = binderVars.get(argcounter).size();

                //p = convertNested(reorder(p));
            }
            /*
            simple implication; create new lambda function which binds var (the variable
            associated with the assumption created in this method). Then add this new lambda
            term as argument to the current meaning term and wrap everything in a new lambda
            term binding the newly created variable.
            */


/*
            SemType newtype = new SemType(f.getLhs().getType());

            SemAtom binderVar = new SemAtom(VAR,LexVariableHandler.returnNewVar(SemVar),newtype);
            SemFunction newArg = new SemFunction(assumptionVars.removeLast(),binderVar);
            //((SemFunction) p.getSemTerm()).setArgument(newArg);


            //Original version:
             p.setSemTerm(new SemFunction(binderVar,new FuncApp(p.getSemTerm(),newArg)));
*/

            //Test new version of compiled semantic side

            /*
            SemanticExpression copy =  (SemanticExpression) p.getSemTerm().clone();


            if (copy.isCompiled()) {
                while (copy.isCompiled()) {
                    copy = (SemanticExpression)((SemFunction) copy).getFuncBody();
                }

                p.setSemTerm(new SemFunction(binderVar, new));

                ((SemanticExpression) p.getSemTerm()).setCompiled(true);


            }





             if (! (p.getSemTerm() instanceof MeaningRepresentation) && ((SemanticExpression) p.getSemTerm()).isCompiled()) {
                 changeSemanticBody((SemanticExpression) p.getSemTerm(), newArg);
                 p.setSemTerm(new SemFunction(binderVar, p.getSemTerm()));

                 ((SemanticExpression) p.getSemTerm()).setCompiled(true);
             }
             else
                 {
                     p.setSemTerm(new SemFunction(binderVar,new FuncApp(p.getSemTerm(),newArg)));
                     ((SemanticExpression) p.getSemTerm()).setCompiled(true);
                 }

*/
        //Test area end

            return p;
        }
        // only an atomic glue term which will become an assumption;
        // return it and add the new variable as meaning side
        else {
           // Integer index = assumptionVars.keySet().size()-argcounter-1;
            p.setSemTerm(assumptionVars.get(argcounter).getLast());
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
                sem = new SemFunction(sem.getBinder(),(SemanticExpression) inner.getSemTerm());

                return new Premise(p.getPremiseIDs(),sem,glue);
            }
        }
        // A -o b; no reordering required
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



    //Test area



    public SemanticRepresentation replaceDummy(SemanticRepresentation sem)
    {

        if (sem instanceof MeaningRepresentation && sem.toString().equals("dum_var"))
        {
            sem = currentSemantics;
            return sem;
        } else
        {
          if (sem instanceof SemFunction)
           {
                return new SemFunction(((SemFunction) sem).getBinder(),replaceDummy(((SemFunction) sem).getFuncBody()),true);
            }

            if (sem instanceof FuncApp)
            {
               return new FuncApp(replaceDummy(((FuncApp) sem).getFunctor()),((FuncApp) sem).getArgument());

          }

          return null;
        }
    }

}
