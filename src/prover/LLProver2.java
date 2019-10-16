package prover;

import glueSemantics.linearLogic.*;
import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.FuncApp;
import glueSemantics.semantics.lambda.SemAtom;
import glueSemantics.semantics.lambda.SemFunction;
import glueSemantics.semantics.lambda.SemType;
import glueSemantics.synInterface.dependency.LexVariableHandler;
import main.Settings;
import test.Debugging;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LLProver2 {

    private static Settings settings;



    private Sequent currentSequent;

    private HashMap<String,List<Premise>> atomicChart = new HashMap<>();
    private HashMap<String,List<Premise>> nonAtomicChart = new HashMap<>();

    private LinkedList<Premise> agenda;
    private LinkedList<Premise> solutions =new LinkedList<>();

    private HashSet<Integer> goalIDs = new HashSet<>();

    public Debugging db;


    /**
     * LLProver version 2.0
     * Implements Lev's rather than Hepple's algorithm. Avoids need for accidental binding.
     * @param settings
     */
    public LLProver2(Settings settings) {
        setSettings(settings);
    }


     public void deduce(Sequent seq) throws ProverException,VariableBindingException
        { LinkedList<Premise> agenda = new LinkedList<>();


            this.db = new Debugging();

            this.currentSequent = seq;

            long startTime = System.nanoTime();

            for (Premise p : currentSequent.getLhs()) {
                agenda.addAll(convert(p));
            }

            StringBuilder sb = new StringBuilder();
            sb.append("Agenda:");
            sb.append(System.lineSeparator());
            for (Premise p : agenda)
            {
                sb.append(p);
                sb.append(System.lineSeparator());
            }
            System.out.println(sb.toString());

            this.agenda = agenda;

            for (Premise p : this.agenda)
            {
                if (p.getPremiseIDs().size() == 1)
                {
                    goalIDs.addAll(p.getPremiseIDs());
                }
            }



            while (!agenda.isEmpty()) {
                ListIterator<Premise> iter = agenda.listIterator();

                Premise combined = null;

                while (iter.hasNext()) {

                    Premise p = iter.next();
                    iter.remove();
                    db.allIterations++;

                    if (p.getGlueTerm() instanceof LLAtom) {

                        if (nonAtomicChart.keySet().contains(p.getGlueTerm().category())) {
                            for (Premise q : nonAtomicChart.get(p.getGlueTerm().category())) {
                                combined = combinePremises(q, p);
                                if (combined != null) {
                                    db.combinations++;
                                    iter.add(combined);
                                }
                            }

                        }

                            for (String key : nonAtomicChart.keySet()) {
                                if (isVar(key)) {
                                    for (Premise q : nonAtomicChart.get(key)) {
                                        combined = combinePremises(q, p);
                                        if (combined != null) {
                                            db.combinations++;
                                            iter.add(combined);
                                        }
                                    }
                                }
                            }




                    } else if (p.getGlueTerm() instanceof LLFormula) {

                        if (((LLAtom) ((LLFormula) p.getGlueTerm()).getLhs()).getLLtype().equals(LLAtom.LLType.VAR)) {
                            for (String key : atomicChart.keySet()) {

                                for (Premise q : atomicChart.get(key)) {
                                    combined = combinePremises(p, q);
                                    if (combined != null) {
                                        iter.add(combined);
                                        db.combinations++;
                                    }
                                }
                            }
                        } else {
                            if (atomicChart.keySet().contains(((LLFormula) p.getGlueTerm()).getLhs().category())) {
                                for (Premise q : atomicChart.get(((LLFormula) p.getGlueTerm()).getLhs().category())) {
                                    combined = combinePremises(p, q);
                                    if (combined != null) {
                                        iter.add(combined);
                                        db.combinations++;
                                    }
                                }
                            }
                        }

                    }
                    updateSolutions(p);
                    adjustChart(p);
                }
            }

            long endTime = System.nanoTime();

            db.computationTime = endTime - startTime;


        }




    public Premise combinePremises(Premise functor, Premise argument) throws VariableBindingException, ProverException {

        Premise func = new Premise(functor.getPremiseIDs(), functor.getSemTerm().clone(), functor.getGlueTerm().clone());

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
        if (((LLFormula) func.getGlueTerm()).getLhs().checkEquivalence(argument.getGlueTerm())
                && Collections.disjoint(func.getPremiseIDs(), argument.getPremiseIDs())) {
            combined_IDs.addAll(func.getPremiseIDs());
            combined_IDs.addAll(argument.getPremiseIDs());


            if (((LLFormula) func.getGlueTerm()).getLhs().getOrderedDischarges().isEmpty()) {

                SemanticRepresentation reducedSem = combine(func,argument);

                combined = new Premise(combined_IDs, reducedSem, ((LLFormula) func.getGlueTerm()).getRhs());

        }
         else {
                if (checkDischarges(func, argument)) {

                    SemanticRepresentation temp = argument.getSemTerm().clone();

                    LinkedList<Premise> discharges =  ((LLFormula) func.getGlueTerm()).getLhs().getOrderedDischarges();

                    while (!discharges.isEmpty())
                    {
                       temp = new SemFunction((SemAtom) discharges.removeLast().getSemTerm(),temp);

                       }

                       Premise argumentClone = new Premise(argument.getPremiseIDs(),temp,argument.getGlueTerm().clone());

                    SemanticRepresentation reducedSem = combine(func,argumentClone);

                    combined = new Premise(combined_IDs, reducedSem, ((LLFormula) func.getGlueTerm()).getRhs());

                }

            }

            if (combined != null) {
                combined.getGlueTerm().assumptions2.addAll(func.getGlueTerm().assumptions2);
                combined.getGlueTerm().assumptions2.addAll(argument.getGlueTerm().assumptions2);
            }


        }


        return combined;

    }

    public static SemanticRepresentation combine(Premise func, Premise argument) throws ProverException
    {
        SemanticRepresentation reducedSem;
        if (getSettings().isBetaReduce()) {
            System.out.println("Beta reduced: " + func.getSemTerm().toString() + ", " + argument.getSemTerm().toString());
            reducedSem = new FuncApp(func.getSemTerm(), argument.getSemTerm()).betaReduce();
            System.out.println("To:" + reducedSem.toString());
        } else
            reducedSem = new FuncApp(func.getSemTerm(), argument.getSemTerm());

        return reducedSem;

    }

    public Boolean checkDischarges(Premise functor, Premise argument) {

        for (Premise t : ((LLFormula) functor.getGlueTerm()).getLhs().getOrderedDischarges()) {
            if (!argument.getGlueTerm().assumptions2.contains(t)){
                return false;
        }
    }
        return true;
    }

    public void adjustChart(Premise p) {
        if (p.getGlueTerm() instanceof LLFormula) {


                if (nonAtomicChart.keySet().contains(((LLFormula) p.getGlueTerm()).getLhs().category())) {
                    nonAtomicChart.get(((LLFormula) p.getGlueTerm()).getLhs().category()).add(p);
                } else {
                    List<Premise> premises = new ArrayList<>();
                    premises.add(p);
                    nonAtomicChart.put(((LLFormula) p.getGlueTerm()).getLhs().category(), premises);
                }

        } else if (p.getGlueTerm() instanceof LLAtom) {
            if (atomicChart.keySet().contains(p.getGlueTerm().category())) {
                atomicChart.get(p.getGlueTerm().category()).add(p);
            } else {
                List<Premise> premises = new ArrayList<>();
                premises.add(p);
                atomicChart.put(p.getGlueTerm().category(), premises);

            }
        }
    }

    public LinkedList<Premise> convert(Premise p)
    {
        LinkedList<Premise> compiled = new LinkedList<>();

        if (p.getGlueTerm() instanceof LLFormula)
        {

            LLFormula f = (LLFormula) p.getGlueTerm();
            LLTerm l = ((LLFormula) p.getGlueTerm()).getLhs();


            if (l instanceof LLFormula)
            {
                db.compilations++;
                //Compile out stuff
                LLFormula compiledGlue = new LLFormula( ((LLFormula) l).getRhs(),f.getRhs(),f.isPolarity(),f.getVariable());
                compiledGlue.getLhs().orderedDischarges.addAll(l.getOrderedDischarges());
                LLTerm outGlue = ((LLFormula) l).getLhs();


                //outGlue.assumptions.add(outGlue);
              //  compiledGlue.getLhs().getOrderedDischarges().add(outGlue);

                SemType newtype = new SemType(((LLFormula) l).getLhs().getType());
                SemAtom asumptionVar = new SemAtom(SemAtom.SemSort.VAR,
                        LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE),newtype);


                Premise assumption = new Premise(currentSequent.getNewID(),asumptionVar,outGlue);


                ((LLFormula) compiledGlue).getLhs().getOrderedDischarges().add(assumption);

                Premise compiledPremise = new Premise(p.getPremiseIDs(),p.getSemTerm(),compiledGlue);




                assumption.getGlueTerm().assumptions2.add(assumption);

              //  compiled.add(compiledPremise);
                // compiled.add(assumption);



                List<Premise> recurseCompiled = convert(compiledPremise);
                List<Premise> recurseAssumption = convert(assumption);

                compiled.addAll(recurseCompiled);
                compiled.addAll(recurseAssumption);

                return compiled;


            } else if (f.isNested() && !f.isModifier())
            {
                Premise temp = new Premise(p.getPremiseIDs(),p.getSemTerm(),f.getRhs());
                LinkedList<Premise> tempList = convert(temp);

                for (int i = 1; i < tempList.size(); i++)
                {
                    compiled.add(tempList.get(i));
                }

                LLFormula newLogic =new LLFormula(f.getLhs(),tempList.getFirst().getGlueTerm(),
                        tempList.getFirst().getGlueTerm().isPolarity());

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



    public boolean isVar(String in)
    {
        Pattern p = Pattern.compile("(\\p{Lu}+).*_.+");
        Matcher pm = p.matcher(in);

        if (pm.find())
        {
            return true;
        }
        else
        {
            return false;
        }

    }


    public void updateSolutions(Premise p)
    {
        if (p.getPremiseIDs().equals(goalIDs))
        {
            solutions.add(p);
        }
    }

    //Getter and Setter
    public LinkedList<Premise> getSolutions() {
        return solutions;
    }

    public void setSolutions(LinkedList<Premise> solutions) {
        this.solutions = solutions;
    }
}
