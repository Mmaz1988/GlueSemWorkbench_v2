package prover;

import glueSemantics.linearLogic.*;
import glueSemantics.semantics.SemanticRepresentation;
import glueSemantics.semantics.lambda.*;
import main.Settings;
import test.Debugging;
import utilities.LexVariableHandler;

import java.io.IOException;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LLProver2 {

    private static Settings settings;



    private Sequent currentSequent;

    //TODO Add a third chart for modifiers and both atomic elements as well as non atomic elements are first
    //run through the modifier chart.
    private HashMap<String,List<Premise>> atomicChart = new HashMap<>();
    private HashMap<String,List<Premise>> nonAtomicChart = new HashMap<>();
 //   private HashMap<String,List<Premise>> modifierChart = new HashMap<>();

    // A chart that associates variables that are compiled out with their original formula.
    // This is necessary to instantiate variables that are atmoic elements rather than variables that occur in formulas.




    private LinkedList<Premise> agenda;
    private LinkedList<Premise> solutions =new LinkedList<>();

    private HashMap<Premise,List<Premise>> variableDependency = new HashMap<>();

    private StringBuilder proofBuilder;


    //private LinkedList<Premise> skeletons = new LinkedList<>();
    //private LinkedList<Premise> modifiers = new LinkedList<>();

    private HashSet<Integer> goalIDs = new HashSet<>();

    public Debugging db;


    /**
     * LLProver version 2.0
     * Implements Lev's rather than Hepple's algorithm. Avoids need for accidental binding.
     * @param settings
     */
    public LLProver2(Settings settings) {
        setSettings(settings);
        this.proofBuilder = new StringBuilder();
    }

    public LLProver2(Settings settings, StringBuilder proofBuilder)
    {
        this.proofBuilder = proofBuilder;
        setSettings(settings);
    }

     public void deduce(Sequent seq) throws ProverException,VariableBindingException
        {
            LinkedList<Premise> agenda = new LinkedList<>();

            this.db = new Debugging();

            this.currentSequent = seq;

            long startTime = System.nanoTime();

            StringBuilder sb = new StringBuilder();
            sb.append("Sequent:");
            sb.append(System.lineSeparator());
            for (Premise le : currentSequent.getLhs())
            {
                sb.append(le);
                sb.append(System.lineSeparator());
            }
            System.out.println(sb.toString());

            //TODO insert boolean for distinguishing between sdout and file
            if (true)
            {
                proofBuilder.append(sb.toString());
                proofBuilder.append(System.lineSeparator());
                proofBuilder.append(System.lineSeparator());
            }

            for (Premise p : currentSequent.getLhs()) {
                    List<Premise> compiled = convert(p);
                    agenda.addAll(compiled);
            }

            StringBuilder ab = new StringBuilder();
            ab.append("Agenda:");
            ab.append(System.lineSeparator());
            for (Premise p : agenda)
            {
                ab.append(p);
                ab.append(System.lineSeparator());
            }
            System.out.println(ab.toString());

            //TODO insert boolean for distinguishing between sdout and file
            if (true)
            {
                proofBuilder.append(ab.toString());
                proofBuilder.append(System.lineSeparator());
                proofBuilder.append(System.lineSeparator());
            }

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

                            if (p.getGlueTerm().getType().equals(LLAtom.LLType.VAR))
                            {

                                for (String category : nonAtomicChart.keySet())
                                {

                                    for (Premise q : nonAtomicChart.get(category))
                                    {
                                        combined = combinePremises(p,q);
                                        if (combined != null) {
                                            db.combinations++;
                                            iter.add(combined);
                                        }
                                    }
                                }

                            } else {

                                if (nonAtomicChart.containsKey(p.getGlueTerm().category())) {
                                    for (Premise q : nonAtomicChart.get(p.getGlueTerm().category())) {
                                        combined = combinePremises(q, p);
                                        if (combined != null) {
                                            db.combinations++;
                                            iter.add(combined);
                                        }
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
                                if (atomicChart.containsKey(((LLFormula) p.getGlueTerm()).getLhs().category())) {
                                    for (Premise q : atomicChart.get(((LLFormula) p.getGlueTerm()).getLhs().category())) {
                                        combined = combinePremises(p, q);
                                        if (combined != null) {
                                            iter.add(combined);
                                            db.combinations++;
                                        }
                                    }
                                }

                                for (String category : atomicChart.keySet())
                                {
                                    if (Character.isUpperCase(category.charAt(0)))
                                    {
                                        for (Premise q : atomicChart.get(category)) {
                                            combined = combinePremises(p, q);
                                            if (combined != null) {
                                                iter.add(combined);
                                                db.combinations++;
                                            }
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

            proofBuilder.append(System.lineSeparator());

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

                    LinkedList<Premise> discharges =  ((LLFormula) func.getGlueTerm()).getLhs().getOrderedDischarges();

                    LLTerm argumentGlueClone = argument.getGlueTerm().clone();

                    while (!discharges.isEmpty())
                    {
                        Premise p = discharges.removeLast();
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

        //    System.out.println("Combining " + f + " and " + a);
        //    System.out.println("to: " + combined.toString());

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

/*
        List<Premise> funcList = ((LLFormula)functor.getGlueTerm()).getLhs().getOrderedDischarges();
        List<Premise> argList  = argument.getGlueTerm().getAssumptions2();

        if (argList.isEmpty())
        {
            return false;
        }

        for (int i = 0; i < ((LLFormula)functor.getGlueTerm()).getLhs().getOrderedDischarges().size(); i++)
        {
            if (!funcList.get(i).equals(argList.get(i)))
            {
                return false;
            }
        }

        return true;

*/


        for (Premise t : ((LLFormula) functor.getGlueTerm()).getLhs().getOrderedDischarges()) {
            if (!argument.getGlueTerm().assumptions2.contains(t)){
                return false;
        }
    }
        return true;

    }

    public void adjustChart(Premise p) {
        if (p.getGlueTerm() instanceof LLFormula) {


                if (nonAtomicChart.containsKey(((LLFormula) p.getGlueTerm()).getLhs().category())) {
                    nonAtomicChart.get(((LLFormula) p.getGlueTerm()).getLhs().category()).add(p);
                } else {
                    List<Premise> premises = new ArrayList<>();
                    premises.add(p);
                    nonAtomicChart.put(((LLFormula) p.getGlueTerm()).getLhs().category(), premises);
                }

        } else if (p.getGlueTerm() instanceof LLAtom) {
            if (atomicChart.containsKey(p.getGlueTerm().category())) {
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
                compiledGlue.getLhs().orderedDischarges.addAll(l.getOrderedDischarges());
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


                compiledGlue.getLhs().getOrderedDischarges().add(assumption);

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



    public boolean isVar(String in)
    {
        Pattern p = Pattern.compile("(\\p{Lu}+).*_.+");
        Matcher pm = p.matcher(in);

        return pm.find();

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

    public void updateVariableDependencies(List<Premise> compiled)
    {
        for (Premise premise : compiled)
        {
            if (premise.getGlueTerm() instanceof LLAtom &&
                    ((LLAtom) premise.getGlueTerm()).getLLtype().equals(LLAtom.LLType.VAR))
            {
                for (Premise q : compiled)
                {

                }

            }
        }
    }

    public StringBuilder getProofBuilder() {
        return proofBuilder;
    }

    public void setProofBuilder(StringBuilder proofBuilder) {
        this.proofBuilder = proofBuilder;
    }

}
