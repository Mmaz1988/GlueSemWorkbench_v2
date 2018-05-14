package gluePaP.linearLogic;

import Prover.Equality;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;


import static gluePaP.linearLogic.LLFormula.LLOperator.LLIMP;

public class LLFormula extends LLTerm {
    private String name;
    private LLTerm lhs;
    private LLTerm rhs;
    private LLOperator operator;

    private LLAtom variable;

    private HashMap<LLAtom,List<LLAtom>> boundVariables = new HashMap<>();

    public enum LLOperator{
        LLIMP
    }


    public LLTerm getLhs() {
        return lhs;
    }

    public LLTerm getRhs() {
        return rhs;
    }


    public LLAtom getVariable() {
        return variable;
    }


    public LLOperator getOperator() { return operator; }

    //Constructor for formulas without variables and with a linear implication as operator
    public LLFormula(LLTerm lhs, LLTerm rhs, boolean pol) {
        this.lhs = lhs;
        this.rhs = rhs;
        this.setPolarity(pol);
        this.operator = LLIMP;
        this.name = this.toString();
    }

    public LLFormula(LLTerm lhs, LLTerm rhs, boolean pol, LLAtom var) {
        this.lhs = lhs;
        this.rhs = rhs;
        this.setPolarity(pol);
        this.operator = LLIMP;
        this.name = this.toString();
        this.variable = var;

        if(variable!= null) {
            List<LLAtom> bvl = Stream.concat(findBoundOccurrences(lhs).stream(),
                    findBoundOccurrences(rhs).stream()).collect(Collectors.toList());
            this.boundVariables.put(variable, bvl);
        }
    }


    //Constructor for formulas without variables
    public LLFormula(LLTerm lhs, LLOperator operator,LLTerm rhs, boolean pol) {
        this.lhs = lhs;
        this.rhs = rhs;
        this.setPolarity(pol);
        this.operator = operator;
        this.name = this.toString();
    }



    //Constructor for formulas with variables
    public LLFormula(LLTerm lhs, LLOperator operator, LLTerm rhs, boolean pol,
                     LLAtom var) {
        this.lhs = lhs;
        this.rhs = rhs;
        this.setPolarity(pol);
        this.operator = operator;
        this.name = this.toString();
        this.variable = var;

        if (variable != null) {
            List<LLAtom> bvl = new ArrayList<>();
            bvl.addAll(findBoundOccurrences(lhs));
            bvl.addAll(findBoundOccurrences(rhs));
                    //Stream.concat(findBoundOccurrences(lhs).stream(),findBoundOccurrences(rhs).stream()).collect(Collectors.toList());
            this.boundVariables.put(variable, bvl);
        }

    }

        public LLFormula(LLFormula f)
        {
            this.assumptions = new HashSet<>(f.assumptions);
            this.discharges = new HashSet<>(f.discharges);
            this.lhs = f.getLhs().clone();
            this.rhs = f.getRhs().clone();
            this.setPolarity(f.isPolarity());
            this.operator = f.getOperator();
            this.name = f.toString();
            if (f.variable != null) {
            this.variable = new LLAtom(f.variable);
                List<LLAtom> bvl = Stream.concat(findBoundOccurrences(lhs).stream(),
                        findBoundOccurrences(rhs).stream()).collect(Collectors.toList());
                this.boundVariables.put(variable, bvl);
            }

        }




    /*
    Represents the binder relation between the quantifier
    and the variables in the scope of the quantifier
    */
    public List<LLAtom> findBoundOccurrences(LLTerm term){


        // Variables that are equivalent are bound by the quantifier
        if (term instanceof LLAtom) {
            if (((LLAtom) term).getLLtype() == LLAtom.LLType.VAR){

                if (this.variable.checkEquivalence(term))
                {
                    List <LLAtom> var = new ArrayList<>();
                    var.add((LLAtom) term);
                    return var;
                }
            }

            //Recursive call to find embedded instances of variables
        } else if ( term instanceof LLFormula)
        {
            List <LLAtom> right = findBoundOccurrences(((LLFormula) term).getLhs());
            List <LLAtom> left = findBoundOccurrences(((LLFormula) term).getRhs());

            return Stream.concat(right.stream(), left.stream()).collect(Collectors.toList());
        }
        List<LLAtom> emptyList = Collections.emptyList();
        return emptyList;
    }



    //Checks to which quantifier a specific variable belongs
    public boolean isInstance(LLAtom var){

        Set keys = boundVariables.keySet();
        Iterator it = keys.iterator();

        while (it.hasNext()){
            Object o = it.next();
            if (boundVariables.get(o).contains(var))
            {
                return true;
            }
        }
        return false;
    }


    //Variables carry over properties of corresponding constants
    public LLTerm instantiateVariables(Equality eq)
    {
        if (this.isInstance(eq.getVariable()))
        {
            for (LLAtom var : boundVariables.get(this.variable))
            {
                var.setName(eq.getConstant().getName());
                var.setLLtype(LLAtom.LLType.CONST);
            }
            return this;
        }
        return this;
    }


    @Override
    // TODO properly represent formulas that have assumptions associated with them are not themselves assumptions.
    public String toString() {
        String as = "";
        String dc = "";
        if (!(this.assumptions.isEmpty())) {
            if (this.assumptions.size() == 1 && this.assumptions.contains(this))
                return "{(" + lhs.toPlainString() + " " + "\u22B8" + " "  + rhs.toPlainString() + ")" + "}";
            else
                as = this.printAssumptions();
        }

        if (!this.discharges.isEmpty())
            dc = this.discharges.toString();

        return "(" + lhs.toPlainString()
                + dc + " "
                + "\u22B8" + " " + rhs.toPlainString() + ")"
                + as;
    }

    public String toPlainString() {
        return "(" + lhs.toPlainString() + " " + "\u22B8" + " "  + rhs.toPlainString() + ")";
    }

    @Override
    public LLTerm clone() {
        return new LLFormula(this   );
    }


    @Override
    public boolean checkEquivalence(LLTerm term) {
        if (term instanceof LLFormula) {
            if (lhs.checkEquivalence(((LLFormula) term).lhs)
                    && rhs.checkEquivalence(((LLFormula) term).rhs)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public LinkedHashSet<Equality> checkCompatibility(LLTerm term) {
        if (term instanceof LLFormula){
            if (this.lhs.checkCompatibility(((LLFormula) term).lhs) != null &&
                    this.rhs.checkCompatibility(((LLFormula) term).rhs) != null)
                    {
                LinkedHashSet<Equality> left = this.lhs.checkCompatibility(((LLFormula) term).lhs);
                LinkedHashSet<Equality> right = this.rhs.checkCompatibility(((LLFormula) term).rhs);


                //LinkedHashSet<Equality> dummy = Stream.concat(right.stream(), left.stream()).collect(Collectors.toCollection(LinkedHashSet::new));
                LinkedHashSet<Equality> dummy = new LinkedHashSet<>();
                dummy.addAll(right);
                dummy.addAll(left);
                return dummy;
                //return Stream.concat(dummy.stream(), Stream.of(operator)).collect(Collectors.toCollection(LinkedHashSet::new));
            }
        }

        return null;
    }


    public boolean isNested(){
        return this.getLhs() instanceof LLFormula;
    }




    public void setVarbinding(LLFormula formula) {
        this.variable = new LLAtom(formula.variable);

            List<LLAtom> bvl = Stream.concat(findBoundOccurrences(lhs).stream(),
                    findBoundOccurrences(rhs).stream()).collect(Collectors.toList());
            this.boundVariables.put(variable, bvl);


    }
}
