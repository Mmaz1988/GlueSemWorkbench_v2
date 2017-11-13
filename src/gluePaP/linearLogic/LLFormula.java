package gluePaP.linearLogic;

import Prover.Equality;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LLFormula extends LLTerm {
    private String name;
    private LLTerm lhs;
    private LLTerm rhs;
    private LLOperator operator;

    private LLAtom variable;

    private HashMap<LLAtom,List<LLAtom>> boundVariables = new HashMap<>();


    public String getName() {
        return name;
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


    //Constructor for formulas without variables

    public LLFormula(String id, LLTerm lhs, LLOperator operator,LLTerm rhs, boolean pol) {
        this.lhs = lhs;
        this.rhs = rhs;
        this.setPolarity(pol);
        this.operator = operator;
        this.setTermId(id);
        this.name = this.toString();
    }



    //Constructor for formulas with variables
    public LLFormula(String id, LLTerm lhs, LLOperator operator, LLTerm rhs, boolean pol,
                     LLAtom var) {
        this.lhs = lhs;
        this.rhs = rhs;
        this.setPolarity(pol);
        this.operator = operator;
        this.setTermId(id);
        this.name = this.toString();
        this.variable = var;

        List<LLAtom> bvl = Stream.concat(findBoundOccurrences(lhs).stream(),
                findBoundOccurrences(rhs).stream()).collect(Collectors.toList());


        this.boundVariables.put(variable,bvl);

    }

    public LLFormula(LLFormula term) {
        this.assumptions = new HashSet<>(term.assumptions);
        this.setDischarge(term.getDischarge());
        this.name = term.getName();
        this.setTermId(term.getTermId());
        this.setType(term.getType());
        this.setPolarity(term.isPolarity());
        this.lhs = term.getLhs();
        this.rhs = term.getRhs();
    }


        /* ##############################
    Modified LLFormula
    ################################
     */

    // represents the binder relation between the quantifier and the variables in the scope of the quantifier

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


    /* ##############################
    Original LLFormula
    ################################
     */


    @Override
    // TODO properly represent formulas that have assumptions associated with them are not themselves assumptions.
    public String toString() {
        String as = "";
        String dc = "";
        if (!(this.assumptions.isEmpty()))
        // return "{(" + lhs.toPlainString() + " " + operator + " "  + rhs.toPlainString() + ")_"+ this.getTermId() + "}";
        {
            as = "{" + assumptions.toString() + "}";
        }

        if ((this.getDischarge() != null))
        // return "{(" + lhs.toPlainString() + " " + operator + " "  + rhs.toPlainString() + ")_"+ this.getTermId() + "}";
        {
            dc = "[" + this.getDischarge().toString() + "]";
        }

   //     } else {
     //   }
        return "(" + lhs.toPlainString() + " "
                + dc + " "
                + operator + " " + rhs.toPlainString() + ")_" + this.getTermId()
                + " " + as;
    }

    public String toPlainString() {
        return "(" + lhs.toPlainString() + " " + operator + " "  + rhs.toPlainString() + ")_"+ this.getTermId();
    }


    @Override
    public boolean checkEquivalence(LLTerm term) {
        if (term instanceof LLFormula) {
            if (lhs.checkEquivalence(((LLFormula) term).lhs)
                    && rhs.checkEquivalence(((LLFormula) term).rhs)
                    && ((LLTerm) this.operator).checkEquivalence(((LLTerm) ((LLFormula) term).operator))) {
                return true;
            }
        }
        return false;
    }

    @Override
    public LinkedHashSet<Equality> checkCompatibility(LLTerm term) {
        if (term instanceof LLFormula){
            if (this.lhs.checkCompatibility(((LLFormula) term).lhs) != null &&
                    this.rhs.checkCompatibility(((LLFormula) term).rhs) != null &&
                    ((LLTerm) this.operator).checkCompatibility(((LLTerm) ((LLFormula) term).operator)) != null)
                    {
                LinkedHashSet<Equality> left = this.lhs.checkCompatibility(((LLFormula) term).lhs);
                LinkedHashSet<Equality> right = this.rhs.checkCompatibility(((LLFormula) term).rhs);
                LinkedHashSet<Equality> operator = ((LLTerm) this.operator).checkCompatibility(((LLTerm) ((LLFormula) term).operator));


                LinkedHashSet<Equality> dummy = Stream.concat(right.stream(), left.stream()).collect(Collectors.toCollection(LinkedHashSet::new));

                return Stream.concat(dummy.stream(), operator.stream()).collect(Collectors.toCollection(LinkedHashSet::new));
            }
        }

        /* reverse arguments so that you don't have to write a new case for this but can just use the override
        LLQuant*/

        return null;
    }

    /*
    * Recursively goes through a LL formula and instantiates all occurrences of the variable var
    * to an appropriately typed LL constant. Returns a LL formula like the input quTerm, but
    * with all variable occurrences instantiated. If the structures of the two input
    * formulas don't match it returns null instead.
    */

    /*
    public static LLConstant getInstantiation(LLVariable var, LLTerm quTerm, LLTerm instTerm){
        if (instTerm instanceof LLConstant && quTerm instanceof LLVariable && quTerm.checkEquivalence(var))
            return (LLConstant) instTerm;
        else if (instTerm instanceof LLFormula && quTerm instanceof LLFormula){
            if(getInstantiation(var,((LLFormula) quTerm).getLhs(),((LLFormula) instTerm).getLhs()) != null)
                return getInstantiation(var,((LLFormula) quTerm).getLhs(),((LLFormula) instTerm).getLhs());
            if(getInstantiation(var,((LLFormula) quTerm).getRhs(),((LLFormula) instTerm).getRhs()) != null)
                return getInstantiation(var,((LLFormula) quTerm).getRhs(),((LLFormula) instTerm).getRhs());
        }
        return null;
    }

    public void instantiate(LLVariable var, LLConstant constant){

    }


    public static LLTerm instantiateVar(LLAtom var, LLTerm quTerm, LLTerm instTerm) {
        if (quTerm instanceof LLAtom && quTerm.checkEquivalence(instTerm))
            return instTerm;
        else if (instTerm instanceof LLConstant && quTerm instanceof LLVariable && quTerm.checkEquivalence(var))
            return instTerm;
        else if (instTerm instanceof LLFormula && quTerm instanceof LLFormula) {
            LLTerm newLeft = instantiateVar(var, ((LLFormula) quTerm).getLhs(), ((LLFormula) instTerm).getLhs());
            LLOperator op = ((LLFormula) quTerm).operator;
            LLTerm newRight = instantiateVar(var, ((LLFormula) quTerm).getRhs(), ((LLFormula) instTerm).getRhs());
            return new LLFormula(instTerm.getTermId(), newLeft, op, newRight, quTerm.isPolarity());
        }

        // Something didn't work out, abort and return null
        return null;

    }
*/

    public boolean isNested(){
        return this.getLhs() instanceof LLFormula;
    }



}
