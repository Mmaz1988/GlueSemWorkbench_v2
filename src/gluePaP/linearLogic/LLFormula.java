package gluePaP.linearLogic;

import Prover.Equality;

import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class LLFormula extends LLTerm {
    private String name;
    private LLTerm lhs;
    private LLTerm rhs;
    private LLOperator operator;


    public String getName() {
        return name;
    }

    public LLTerm getLhs() {
        return lhs;
    }

    public LLTerm getRhs() {
        return rhs;
    }

    public LLOperator getOperator() { return operator; }


    public LLFormula(String id, LLTerm lhs, LLOperator operator,LLTerm rhs, boolean pol) {
        this.lhs = lhs;
        this.rhs = rhs;
        this.setPolarity(pol);
        this.operator = operator;
        this.setTermId(id);
        this.name = this.toString();
    }

    @Override
    public String toString() {
        if (!this.assumptions.isEmpty())
            return "{(" + lhs.toPlainString() + " " + operator + " "  + rhs.toPlainString() + ")_"+ this.getTermId() + "}";
        else
            return "(" + lhs.toPlainString() + " " + operator + " "  + rhs.toPlainString() + ")_"+ this.getTermId();
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
    public List<Equality> checkCompatibility(LLTerm term) {
        if (term instanceof LLFormula){
            if (this.lhs.checkCompatibility(((LLFormula) term).lhs) != null &&
                    this.rhs.checkCompatibility(((LLFormula) term).rhs) != null &&
                    ((LLTerm) this.operator).checkCompatibility(((LLTerm) ((LLFormula) term).operator)) != null)
                    {
                List<Equality> left = this.lhs.checkCompatibility(((LLFormula) term).lhs);
                List<Equality> right = this.rhs.checkCompatibility(((LLFormula) term).rhs);
                List<Equality> operator = ((LLTerm) this.operator).checkCompatibility(((LLTerm) ((LLFormula) term).operator));


                List<Equality> dummy = Stream.concat(right.stream(), left.stream()).collect(Collectors.toList());

                return Stream.concat(dummy.stream(), operator.stream()).collect(Collectors.toList());
            }
        }

        /* reverse arguments so that you don't have to write a new case for this but can just use the override
        LLQuant*/
        if (term instanceof LLUniversalQuant){
            return term.checkCompatibility(this);
        }

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
