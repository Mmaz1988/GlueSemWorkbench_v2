package gluePaP.linearLogic;

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
        return "(" + lhs + " " + operator + " "  + rhs + ")_"+ this.getTermId();
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

    /*
    * Recursively goes through a LL formula and instantiates all occurrences of the variable var
    * to an appropriately typed LL constant. Returns a LL formula like the input quTerm, but
    * with all variable occurrences instantiated. If the structures of the two input
    * formulas don't match it returns null instead.
    */
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


    public static LLTerm instantiateVar(LLVariable var, LLTerm quTerm, LLTerm instTerm) {
        if (quTerm instanceof LLConstant && quTerm.checkEquivalence(instTerm))
            return instTerm;
        else if (instTerm instanceof LLConstant && quTerm instanceof LLVariable && quTerm.checkEquivalence(var))
            return instTerm;
        else if (instTerm instanceof LLFormula && quTerm instanceof  LLFormula){
            LLTerm newLeft = instantiateVar(var, ((LLFormula) quTerm).getLhs(),((LLFormula) instTerm).getLhs());
            LLOperator op = ((LLFormula) quTerm).operator;
            LLTerm newRight = instantiateVar(var,((LLFormula) quTerm).getRhs(),((LLFormula) instTerm).getRhs());
            return new LLFormula(instTerm.getTermId(),newLeft,op,newRight,quTerm.isPolarity());
        }

        // Something didn't work out, abort and return null
        return null;

    }





}
