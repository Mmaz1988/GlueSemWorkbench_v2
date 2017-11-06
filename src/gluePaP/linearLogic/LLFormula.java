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

    public LLTerm instantiateVar(LLVariable var, LLTerm quTerm, LLTerm instTerm) {
        if (instTerm instanceof LLConstant) {
            if (quTerm instanceof LLConstant && quTerm.checkEquivalence(instTerm))
                return instTerm;
            // TODO Does the equals() check work?
            else if (quTerm instanceof LLVariable && quTerm.equals(var))
                return instTerm;
            else if (quTerm instanceof LLFormula && instTerm instanceof  LLFormula){
                LLTerm newLeft = instantiateVar(var, ((LLFormula) quTerm).getLhs(),((LLFormula) instTerm).getLhs());
                LLOperator op = this.operator;
                LLTerm newRight = instantiateVar(var,((LLFormula) quTerm).getRhs(),((LLFormula) instTerm).getRhs());
                return new LLFormula(quTerm.getTermId(),newLeft,op,newRight,quTerm.isPolarity());

            }
        }

        // Something didn't work out abort and return null
        return null;

    }





}
