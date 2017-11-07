package gluePaP.linearLogic;

public class LLUniversalQuant extends LLTerm{
    private final String symbol = "\u2200";
    private LLVariable variable;
    private LLFormula term;

    public LLVariable getVariable() {
        return variable;
    }

    public LLFormula getTerm() {
        return term;
    }

    public LLUniversalQuant(LLVariable variable, LLFormula term) {
        this.variable = variable;
        this.term = term;
    }

    public String toString() {

        return symbol + variable + "." + term;
    }

    @Override
    // TODO is this required?
    public boolean checkEquivalence(LLTerm term) {
/*        if (term instanceof LLUniversalQuant)
            return true;
        return false;*/
        return true;
    }
}
