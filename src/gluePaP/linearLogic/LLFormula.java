package gluePaP.linearLogic;

public class LLFormula extends LLTerm {
    // Complex LLterms
    private String id;
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

    public void setLhs(LLTerm lhs) {
        this.lhs = lhs;
    }

    public LLTerm getRhs() {
        return rhs;
    }

    public void setRhs(LLTerm rhs) {
        this.rhs = rhs;
    }


    public LLFormula(String id, LLTerm lhs, LLOperator operator,LLTerm rhs) {
        this.lhs = lhs;
        this.rhs = rhs;
        this.operator = operator;
        this.setId(id);
        this.name = this.toString();
    }

    @Override
    public String toString() {
        return "(" + lhs + " " + operator + " "  + rhs + ")_"+ this.getId();
    }
}
