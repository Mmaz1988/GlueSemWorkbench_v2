package gluePaP.linearLogic;

public class LLFormula extends LLTerm {
    // Complex LLterms
    private String id;
    private String name;
    private LLTerm lhs;
    private LLTerm rhs;
    private LLOperator operator;


    public LLFormula(String id) {
        this.setId(id);
    }

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

    public LLFormula(String id, String name, LLTerm lhs, LLTerm rhs, LLOperator operator) {
        this.id = id;
        this.name = name;
        this.lhs = lhs;
        this.rhs = rhs;
        this.operator = operator;
    }

    public LLFormula(LLTerm lhs, LLOperator operator,LLTerm rhs) {
        this.id = "0";
        this.lhs = lhs;
        this.rhs = rhs;
        this.operator = operator;
        //this.name = this.toString();
    }

    @Override
    public String toString() {
        return "(" + lhs  + operator + rhs + ")";
    }
}
