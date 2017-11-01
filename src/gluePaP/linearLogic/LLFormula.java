package gluePaP.linearLogic;

public class LLFormula extends LLTerm {
    // Complex LLterms
    private String id;
    private String name;
    private LLTerm lhs;
    private LLTerm rhs;
    private LLOperator operator;


    public LLFormula(String id, String name, LLTerm lhs, LLTerm rhs, LLOperator operator) {
        this.id = id;
        this.name = name;
        this.lhs = lhs;
        this.rhs = rhs;
        this.operator = operator;
    }
}
