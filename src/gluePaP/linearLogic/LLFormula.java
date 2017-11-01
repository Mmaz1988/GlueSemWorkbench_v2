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

    public void setName(String name) {
        this.name = name;
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

}
