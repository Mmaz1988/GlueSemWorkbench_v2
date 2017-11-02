package gluePaP.linearLogic;

public class Premise {
    private String premiseId;
    private LLTerm llterm;

    public Premise(String premiseId, LLTerm llterm) {
        this.premiseId = premiseId;
        this.llterm = llterm;
    }

    @Override
    public String toString() {
        return llterm + "[" + premiseId + "]";
    }
}
