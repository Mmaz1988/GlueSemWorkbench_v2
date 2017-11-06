package gluePaP.linearLogic;

public class LLVariable extends LLTerm implements Atom{
    private String name;
    private Type type;


    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }



    public LLVariable(String id, String name, boolean pol) {
        this.name = name;
        this.setTermId(id);
        this.setPolarity(pol);
    }

    @Override
    public String toString() {
        return name + "_" + this.getTermId();
    }

    // TODO implement equivalence check for LL variables
    public boolean checkEquivalence(LLTerm term) {
        return false;
    }

    @Override
    public Type getType() {
        return this.type;
    }
}
