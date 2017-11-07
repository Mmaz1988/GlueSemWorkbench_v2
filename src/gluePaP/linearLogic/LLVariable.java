package gluePaP.linearLogic;

public class LLVariable extends LLTerm implements LLAtom {
    private String name;
    private Type type;


    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }



    public LLVariable(String id, String name, Type type, boolean pol) {
        this.name = name;
        this.setTermId(id);
        this.type = type;
        this.setPolarity(pol);
    }

    @Override
    public String toString() {
        return name + "_" + this.getTermId();
    }

    // TODO implement equivalence check for LL variables
    public boolean checkEquivalence(LLTerm term) {
        if (term instanceof LLVariable) {
            if (this.name.equals(((LLVariable) term).name)
                    && this.type.equals(((LLVariable) term).type)){
                return true;
            }
        }
        return false;
    }

    @Override
    public Type getType() {
        return this.type;
    }
}
