package gluePaP.linearLogic;

public class LLVariable extends LLTerm {
    private String name;



    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }



    public LLVariable(String id, String name, Type type, boolean pol) {
        this.name = name;
        this.setTermId(id);
         this.setType(type);
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
                    && this.getType().equals(((LLVariable) term).getType())){
                return true;
            }
        }
        return false;
    }

    @Override
    public Type getType() {
        return this.getType();
    }
}
