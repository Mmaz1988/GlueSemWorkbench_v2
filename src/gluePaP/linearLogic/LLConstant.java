package gluePaP.linearLogic;

public class LLConstant extends LLTerm implements Atom {
    private String name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }



    public LLConstant(String id, String name) {
        this.name = name;
        this.setTermId(id);
    }

    @Override
    public String toString() {
        return name + "_" + this.getTermId();
    }

    @Override
    public boolean checkEquivalence(LLTerm term) {
        if (term instanceof LLConstant) {
            if (this.name.equals(((LLConstant) term).name)){
                return true;
            }
        }
        return false;

    }
}
