package gluePaP.linearLogic;

public class LLVariable extends LLTerm implements Atom{
    private String name;


    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }



    public LLVariable(String id, String name) {
        this.name = name;
        this.setTermId(id);
    }

    @Override
    public String toString() {
        return name + "_" + this.getTermId();
    }
}
