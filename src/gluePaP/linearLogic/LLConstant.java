package gluePaP.linearLogic;

public class LLConstant extends LLTerm implements Atom {
    private String name;
    //private int id;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }



    public LLConstant(String name, String id) {
        this.name = name;
        this.setId(id);
    }
}
