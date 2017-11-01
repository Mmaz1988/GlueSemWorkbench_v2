package gluePaP.linearLogic;

public class LLConstant extends LinearLogicTerm implements Atom {
    private String name;
    //private int id;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }



    public LLConstant(String name) {
        this.name = name;
        //this.id = id;
    }
}
