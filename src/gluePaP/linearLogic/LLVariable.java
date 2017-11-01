package gluePaP.linearLogic;

public class LLVariable extends LinearLogicTerm implements Atom{
    private String name;


    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }



    public LLVariable(String name) {
        this.name = name;
        //this.id = id;
    }
}
