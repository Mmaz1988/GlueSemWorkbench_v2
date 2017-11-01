package gluePaP.linearLogic;

public class LLVariable implements Atom{
    private String name;
    //private int id;

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
