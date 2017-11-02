package gluePaP.linearLogic;

public class LLVariable extends LLTerm implements Atom{
    private String name;


    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }



    public LLVariable(String name, String id) {
        this.name = name;
        this.setId(id);
    }
}
