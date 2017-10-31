package gluePaP.linearLogic;

public class LLConstant implements Atom {
    private String name;
    private int id;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }


    public LLConstant(String name) {
        this.name = name;
        //this.id = id;
    }
}
