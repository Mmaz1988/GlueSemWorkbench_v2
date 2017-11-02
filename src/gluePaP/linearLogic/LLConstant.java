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



    public LLConstant(String name) {
        this.name = name;
        this.setId("0");
    }

    @Override
    public String toString() {
        return "LLConstant{" +
                "name='" + name + '\'' +
                "} " + super.toString();
    }
}
