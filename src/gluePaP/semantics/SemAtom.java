package gluePaP.semantics;

public class SemAtom extends Identifier {
    private String name;
    //private String value;
    private SemType type;
    private SemSort sort;



    public SemAtom(SemSort sort, String name, SemType type)
    {
        this.name = name;
        this.sort = sort;
        this.type = type;
    }


    public enum SemType {
        E,T,COMPLEX
    }

    public enum SemSort {
        VAR, CONST
    }

    public SemType getType() {
        return type;
    }

    public void setType(SemType type) {
        this.type = type;
    }


    //Getter and Setter methods

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name + "_" + type;
    }
}
