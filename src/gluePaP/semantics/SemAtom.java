package gluePaP.semantics;



public class SemAtom extends SemRepresentation {
    private String name;
    //private String value;
    //private AtomicType atomicType;
    private SemSort sort;


    // Constructor for atomic type atom
    public SemAtom(SemSort sort, String name, SemType.AtomicType type)
    {
        this.name = name;
        this.sort = sort;
        this.setType(type);
    }

    // Constructor for higher type atom
    public SemAtom(SemSort sort, String name, SemType type)
    {
        this.name = name;
        this.sort = sort;
        this.setType(type);
    }


    public enum SemSort {
        VAR, CONST
    }


    // If this method is called, something must have gone wrong, return false.
    @Override
    public boolean applyTo(SemAtom var, SemRepresentation arg) {
        return false;
    }

    public String getName() {
        return name;
    }


    @Override
    public String toString() {
        return name + "_" + getType();
    }

}
