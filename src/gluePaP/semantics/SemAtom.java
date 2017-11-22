package gluePaP.semantics;



public class SemAtom extends SemRepresentation {
    private final String name;
    //private String value;
    //private AtomicType atomicType;
    private final SemSort sort;


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

    public SemAtom(SemAtom a) {
        this.name = a.name;
        this.sort = a.sort;
        this.setType(a.getType());
    }


    public enum SemSort {
        VAR, CONST
    }


    @Override
    public SemRepresentation betaReduce() {
        return this;
    }

    // TODO check this again, does var need to be equal to this object?
    @Override
    public SemRepresentation applyTo(SemAtom var, SemRepresentation arg) {
        if (this == var)
            return arg;
        else
            return this;
    }

    @Override
    public SemRepresentation clone() {
        return new SemAtom(this);
    }

    public String getName() {
        return name;
    }

    public SemSort getSort() {
        return sort;
    }

    @Override
    public String toString() {
        return name + "_" + getType();
    }

}
