package gluePaP.semantics;


import static gluePaP.semantics.SemType.AtomicType.E;
import static gluePaP.semantics.SemType.AtomicType.T;

public abstract class SemRepresentation {
    private SemType type;

    public SemRepresentation() {
    }

    public SemType getType() {
        return type;
    }

    public void setType(SemType.AtomicType type) {
        this.type = new SemType(type);
    }

    public void setType(SemType type) {
        this.type = type;
    }

    public abstract SemRepresentation betaReduce();

    public abstract SemRepresentation applyTo(SemAtom var, SemRepresentation arg);

    public abstract SemRepresentation clone();



}
