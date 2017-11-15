package gluePaP.semantics;


import static gluePaP.semantics.SemType.AtomicType.E;
import static gluePaP.semantics.SemType.AtomicType.T;

public abstract class SemRepresentation {

    private String name;
    private SemType type;

    //public final SemType ET = new SemType(E,T);

    //public abstract SemAtom getArg(int i);



    public SemType getType() {
        return type;
    }

    public void setType(SemType.AtomicType type) {
        this.type = new SemType(type);
    }

    public void setType(SemType type) {
        this.type = type;
    }

    public abstract boolean applyTo(SemAtom var, SemRepresentation arg);

    enum SemOperator {

    }

    //public abstract SemRepresentation betaReduce();


}
