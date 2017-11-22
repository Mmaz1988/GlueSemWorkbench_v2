package gluePaP.semantics;

import static gluePaP.semantics.SemType.AtomicType.T;

public class SemQuantEx extends SemRepresentation{
    private final SemQuant quantifier;
    private final SemAtom binder;
    private final SemRepresentation quantBody;

    public SemQuantEx(SemQuant quantifier, SemAtom binder, SemRepresentation quantBody) {
        this.quantifier = quantifier;
        this.binder = binder;
        this.quantBody = quantBody;
    }

    public SemQuantEx(SemQuantEx s) {
        this.quantifier = s.quantifier;
        this.binder = s.binder;
        this.quantBody = s.quantBody.clone();
    }

    public enum SemQuant {
        UNI,EX
    }


    @Override
    public SemType getType() {
        return new SemType(T);
    }

    @Override
    public SemRepresentation betaReduce() {
        return quantBody.betaReduce();
    }

    @Override
    public SemRepresentation applyTo(SemAtom var, SemRepresentation arg) {
        return quantBody.applyTo(var,arg);
    }

    @Override
    public SemRepresentation clone() {
        return new SemQuantEx(this);
    }
}
