package gluePaP.semantics;

import static gluePaP.semantics.SemAtom.SemSort.VAR;

public class FuncApp extends SemRepresentation{
    private final SemRepresentation functor;
    private final SemRepresentation argument;

    public FuncApp(SemRepresentation functor, SemRepresentation argument) {
        this.functor = functor;
        this.argument = argument;
    }

    public FuncApp(FuncApp fa) {
        this.functor = fa.functor.clone();
        this.argument = fa.argument.clone();
    }

    public SemRepresentation getFunctor() {
        return functor;
    }

    public SemRepresentation getArgument() {
        return argument;
    }



    // Does a full beta reduction of the term including all nested functional applications
    public SemRepresentation betaReduce() {
        if (argument != null)
            return apply(argument);
        return this;
    }

    // Applies the functor to the argument. The functor must be a SemFunc and the
    // applyTo() function of the functor is called for the actual application step.
    public SemRepresentation apply(SemRepresentation arg) {
        if (this.functor instanceof SemFunction) {
            SemFunction lambda = (SemFunction) this.functor;
            if (lambda.getBinder().getType().equalsType(arg.getType())) {
                SemRepresentation newBody = lambda.getFuncBody();
                newBody = newBody.applyTo(lambda.getBinder(), arg);
                newBody = newBody.betaReduce();

                return newBody;
            }
        }
        return this;
    }

    // TODO is this enough?
    // This method is only called when this object is the body of a SemFunc
    // which amounts to two cases ( [] = functor, () = argument):
    // a) [LP. Lx. P(x)]  (Ly.predicate(y))
    // b) [Lu. LP. P  (Lv.u)] (predicate(v)))
    // In the first case we want to substitute P for the argument and then betaReduce.
    // In the second case we want to apply the argument of this FuncApp to arg.
    // Both cases can be done sequentially as an unsuccessful application attempt
    // simply returns the unsimplified expression.
    public SemRepresentation applyTo(SemAtom var,SemRepresentation arg) {
            SemRepresentation appliedFunc = this.functor.applyTo(var, arg);
            if (appliedFunc instanceof FuncApp)
                appliedFunc = ((FuncApp) appliedFunc).betaReduce();
            SemRepresentation appliedArg = this.argument.applyTo(var, arg);

            return new FuncApp(appliedFunc,appliedArg);
    }

    @Override
    public SemRepresentation clone() {
        return new FuncApp(this);
    }

    @Override
    public SemType getType() {
        return functor.getType();
    }

    @Override
    public String toString() {
        return functor + "(" + argument + ")";
    }
}
