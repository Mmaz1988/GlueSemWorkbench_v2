package gluePaP.semantics;

public class FuncApp extends SemRepresentation{
    private SemRepresentation functor;
    private SemRepresentation argument;

    public FuncApp(SemRepresentation functor, SemRepresentation argument) {
        this.functor = functor;
        this.argument = argument;
    }

    public SemRepresentation getFunctor() {
        return functor;
    }

    public void setFunctor(SemRepresentation functor) {
        this.functor = functor;
    }

    public SemRepresentation getArgument() {
        return argument;
    }

    public void setArgument(SemRepresentation argument) {
        this.argument = argument;
    }

    public SemRepresentation betaReduce() {
        if (argument != null) {
            SemRepresentation reduced = apply(argument);
            argument = null;
            if (reduced instanceof SemFunction)
                return ((SemFunction) reduced).betaReduce();
            else
                return reduced;
        }
        return this;
    }

    public SemRepresentation apply(SemRepresentation arg) {
        if (this.functor instanceof SemFunction) {
            SemFunction lambda = (SemFunction) this.functor;
            if (lambda.getBinder().getType().equalsType(arg.getType())) {
                SemRepresentation newBody = lambda.getFuncBody();
                newBody.applyTo(lambda.getBinder(), arg);

                return newBody;
            }
        }
        return null;
    }

    // TODO is this enough?
    public SemRepresentation applyTo(SemAtom var,SemRepresentation arg) {
        return this.functor.applyTo(var, arg);
    }
}
