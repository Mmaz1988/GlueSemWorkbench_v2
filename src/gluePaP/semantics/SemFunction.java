package gluePaP.semantics;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import static gluePaP.semantics.SemAtom.SemSort.VAR;

public class SemFunction extends SemRepresentation {

    // Operator: lambda, quantifiers. What about connectives?
    private final char operator = '\u03BB';
    private SemAtom binder;
    // Is this necessary or do we only have one list on the SemPred?
    //private List<SemAtom> boundVars;
    // The body of the function, a nested SemFunction or Predicate
    private SemRepresentation funcBody;
    // Optional field that is used when doing lambda application and possibly when
    // doing glue derivations in general
    private SemRepresentation argument;


/*    public SemAtom getArg(int i) {
        return funcBody.getArg(i);
    }*/

    public SemFunction(SemAtom binder, SemRepresentation funcBody) {
        this.binder = binder;
        this.funcBody = funcBody;
        this.setType(new SemType(binder.getType(),funcBody.getType()));

    }

    public SemFunction(SemAtom binder, SemRepresentation funcBody, SemRepresentation argument) {
        this.binder = binder;
        this.funcBody = funcBody;
        this.argument = argument;
        this.setType(new SemType(binder.getType(),funcBody.getType()));
    }

/*    @Override
    public SemType getType() {
        return new SemType(binder.getType(),funcBody.getType());
    }*/

    public SemRepresentation getFuncBody() {
        return funcBody;
    }

    public void setFuncBody(SemRepresentation funcBody) {
        this.funcBody = funcBody;
    }

    public SemRepresentation getArgument() {
        return argument;
    }

    public void setArgument(SemRepresentation argument) {
        this.argument = argument;
    }

    @Override
    public String toString() {
        if (argument != null) {
            return operator + binder.toString() + "." +
                    funcBody.toString() + "(" + argument.toString() + ")";
        }
            else {
            return operator + binder.toString() + "." + funcBody.toString();
        }
    }

    // TODO for variable instantiation we could probably recycle the mechanism from the
    // glue side, including the Equality class.

    public SemRepresentation betaReduce() {
        if (argument != null) {
            SemRepresentation reduced = apply(argument);
            if (reduced instanceof SemFunction)
                return ((SemFunction) reduced).betaReduce();
            else
                return reduced;
        }
        return this;
    }

    public SemRepresentation apply(SemRepresentation arg) {
        if (this.binder.getType().equalsType(arg.getType())) {
            SemRepresentation newBody = this.getFuncBody();
            newBody.applyTo(binder, arg);

            return newBody;
        }
        return null;
    }

    public boolean applyTo(SemAtom var,SemRepresentation arg) {
        return (this.funcBody.applyTo(var,arg) || this.argument.applyTo(var,arg));
    }



}
