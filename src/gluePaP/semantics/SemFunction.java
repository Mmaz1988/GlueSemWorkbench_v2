package gluePaP.semantics;

import java.util.List;

public class SemFunction extends SemRepresentation {

    // Operator: lambda, quantifiers. What about connectives?
    private final char operator = '\u03BB';
    private SemAtom binder;
    // Is this necessary or do we only have one list on the SemPred?
    private List<SemVariable> boundVars;
    // The body of the function, a nested SemFunction or Predicate
    private SemRepresentation funcBody;
    // Optional field that is used when doing lambda application and possibly when
    // doing glue derivations in general
    private SemRepresentation argument;


    public SemAtom getArg(int i) {
        return funcBody.getArg(i);
    }

    public SemFunction(SemAtom binder, SemRepresentation funcBody) {
        this.binder = binder;
        this.funcBody = funcBody;
    }

    public SemFunction(SemAtom binder, SemRepresentation funcBody, SemRepresentation argument) {
        this.binder = binder;
        this.funcBody = funcBody;
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

        }
        return null;
    }




}
