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

    public SemAtom getBinder() {
        return binder;
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
            argument = null;
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

    public SemRepresentation applyTo(SemAtom var,SemRepresentation arg) {
        SemRepresentation applied = null;
        if (this.argument != null) {
            applied = this.argument.applyTo(var, arg);
        }
        if (applied == null) {
            /* NOTE:
            * We only want to get rid of the lambda binder if it is
            * actually the variable that is being replaced. In Hepple-style
            * deductions we get situations like the following:
            * functor: \v.u     arg: sleep(v)
            * where the u is bound somewhere else and we replace it with the arg whilst
            * keeping the \v slot.
            * */
            if (this.binder == var)
                return this.funcBody.applyTo(var, arg);
            else {
                this.setFuncBody(funcBody.applyTo(var, arg));
                return this;
            }

        } else {
            this.argument = applied;
            return this;
        }
    }



}
