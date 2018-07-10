/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.semantics;

public class SemFunction extends SemRepresentation {

    // Operator: lambda, quantifiers. What about connectives?
    private final char operator = '\u03BB';
    private final SemAtom binder;
    // Is this necessary or do we only have one list on the SemPred?
    //private List<SemAtom> boundVars;
    // The body of the function, a nested SemFunction or Predicate
    private final SemRepresentation funcBody;
    // Optional field that is used when doing lambda application and possibly when
    // doing glue derivations in general


    public SemFunction(SemAtom binder, SemRepresentation funcBody) {
        this.binder = binder;
        this.funcBody = funcBody;
        this.setType(new SemType(binder.getType(),funcBody.getType()));

    }

    public SemFunction(SemFunction f) {
        this.binder = f.binder;
        this.funcBody = f.funcBody.clone();
        this.setType(f.getType());
    }

    public SemRepresentation getFuncBody() {
        return funcBody;
    }

    public SemAtom getBinder() {
        return binder;
    }

    @Override
    public String toString() {
            return operator + binder.toStringTyped() + "." + funcBody.toString();
    }


    @Override
    public SemRepresentation betaReduce() {
        return new SemFunction(this.binder,funcBody.betaReduce());
    }

    public SemRepresentation applyTo(SemAtom var, SemRepresentation arg) {
        return new SemFunction(this.binder,this.funcBody.applyTo(var, arg));
    }

    @Override
    public SemRepresentation clone() {
        return new SemFunction(this);
    }

}
