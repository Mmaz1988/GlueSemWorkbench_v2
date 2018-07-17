/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.semantics.lambda;

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

    /*
    Applies the functor to the argument. The functor must be a SemFunc or SemQuantEx and the
    applyTo() function of the functor is called for the actual application step.
    */
    public SemRepresentation apply(SemRepresentation arg) {
        if (this.functor instanceof SemFunction) {
            SemFunction lambda = (SemFunction) this.functor;
            if (lambda.getBinder().getType().equalsType(arg.getType())) {
                SemRepresentation newBody = lambda.getFuncBody();
                newBody = newBody.applyTo(lambda.getBinder(), arg);
                //newBody = newBody.betaReduce();
                if (arg != this.argument)
                    return new FuncApp(newBody,this.argument).betaReduce();
                else
                    return newBody.betaReduce();
            }
        }
        else if (this.functor instanceof SemQuantEx) {
            SemQuantEx quant = (SemQuantEx) this.functor;
            if (quant.getBinder().getType().equalsType(arg.getType())) {
                SemRepresentation newBody = quant.getQuantBody();
                newBody = newBody.applyTo(quant.getBinder(), arg);
                //newBody = newBody.betaReduce();
                if (arg != this.argument)
                    return new FuncApp(newBody,this.argument);
                else
                    return newBody.betaReduce();
            }
        }
        else if (this.functor instanceof FuncApp) {
            return ((FuncApp) this.functor).apply(arg);
        }
        return this;
    }

    /*
    This method is only called when this object is the body of a SemFunc
    which amounts to two cases ( [] = functor, () = argument):
    a) [LP. Lx. P(x)]  (Ly.predicate(y))
    b) [Lu. LP. P  (Lv.u)] (predicate(v)))
    In the first case we want to substitute P for the argument and then betaReduce.
    In the second case we want to apply the argument of this FuncApp to arg.
    Both cases can be done sequentially as an unsuccessful application attempt
    simply returns the unsimplified expression.
    */
    public SemRepresentation applyTo(SemAtom var,SemRepresentation arg) {
            SemRepresentation appliedFunc = this.functor.applyTo(var, arg);
            if (appliedFunc instanceof FuncApp)
                appliedFunc = appliedFunc.betaReduce();
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
