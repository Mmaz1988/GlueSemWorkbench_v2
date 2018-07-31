/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.semantics.lambda;

import glueSemantics.semantics.FunctionalApplication;
import glueSemantics.semantics.MeaningRepresentation;
import glueSemantics.semantics.SemanticRepresentation;
import prover.ProverException;

public class FuncApp extends SemanticExpression implements FunctionalApplication {
    private SemanticRepresentation functor;
    private SemanticRepresentation argument;

    public FuncApp(SemanticRepresentation functor, SemanticRepresentation argument) {
        //this.functor = functor;
        //this.argument = argument;
        this.instantiateFunctionalApp(functor,argument);
    }

    public FuncApp(FuncApp fa) {
        this.functor = fa.functor.clone();
        this.argument = fa.argument.clone();
    }

    public SemanticRepresentation getFunctor() {
        return functor;
    }

    public SemanticRepresentation getArgument() {
        return argument;
    }



    // Does a full beta reduction of the term including all nested functional applications
    public SemanticRepresentation betaReduce() throws ProverException {
        if (argument != null)
            return apply(argument);
        return this;
    }

    /*
    Applies the functor to the argument. The functor must be a SemFunc or SemQuantEx and the
    applyTo() function of the functor is called for the actual application step.
    */
    public SemanticRepresentation apply(SemanticRepresentation arg) throws ProverException {
        if (this.functor instanceof SemFunction) {
            SemFunction lambda = (SemFunction) this.functor;
            if (lambda.getBinder().getType().equalsType(arg.getType())) {
                SemanticRepresentation newBody = lambda.getFuncBody();
                newBody = newBody.applyTo(lambda.getBinder(), arg);
                //newBody = newBody.betaReduce();
                if (arg != this.argument)
                    return new FuncApp(newBody, this.argument).betaReduce();
                else
                    return newBody.betaReduce();
            }
        }
        else if (this.functor instanceof SemQuantEx) {
            SemQuantEx quant = (SemQuantEx) this.functor;
            if (quant.getBinder().getType().equalsType(arg.getType())) {
                SemanticRepresentation newBody = quant.getQuantBody();
                newBody = newBody.applyTo(quant.getBinder(), arg);
                //newBody = newBody.betaReduce();
                if (arg != this.argument)
                    return new FuncApp(newBody, this.argument);
                else
                    return newBody.betaReduce();
            }
        }
        else if (this.functor instanceof FuncApp) {
            return ((FuncApp) this.functor).apply(arg);
        }
        else if (this.functor instanceof MeaningRepresentation) {
            if (arg.equals(this.argument))
                return this;
            return new FuncApp(this,arg);
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
    Both cases can be done sequentially, as an unsuccessful application attempt
    simply returns the unsimplified expression.
    */
    public SemanticRepresentation applyTo(SemanticRepresentation var, SemanticRepresentation arg) throws ProverException {
            SemanticRepresentation appliedFunc = this.functor.applyTo(var, arg);
            if (appliedFunc instanceof FuncApp)
                appliedFunc = appliedFunc.betaReduce();
            SemanticRepresentation appliedArg = this.argument.applyTo(var, arg);

            return new FuncApp(appliedFunc,appliedArg);
    }

    @Override
    public SemanticExpression clone() {
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

    @Override
    public void instantiateFunctionalApp(SemanticRepresentation func, SemanticRepresentation arg) {
        this.functor = func;
        this.argument = arg;
    }
}
