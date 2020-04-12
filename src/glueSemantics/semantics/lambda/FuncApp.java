/*
 * Copyright 2018 Mark-Matthias Zymla & Moritz Messmer
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

package glueSemantics.semantics.lambda;

import glueSemantics.semantics.FunctionalApplication;
import glueSemantics.semantics.MeaningRepresentation;
import glueSemantics.semantics.SemanticRepresentation;
import main.Settings;
import prover.LLProver2;
import prover.ProverException;
import utilities.LexVariableHandler;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class FuncApp extends SemanticExpression implements FunctionalApplication {

    private SemanticRepresentation functor;
    private SemanticRepresentation argument;

    public FuncApp(SemanticRepresentation functor, SemanticRepresentation argument) {
        //this.functor = functor;
        //this.argument = argument;

        //Version for pointwise functional application


        this.instantiateFunctionalApp(functor, argument);

    }
    public FuncApp(FuncApp fa) {
        this.functor = fa.functor.clone();
        this.argument = fa.argument.clone();

        //Test version
        //this.compiled = fa.compiled;
    }

    public SemanticRepresentation getFunctor() {
        return functor;
    }

    public SemanticRepresentation getArgument() {
        return argument;
    }



    // Does a full beta reduction of the term including all nested functional applications
    public SemanticRepresentation betaReduce() throws ProverException {
        alphaConversion();
        if (this.functor instanceof FuncApp)
            this.functor = functor.betaReduce();

        if (functor instanceof SemSet)
        {
            List<SemanticRepresentation> newSet = new ArrayList<>();
            for (SemanticRepresentation m : ((SemSet) functor).getMembers())
            {
                FuncApp newFA = new FuncApp(m,argument);
                newSet.add(newFA);
            }

            SemSet out = new SemSet(newSet,newSet.get(0).getType());
            return out.betaReduce();

        } else if (argument instanceof SemSet)
        {
            List<SemanticRepresentation> newSet = new ArrayList<>();
            for (SemanticRepresentation m : ((SemSet) argument).getMembers())
            {
                FuncApp newFA = new FuncApp(functor,m);
                newSet.add(newFA);
            }
            SemSet out = new SemSet(newSet,newSet.get(0).getType());
            return out.betaReduce();
        }


        if (argument != null)
            return apply(argument);
        return this;
    }

    /*
    Applies the functor to the argument. The functor must be a SemFunc or SemQuantEx and the
    applyTo() function of the functor is called for the actual application step.
    */
    public SemanticRepresentation apply(SemanticRepresentation arg) throws ProverException {

        if (arg instanceof FuncApp) {
            arg = arg.betaReduce();
            this.argument = arg;
        }

        if (this.functor instanceof SemFunction) {
            SemFunction lambda = (SemFunction) this.functor;

            //For end beta reduction

            if (lambda.getBinder().getType().equals(arg.getType())) {
                SemanticRepresentation newBody = lambda.getFuncBody();
                newBody = newBody.applyTo(lambda.getBinder(), arg);
                newBody = newBody.betaReduce();
          //      if (arg != this.argument)
         //           return new FuncApp(newBody, this.argument).betaReduce();
          //      else
                    if (newBody instanceof SemFunction)
                        return newBody;
                    else return  newBody.betaReduce();

            }
        }
        else if (this.functor instanceof SemQuantEx) {
            SemQuantEx quant = (SemQuantEx) this.functor;
            if (quant.getBinder().getType().equals(arg.getType())) {
                SemanticRepresentation newBody = quant.getQuantBody();
                newBody = newBody.applyTo(quant.getBinder(), arg);
                //newBody = newBody.betaReduce();
                if (arg != this.argument)
                    return new FuncApp(newBody, this.argument);
                    //return new FuncApp(newBody,arg);
                else
                    return newBody.betaReduce();
            }
        }
        else if (this.functor instanceof FuncApp) {
            //Old version did weird stuff for meaning representations
       //    return new FuncApp(this,arg);

            return this;
            //old version
            //TODO this line makes no sense at all, but is required to work.
          //  return ((FuncApp) this.functor).apply(arg);



          //  return this;
        }
        else if (this.functor instanceof MeaningRepresentation) {

            if (arg.equals(this.argument))
                return this;
            return new FuncApp(this,arg);

         //   return new MeaningRepresentation(String.format("app(%s,%s)",functor.toString(),arg.toString()));
        }
        return this;
    }


  /*
    public SemanticRepresentation checkBinder(SemanticRepresentation arg)
    {

        Set<String> possibleVariables = new HashSet<>();

        SemanticRepresentation functor = this;

        if (arg instanceof FuncApp)
        {
            while (functor instanceof FuncApp)
            {
                if (((FuncApp) functor).argument instanceof SemAtom)
                {
                    possibleVariables.add(SemAtom);
                }


                if (!(((FuncApp) functor).functor instanceof FuncApp))
                {
                    break;
                }

                functor = ((FuncApp) functor).functor;
            }


        } else if (arg instanceof SemPred)
        {

        }
    }
    */

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
         //   if (appliedFunc instanceof FuncApp)
         //       appliedFunc = appliedFunc.betaReduce();
            SemanticRepresentation appliedArg = this.argument.applyTo(var, arg);

            return new FuncApp(appliedFunc,appliedArg);
    }

    @Override
    public SemanticExpression clone() {
        return new FuncApp(this);
    }

    @Override
    public Set<SemAtom> findBoundVariables() {
        Set<SemAtom> out = new HashSet<>();
        out.addAll(functor.findBoundVariables());
        out.addAll(argument.findBoundVariables());
        return out;
    }

    @Override
    public SemType getType() {
        return functor.getType();
    }

    @Override
    public String toString() {
        if (LLProver2.getSettings().getSemanticOutputStyle() == Settings.PROLOG)
            return String.format("app(%s,%s)",functor.toString(),argument.toString());
        else
            return functor.toString() + "(" + argument.toString() + ")";

    }

    @Override
    public void instantiateFunctionalApp(SemanticRepresentation func, SemanticRepresentation arg) {
        this.functor = func;
        this.argument = arg;
        this.setType(func.getType());
    }


    public void setFunctor(SemanticRepresentation functor) {
        this.functor = functor;
    }

    public void alphaConversion()
    {
        Set<SemAtom> funcSet = functor.findBoundVariables();
        Set<SemAtom> argSet = argument.findBoundVariables();

        for (SemAtom var1 : funcSet)
        {
            for (SemAtom var2 : argSet)
            {
                if (!(var1 == var2) && var1.getName().equals(var2.getName()))
                {
                    var2.setName(LexVariableHandler.returnNewVar(LexVariableHandler.variableType.SemVarE));
                }
            }
        }


    }

}
