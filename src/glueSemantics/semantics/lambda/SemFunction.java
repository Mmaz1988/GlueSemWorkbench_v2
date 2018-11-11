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

import glueSemantics.semantics.FunctionalAbstraction;
import glueSemantics.semantics.FunctionalApplication;
import glueSemantics.semantics.SemanticRepresentation;
import prover.LLProver;
import prover.ProverException;

import static main.Settings.PROLOG;

public class SemFunction extends SemanticExpression implements FunctionalAbstraction {

    // Operator: lambda, quantifiers. What about connectives?
    private final char operator = '\u03BB';
    private SemAtom binder;
    // Is this necessary or do we only have one list on the SemPred?
    //private List<SemAtom> boundVars;
    // The body of the function, a nested SemFunction or Predicate
    private SemanticRepresentation funcBody;
    // Optional field that is used when doing lambda application and possibly when
    // doing glue derivations in general


    public SemFunction(SemAtom binder, SemanticRepresentation funcBody) {
        //this.binder = binder;
        //this.funcBody = funcBody;
        this.instantiateFunctionalAbstraction(binder,funcBody);
        this.setType(new SemType(binder.getType(),funcBody.getType()));
    }

    public SemFunction(SemFunction f) {
        this.binder = f.binder;
        this.funcBody = f.funcBody.clone();
        this.setType(f.getType());
    }

    @Override
    public void instantiateFunctionalAbstraction(SemAtom binder, SemanticRepresentation body) {
        this.binder = binder;
        this.funcBody = body;
    }

    public SemanticRepresentation getFuncBody() {
        return funcBody;
    }

    public SemAtom getBinder() {
        return binder;
    }

    @Override
    public String toString() {
        if(LLProver.getSettings().getSemanticOutputStyle() == PROLOG)
            return String.format("lam(%s,%s)",binder.toString(),funcBody.toString());
        else
            return operator + binder.toStringTyped() + "." + funcBody.toString();
    }


    @Override
    public SemanticRepresentation betaReduce() throws ProverException {
        return new SemFunction(this.binder,funcBody.betaReduce());
    }

    public SemanticRepresentation applyTo(SemanticRepresentation var, SemanticRepresentation arg) throws ProverException {
        return new SemFunction(this.binder,this.funcBody.applyTo(var, arg));
    }

    @Override
    public SemanticExpression clone() {
        return new SemFunction(this);
    }

}
