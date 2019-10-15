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

import glueSemantics.semantics.SemanticRepresentation;
import prover.LLProver;
import prover.LLProver2;
import prover.ProverException;

import static glueSemantics.semantics.lambda.SemQuantEx.SemQuant.UNI;
import static glueSemantics.semantics.lambda.SemType.AtomicType.T;
import static main.Settings.PROLOG;

public class SemQuantEx extends SemanticExpression {
    private final SemQuant quantifier;
    private final SemAtom binder;
    private final SemanticRepresentation quantBody;

    public SemQuantEx(SemQuant quantifier, SemAtom binder, SemanticRepresentation quantBody) {
        this.quantifier = quantifier;
        this.binder = binder;
        this.quantBody = quantBody;
    }

    private SemQuantEx(SemQuantEx s) {
        this.quantifier = s.quantifier;
        this.binder = s.binder;
        this.quantBody = s.quantBody.clone();
        this.setCompiled(s.isCompiled());
    }

    public enum SemQuant {
        UNI,EX
    }

    public SemAtom getBinder() {
        return binder;
    }

    public SemanticRepresentation getQuantBody() {
        return quantBody;
    }

    @Override
    public SemType getType() {
        return new SemType(T);
    }

    @Override
    public SemanticRepresentation betaReduce() throws ProverException {
        return new SemQuantEx(quantifier, binder, quantBody.betaReduce());
    }

    @Override
    public SemanticRepresentation applyTo(SemanticRepresentation var, SemanticRepresentation arg) throws ProverException {
        return new SemQuantEx(quantifier, binder, quantBody.applyTo(var,arg));
    }

    @Override
    public SemanticExpression clone() {
        return new SemQuantEx(this);
    }

    @Override
    public String toString() {
        if (quantifier == UNI)

            if(LLProver2.getSettings().getSemanticOutputStyle() == PROLOG)
                return String.format("all(%s,%s)",binder.toString(),quantBody.toString());
            else
                return String.valueOf('\u2200') + binder + "[" + quantBody + "]";
        else
        if(LLProver2.getSettings().getSemanticOutputStyle() == PROLOG)
            return String.format("some(%s,%s)",binder.toString(),quantBody.toString());
        else
            return String.valueOf('\u2203') + binder + "[" + quantBody + "]";
    }
}
