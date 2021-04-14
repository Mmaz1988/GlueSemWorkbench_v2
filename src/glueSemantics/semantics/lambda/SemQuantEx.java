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

import glueSemantics.parser.SemanticParser;
import glueSemantics.semantics.SemanticRepresentation;
import prover.ProverException;

import java.util.HashSet;
import java.util.Set;

import static glueSemantics.semantics.lambda.SemQuantEx.SemQuant.EX;
import static glueSemantics.semantics.lambda.SemQuantEx.SemQuant.UNI;
import static main.Settings.PROLOG;

public class SemQuantEx extends SemanticExpression {
    private final SemQuant quantifier;
    private final SemAtom binder;
    private final SemanticRepresentation quantBody;

    public SemQuantEx(SemQuant quantifier, SemAtom binder, SemanticRepresentation quantBody, SemType type) {
        this.quantifier = quantifier;
        this.binder = binder;
        this.quantBody = quantBody;
        this.setType(type);
    }

    private SemQuantEx(SemQuantEx s) {
        this.quantifier = s.quantifier;
        this.binder = s.binder;
        this.setType(s.getType());
        this.quantBody = s.quantBody.clone();
    }

    public enum SemQuant {
        UNI,EX,DEF
    }

    public SemAtom getBinder() {
        return binder;
    }

    public SemanticRepresentation getQuantBody() {
        return quantBody;
    }


    @Override
    public SemanticRepresentation betaReduce() throws ProverException {
        return new SemQuantEx(quantifier, binder, quantBody.betaReduce(),getType());
    }

    @Override
    public SemanticRepresentation applyTo(SemanticRepresentation var, SemanticRepresentation arg) throws ProverException {
        return new SemQuantEx(quantifier, binder, quantBody.applyTo(var,arg),getType());
    }

    @Override
    public SemanticExpression clone() {
        return new SemQuantEx(this);
    }

    @Override
    public Set<SemAtom> findBoundVariables() {
        Set<SemAtom> out = new HashSet<>();
        out.add(binder);
        out.addAll(quantBody.findBoundVariables());
        return out;
    }

    @Override
    public String toString() {
        if (quantifier == UNI) {
            if (SemanticParser.settings.getSemanticOutputStyle() == PROLOG)
                return String.format("all(%s,%s)", binder.toString(), quantBody.toString());
            else
                return '\u2200' + binder.toString() + "[" + quantBody.toString() + "]";
        }
        else if (quantifier == EX) {
            if (SemanticParser.settings.getSemanticOutputStyle() == PROLOG)
                return String.format("some(%s,%s)", binder.toString(), quantBody.toString());
            else
                return '\u2203' + binder.toString() + "[" + quantBody.toString() + "]";
        }
        else
        {
            if(SemanticParser.settings.getSemanticOutputStyle() == PROLOG)
                return String.format("the(%s,%s)",binder.toString(),quantBody.toString());
            else
                return '\u03B9' + binder.toString() + "[" + quantBody.toString() + "]";
        }
    }
}
