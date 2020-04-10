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
import prover.LLProver2;
import prover.ProverException;

import static glueSemantics.semantics.lambda.BinaryTerm.SemOperator.AND;
import static glueSemantics.semantics.lambda.BinaryTerm.SemOperator.IMP;
import static glueSemantics.semantics.lambda.SemType.AtomicType.T;
import static main.Settings.PROLOG;

public class BinaryTerm extends SemanticExpression {
    private SemanticRepresentation left;
    private SemanticRepresentation right;
    private final SemOperator operator;

    public BinaryTerm(SemanticRepresentation left, SemOperator operator, SemanticRepresentation right) {
        this.left = left;
        this.right = right;
        this.operator = operator;
    }

    public BinaryTerm(BinaryTerm b) {
        this.left = b.left;
        this.right = b.right;
        this.operator = b.operator;
    }

    public SemanticRepresentation getLeft() {
        return left;
    }

    public void setLeft(SemanticRepresentation left) {
        this.left = left;
    }

    public SemanticRepresentation getRight() {
        return right;
    }

    public void setRight(SemanticRepresentation right) {
        this.right = right;
    }

    public SemOperator getOperator() {
        return operator;
    }

    @Override
    public SemType getType() {
        return new SemType(T);
    }

    @Override
    public SemanticRepresentation betaReduce() throws ProverException {
        return new BinaryTerm(left.betaReduce(),operator,right.betaReduce());
    }

    @Override
    public SemanticRepresentation applyTo(SemanticRepresentation var, SemanticRepresentation arg) throws ProverException {
        return new BinaryTerm(left.applyTo(var,arg),operator,right.applyTo(var,arg));
    }

    @Override
    public SemanticExpression clone() {
        return new BinaryTerm(this);
    }

    public enum SemOperator {
        AND, OR, IMP;
    }

    @Override
    public String toString() {
        if (operator == AND)
            if(LLProver2.getSettings().getSemanticOutputStyle() == PROLOG)
                return String.format("and(%s,%s)",left.toString(),right.toString());
            else
                return left.toString() + " " + String.valueOf('\u2227') + " " + right.toString();
        else if (operator == IMP)
            if(LLProver2.getSettings().getSemanticOutputStyle() == PROLOG)
                return String.format("imp(%s,%s)",left.toString(),right.toString());
            else
                return left.toString() + " " + String.valueOf('\u2192') + " " + right.toString();
        else
        if(LLProver2.getSettings().getSemanticOutputStyle() == PROLOG)
            return String.format("or(%s,%s)",left.toString(),right.toString());
        else
            return left.toString() + " " + String.valueOf('\u2228') + " " + right.toString();
    }
}
