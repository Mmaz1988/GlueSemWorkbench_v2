/*
 * Copyright 2018 Moritz Messmer and Mark-Matthias Zymla.
 * This file is part of the Glue Semantics Workbench
 * The Glue Semantics Workbench is free software and distributed under the conditions of the GNU General Public License,
 * without any warranty.
 * You should have received a copy of the GNU General Public License along with the source code.
 * If not, please visit http://www.gnu.org/licenses/ for more information.
 */

package glueSemantics.semantics.lambda;

import glueSemantics.semantics.SemanticRepresentation;
import prover.ProverException;

import static glueSemantics.semantics.lambda.BinaryTerm.SemOperator.AND;
import static glueSemantics.semantics.lambda.BinaryTerm.SemOperator.IMP;
import static glueSemantics.semantics.lambda.SemType.AtomicType.T;

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
            return left + " " + String.valueOf('\u2227') + " " + right;
        else if (operator == IMP)
            return left + " " + String.valueOf('\u2192') + " " + right;
        else
            return left + " " + String.valueOf('\u2228') + " " + right;
    }
}
