package gluePaP.semantics;

import static gluePaP.semantics.BinaryTerm.SemOperator.AND;
import static gluePaP.semantics.BinaryTerm.SemOperator.IMP;
import static gluePaP.semantics.SemType.AtomicType.T;

public class BinaryTerm extends SemRepresentation{
    private SemRepresentation left;
    private SemRepresentation right;
    private final SemOperator operator;

    public BinaryTerm(SemRepresentation left, SemOperator operator, SemRepresentation right) {
        this.left = left;
        this.right = right;
        this.operator = operator;
    }

    public BinaryTerm(BinaryTerm b) {
        this.left = b.left;
        this.right = b.right;
        this.operator = b.operator;
    }

    public SemRepresentation getLeft() {
        return left;
    }

    public void setLeft(SemRepresentation left) {
        this.left = left;
    }

    public SemRepresentation getRight() {
        return right;
    }

    public void setRight(SemRepresentation right) {
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
    public SemRepresentation betaReduce() {
        return new BinaryTerm(left.betaReduce(),operator,right.betaReduce());
    }

    @Override
    public SemRepresentation applyTo(SemAtom var, SemRepresentation arg) {
        return new BinaryTerm(left.applyTo(var,arg),operator,right.applyTo(var,arg));
    }

    @Override
    public SemRepresentation clone() {
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
