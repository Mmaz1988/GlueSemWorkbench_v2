package gluePaP.semantics;


import static gluePaP.semantics.SemType.AtomicType.E;
import static gluePaP.semantics.SemType.AtomicType.T;

public class SemType {
    private SemType left;
    private SemType right;
    private AtomicType simple;

    public enum AtomicType {
        E,T
    }

    public SemType getLeft() {
        return left;
    }

    public void setLeft(SemType left) {
        this.left = left;
    }

    public SemType getRight() {
        return right;
    }

    public void setRight(SemType right) {
        this.right = right;
    }

    public SemType(AtomicType simple) {
        this.simple = simple;
        this.left = null;
        this.right = null;
    }

    public SemType(SemType left, SemType right) {
        this.left = left;
        this.right = right;
    }

    public SemType(AtomicType left, AtomicType right) {
        this.left = new SemType(left);
        this.right = new SemType(right);
    }

    @Override
    public String toString() {
        if (left != null)
            return "<" + left.toString() + "," + right.toString() + ">";
        else {
            if (simple == E)
                return "e";
            else
                return "t";
        }
    }

    public boolean equalsType(SemType t) {
        if (this.simple != null && t.simple != null)
            return this.simple.equals(t.simple);
        else
            return (this.left.equalsType(t.left) && this.right.equalsType(t.right));
    }
}